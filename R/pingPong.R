#' Ping-pong table generator
#'
#' Takes a slow view and materializes it into an intermediate table and "final" fast view to use in queries
#'
#' @param originView A list in the form list(schema = "schemaName", name = "viewName")
#' @param intermediateTablePrefix A list with the desired intermediate table prefix in the form list(schema = "desintationSchema", name = "intermediateTableName") (defaults to list(schema = racas::applicationSettings$server.database.username, name = originView$name)
#' @param destinationViewName A list with the desired final view name in the form list(schema = "destinationSchema", name = "destinationViewName", tableSpace, options) 
#' @param primaryKey Optional character string of origin view column in which to create primary key
#' @param indexes Optional character vector of column names in which to create indexes
#' @param applicationSettings Optional applicationSettings dataframe (defaults to racas::applicationSettings)
#' @return Returns a list of the created database objects dropped or created
#' @keywords ping-pong, pingpong, pingPong
#' @export
#' @examples
#' pingPong(
#'   originView =  list(schema = "ACAS", name = "API_ALL_DATA"),
#'   intermediateTablePrefix = list(schema = racas::applicationSettings$server.database.username, name = "API_ALL_DATA", tableSpace = "KALYPSYSADMIN_NOLOG", options = c("NOLOGGING")),
#'   destinationViewName = list(schema = "acas", name = "PP_API_ALL_DATA"),
#'   #indexes = lapply(list("AGV_ID", "AG_ID", "AG_PUBLIC_DATA", "AG_TESTED_LOT", "CV_ID", "C_ID", "C_PUBLIC_DATA", "C_STATE_ID", "PROJECT", "PROTOCOL_NAME", "SV_ID", "S_ID", "S_PUBLIC_DATA", "S_STATE_ID", "TGV_ID", "TG_ID", "TG_PUBLIC_DATA", "TG_STATE_ID", "TG_TESTED_LOT"), function(x) list(name = x, tableSpace = "KALYPSYSADMIN_NOLOG", options = "NOLOGGING", "compute statistics"))
#'  indexes = lapply(list("AGV_ID"), function(x) list(name = x, tableSpace = "KALYPSYSADMIN_NOLOG", options = "NOLOGGING", "compute statistics"))
#'  
pingPong <- function(originView, intermediateTablePrefix = list(schema = racas::applicationSettings$server.database.username, name = originView$name, tableSpace = NA, options = c()), destinationViewName, primaryKey = NULL, indexes = NULL, applicationSettings = racas::applicationSettings) {
  logger <- createLogger(logName = "com.mcneilco.racas.pingpong.apiviews")
  options(scipen=99)
  error_ping_pong_generator <- FALSE
  conn <- getDatabaseConnection(applicationSettings)
  on.exit(cat(dbDisconnect(conn)))
  if (dbExistsTable(conn, name = paste0(intermediateTablePrefix$name,"_a"), schema = intermediateTablePrefix$schema)){
    pingPongTableNew <- 'b'
    pingPongTableOld <- 'a'
    logger$debug(paste0("New ",intermediateTablePrefix$name," table is ", pingPongTableNew))
    logger$debug(paste0("Old ",intermediateTablePrefix$name," table is ", pingPongTableOld))
  } else {
    pingPongTableNew <- 'a'
    pingPongTableOld <- 'b'
    logger$debug(paste0("New ",intermediateTablePrefix$name," table is ", pingPongTableNew))
    logger$debug(paste0("Old ",intermediateTablePrefix$name," table is ", pingPongTableOld))
  }
  
  #Check to see if both tables are preset A and B
  if (dbExistsTable(conn, name = paste0(intermediateTablePrefix$name,"_a"), schema = intermediateTablePrefix$schema) & dbExistsTable(conn, name = paste0(intermediateTablePrefix$name,"_b"), schema = intermediateTablePrefix$schema)){
    logger$warn(paste0(intermediateTablePrefix$schema,".",intermediateTablePrefix$name,' Table A and B are present for'))
  }
  
  #Create the intermediate table
  qu <- paste0("CREATE TABLE ",intermediateTablePrefix$schema,".",intermediateTablePrefix$name,"_", pingPongTableNew, " ",
               ifelse(is.na(intermediateTablePrefix$tableSpace), "", paste("tablespace", intermediateTablePrefix$tableSpace)), " ",
               paste0(intermediateTablePrefix$options, collapse = " "),
               " AS SELECT * FROM ",originView$schema,".",originView$name)
  newResultTblCreated <- query(qu, applicationSettings = applicationSettings, conn = conn)
  if(class(newResultTblCreated) == "list") {
    logger$error(qu)
    logger$error(newResultTblCreated$error)
    error_ping_pong_generator <- TRUE
  } else {
    logger$debug(paste0(qu, " Successful"))
  }
  
  ##Primary Key creation
  if(!is.null(primaryKey)) {
    qu <- paste0(" ALTER TABLE ",intermediateTablePrefix$schema,".",intermediateTablePrefix$name,"_", pingPongTableNew," ADD PRIMARY KEY (",primaryKey,") ", paste0(intermediateTablePrefix$options, collapse = " "))
    primaryKeyCreated <- query(qu, applicationSettings = applicationSettings, conn = conn)
    if(class(primaryKeyCreated) == "list") {
      logger$error(qu)
      logger$error(primaryKeyCreated$error)
      error_ping_pong_generator <- TRUE
    } else {
      logger$debug(paste0(qu, " Successful"))
    }
  }
  
  #Index Creation
  if(!is.null(indexes)) {
    for(i in 1:length(indexes)) {
      currentTime <- paste0(as.character(format(Sys.time(), "%s")),strsplit(as.character(format(Sys.time(), "%OS3")),"\\.")[[1]][2])
      index <- indexes[[i]]
      idxName <- paste0(index$name,currentTime)
      idxName <- substr(idxName, nchar(idxName)-30+1, nchar(idxName))
      
      qu <- paste0(" CREATE INDEX ",idxName," ON ",intermediateTablePrefix$schema,".",intermediateTablePrefix$name,"_", pingPongTableNew," (",index$name,")  ",
                   ifelse(is.na(index$tableSpace), "", paste0("tablespace ", index$tableSpace)), " ",
                   paste0(index$options, collapse = " "))
      idxCreated <- query(qu, applicationSettings = applicationSettings, conn = conn)
      if(class(idxCreated) == "list") {
        logger$error(qu)
        logger$error(idxCreated$error)
        error_ping_pong_generator <- TRUE
      } else {
        logger$debug(paste0(qu, " Successful"))
      }
    }
  }
  ## refresh views
  qu <- paste0(" CREATE OR REPLACE VIEW ",destinationViewName$schema,".",destinationViewName$name,"
               as
               SELECT *
               FROM ",intermediateTablePrefix$schema, ".", intermediateTablePrefix$name,"_", pingPongTableNew,"
               ")
  destinationViewCreated <- query(qu, applicationSettings = applicationSettings, conn = conn)
  if(class(destinationViewCreated) == "list") {
    logger$error(qu)
    logger$error(newResultTblCreated$error)
    error_ping_pong_generator <- TRUE
  } else {
    logger$debug(paste0(qu, " Successful"))
  }
  
  #Drop the old table
  if (dbExistsTable(conn, name = paste0(intermediateTablePrefix$name,'_', pingPongTableOld), schema = intermediateTablePrefix$schema)){
    newResultTblRemoved <- query(paste0("drop table ",intermediateTablePrefix$schema,".",intermediateTablePrefix$name,"_", pingPongTableOld), applicationSettings = applicationSettings, conn = conn)
    if(class(newResultTblRemoved) == "list") {
      logger$error(paste0("Could not remove ",intermediateTablePrefix$schema,".",intermediateTablePrefix$name,"_", pingPongTableOld))
      logger$error(newResultTblRemoved$error)
    }
  }
  
  if(error_ping_pong_generator) {
    logger$error("PING-PONG tables update unsuccessful, rolledback ")
    stop(paste0("PING-PONG tables update unsuccessful, rolled back\n for details see\npingpongtables.log"))
  } else {
    logger$info("PING-PONG tables successfully updated and committed")
  }
}
