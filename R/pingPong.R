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
  logger <- createLogger(logName = "com.mcneilco.racas.pingpong.apiviews", logToConsole = FALSE)
  options(scipen=99)
  error_ping_pong_generator <- FALSE
  conn <- getDatabaseConnection(applicationSettings)
  on.exit(disconnected <- dbDisconnect(conn))
  
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
    msg <- paste0(intermediateTablePrefix$schema,".",intermediateTablePrefix$name,' Table A and B are present for ',intermediateTablePrefix$schema, ".", paste0(intermediateTablePrefix$name))
    logger$error(msg)
    stop(msg)
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
    stopUser(paste0("PING-PONG tables update unsuccessful, rolled back\n for details see\npingpongtables.log"))
  } else {
    logger$info("PING-PONG tables successfully updated and committed")
  }
}
startTransaction <- function(conn) {
  type <- getDBType(conn=conn)
  transaction <-switch(type,
                       "Postgres" = dbSendQuery(conn, "BEGIN TRANSACTION"),
                       "MySQL" = dbSendQuery(conn, "START TRANSACTION"),
                       "Oracle" = "NO OP"
  )
  return(transaction)
}
materialize_dose_response_views <- function(update = TRUE, createTableOptions = NA, createIndexOptions = NA) {
  logger <- createLogger(logName = "com.mcneilco.racas.materialize.doseresponse", logToConsole = TRUE)  
  logger$info("materialize dose response initiated")  
  conn <- getDatabaseConnection()
  on.exit({dbRollback(conn);dbDisconnect(conn)})
  transaction <- startTransaction(conn)
  
  curveIdsRemovedMaterializedName <-  "api_removed_curve_ids_tmp"
  curveIdsAddedMaterializedName <-  "api_added_curve_ids_tmp"
  curveIdsMaterializedName <-  "api_curve_ids_m"
  curveParamsMaterializedName <-  "api_curve_params_m"
  doseResponseMaterializedName <-  "api_dose_response_m"
  
  apiCurveIdsRemovedAlreadyExisted <- dbExistsTable(conn, curveIdsRemovedMaterializedName)
  apiCurveIdsAddedAlreadyExisted <- dbExistsTable(conn, curveIdsAddedMaterializedName)
  apiCurveIdsAlreadyExisted <- dbExistsTable(conn, curveIdsMaterializedName)
  apiCurveParamsAlreadyExisted <- dbExistsTable(conn, curveParamsMaterializedName)
  apiDoseResponseAlreadyExisted <- dbExistsTable(conn, doseResponseMaterializedName)
  
  currentCurveIdsSQL <- "SELECT /*+ FIRST_ROWS(1) */ analysisgr0_.string_value AS curveid, analysisgr0_.id as valueId
                                             FROM analysis_group_value analysisgr0_
                                             INNER JOIN analysis_group_state analysisgr1_
                                             ON analysisgr0_.analysis_state_id=analysisgr1_.id
                                             INNER JOIN analysis_group analysisgr2_
                                             ON analysisgr1_.analysis_group_id=analysisgr2_.id
                                             INNER JOIN analysis_group analysisgr3_
                                             ON analysisgr1_.analysis_group_id=analysisgr3_.id
                                             INNER JOIN EXPERIMENT_ANALYSISGROUP eag
                                             ON eag.analysis_group_id=analysisgr3_.id
                                             INNER JOIN EXPERIMENT e
                                             ON e.id                            =eag.experiment_id
                                             WHERE analysisgr1_.ignored         = '0'
                                             AND analysisgr0_.ls_type           ='stringValue'
                                             AND analysisgr0_.ls_kind           ='curve id'
                                             AND analysisgr0_.ignored           = '0'
                                             AND analysisgr2_.ignored           = '0'
                                             AND e.ignored                      = '0'
                                             AND e.deleted                      = '0'
                                             AND analysisgr1_.ls_type ='data'
                                             AND analysisgr1_.ls_kind ='dose response'"
  
  if(apiCurveIdsRemovedAlreadyExisted & update == TRUE) {
    if(apiCurveIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT INTO ",curveIdsRemovedMaterializedName,"
                  ( curveid, 
                    valueId
                  )
                  SELECT *
                    FROM api_curve_ids_m
                  WHERE NOT EXISTS
                  (SELECT *
                     FROM
                   (",
                   currentCurveIdsSQL,
                   ") a
                   WHERE ",curveIdsMaterializedName,".valueId = a. valueId
                  )"))
    }
  } else {
    if(apiCurveIdsRemovedAlreadyExisted) {
      logger$info(paste0(curveIdsRemovedMaterializedName, " already exists, dropping"))            
      dbSendQuery(conn, paste0("drop table ",curveIdsRemovedMaterializedName))
    }
    apiCurveIdsRemovedCreated <- dbSendQuery(conn, paste0("create global temporary table ",curveIdsRemovedMaterializedName," (curveid VARCHAR2(4000), valueid NUMBER(19)) on commit delete rows"))    
  }
  if(apiCurveIdsAddedAlreadyExisted & update == TRUE) {
    if(apiCurveIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",curveIdsAddedMaterializedName,"
                                            (
                                            curveid,
                                            valueId
                                            )
                                            ( SELECT a.curveid,a.valueId
                                            FROM (",currentCurveIdsSQL,") a
                                            WHERE NOT EXISTS
                                            (SELECT *
                                            FROM ",curveIdsMaterializedName," b
                                            WHERE b.valueId = a.valueId
                                            ))"))
    }
  } else {
    if(apiCurveIdsAddedAlreadyExisted) {
      logger$info(paste0(curveIdsAddedMaterializedName, " already exists, dropping"))            
      dbSendQuery(conn, paste0("drop table ",curveIdsAddedMaterializedName))
    }
    apiCurveIdsAddedCreated <- dbSendQuery(conn, paste0("create global temporary table ",curveIdsAddedMaterializedName," (curveid VARCHAR2(4000), valueid NUMBER(19)) on commit delete rows"))    
  }
  #Curve Ids
  if(apiCurveIdsAlreadyExisted & update == TRUE) {
    
    logger$info(paste0("updating ",curveIdsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",curveIdsMaterializedName,"
                                             WHERE valueid in (
                                                select valueid from ",curveIdsRemovedMaterializedName,"
                                             )"))
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",curveIdsMaterializedName,"
                                            (
                                            curveid,
                                            valueId
                                            )
                                            ( SELECT a.curveid,a.valueId
                                            FROM (",curveIdsAddedMaterializedName,") a
                                            )"))
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
  
  } else {
    if(apiCurveIdsAlreadyExisted) {
      logger$info(paste0(curveIdsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",curveIdsMaterializedName))
    }
    logger$info(paste0("creating ",curveIdsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",curveIdsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",currentCurveIdsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key curveid"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",curveIdsMaterializedName," ADD PRIMARY KEY (curveid) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
    logger$info(paste0("adding index IDX_API_CURVE_IDS_M_VALUEID"))
    valueidIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_CURVE_IDS_M_VALUEID ON ",curveIdsMaterializedName," (valueid)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
  
  }
  #Curve Params
  if(apiCurveParamsAlreadyExisted & update == TRUE) {
    logger$info(paste0("updating ",curveParamsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",curveParamsMaterializedName,"
                                             WHERE curveValueId in (
                                                select valueid from ",curveIdsRemovedMaterializedName,"
                                             )"))
    
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",curveParamsMaterializedName,"
                                            (
                                            stateid,
                                            valueid,
                                            codekind,
                                            codeorigin,
                                            codetype,
                                            codevalue,
                                            comments,
                                            concunit,
                                            concentration,
                                            lskind,
                                            lstransaction,
                                            lstype,
                                            numericvalue,
                                            operatorkind,
                                            operatortype,
                                            publicdata,
                                            recordedby,
                                            recordeddate,
                                            stringvalue,
                                            uncertainty,
                                            uncertaintytype,
                                            unitkind,
                                            unittype,
                                            urlvalue,
                                            version,
                                            curveid,
                                            curvevalueid,
                                            curvedisplaymin,
                                            curvedisplaymax
                                            )
                                            (SELECT api_curve_params.stateid,
                                            api_curve_params.valueid,
                                            api_curve_params.codekind,
                                            api_curve_params.codeorigin,
                                            api_curve_params.codetype,
                                            api_curve_params.codevalue,
                                            api_curve_params.comments,
                                            api_curve_params.concunit,
                                            api_curve_params.concentration,
                                            api_curve_params.lskind,
                                            api_curve_params.lstransaction,
                                            api_curve_params.lstype,
                                            api_curve_params.numericvalue,
                                            api_curve_params.operatorkind,
                                            api_curve_params.operatortype,
                                            api_curve_params.publicdata,
                                            api_curve_params.recordedby,
                                            api_curve_params.recordeddate,
                                            api_curve_params.stringvalue,
                                            api_curve_params.uncertainty,
                                            api_curve_params.uncertaintytype,
                                            api_curve_params.unitkind,
                                            api_curve_params.unittype,
                                            api_curve_params.urlvalue,
                                            api_curve_params.version,
                                            api_curve_params.curveid,
                                            api_curve_params.curveValueId,
                                            api_curve_params.curvedisplaymin,
                                            api_curve_params.curvedisplaymax
                                            FROM api_curve_params
                                            WHERE curveValueId in 
                                             ( SELECT a.valueId
                                            FROM (",curveIdsAddedMaterializedName,") a
                                            ))"))
    
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    
    if(apiCurveParamsAlreadyExisted) {
      logger$info(paste0(curveParamsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",curveParamsMaterializedName))
    }
    logger$info(paste0("creating ",curveParamsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",curveParamsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as select * from api_curve_params")
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
  
    logger$info(paste0("adding primary key curveid"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",curveParamsMaterializedName," ADD PRIMARY KEY (valueid) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
    logger$info(paste0("adding curveid index"))  
    indexSQL <- paste0("CREATE INDEX IDX_API_CURVE_PARAMS_M_CURVEID ON ",curveParamsMaterializedName," (curveid)",ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",indexSQL))
    curveidIndex <- dbSendQuery(conn,indexSQL)    
    
  }
  
  #Api Dose Response
  if(apiDoseResponseAlreadyExisted & update == TRUE) {
    logger$info(paste0("updating ",doseResponseMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",doseResponseMaterializedName,"
                                             WHERE curveValueId in (
                                               select valueid from ",curveIdsRemovedMaterializedName,"
                                             )"))

    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))
    missingData <- dbSendQuery(conn, paste0("INSERT
                                          INTO ",doseResponseMaterializedName,"
                                            (
                                            responsesubjectvalueid,
                                            analysisgroupcode,
                                            recorded_by,
                                            lstransaction,
                                            response,
                                            responseunits,
                                            responsekind,
                                            dose,
                                            doseunits,
                                            algorithmflagstatus,
                                            algorithmflagobservation,
                                            algorithmflagcause,
                                            algorithmflagcomment,
                                            preprocessflagstatus,
                                            preprocessflagobservation,
                                            preprocessflagcause,
                                            algorithmflaglskind,
                                            preprocessflaglskind,
                                            userflaglskind,
                                            preprocessflagcomment,
                                            userflagstatus,
                                            userflagobservation,
                                            userflagcause,
                                            userflagcomment,
                                            curveid,
                                            curveValueId
                                            )
                                            (SELECT api_dose_response.responsesubjectvalueid,
                                            api_dose_response.analysisgroupcode,
                                            api_dose_response.recorded_by,
                                            api_dose_response.lstransaction,
                                            api_dose_response.response,
                                            api_dose_response.responseunits,
                                            api_dose_response.responsekind,
                                            api_dose_response.dose,
                                            api_dose_response.doseunits,
                                            api_dose_response.algorithmflagstatus,
                                            api_dose_response.algorithmflagobservation,
                                            api_dose_response.algorithmflagcause,
                                            api_dose_response.algorithmflagcomment,
                                            api_dose_response.preprocessflagstatus,
                                            api_dose_response.preprocessflagobservation,
                                            api_dose_response.preprocessflagcause,
                                            api_dose_response.algorithmflaglskind,
                                            api_dose_response.preprocessflaglskind,
                                            api_dose_response.userflaglskind,
                                            api_dose_response.preprocessflagcomment,
                                            api_dose_response.userflagstatus,
                                            api_dose_response.userflagobservation,
                                            api_dose_response.userflagcause,
                                            api_dose_response.userflagcomment,
                                            api_dose_response.curveid,
                                            api_dose_response.curvevalueid
                                            FROM api_dose_response
                                            WHERE curveValueId in 
                                             ( SELECT a.valueId
                                            FROM (",curveIdsAddedMaterializedName,") a
                                            ))"))
    
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    if(apiDoseResponseAlreadyExisted) {
      logger$info(paste0(doseResponseMaterializedName, " already exists, dropping"))
      dbSendQuery(conn, paste0("DROP table ",doseResponseMaterializedName))
    }
    logger$info(paste0("creating ",doseResponseMaterializedName))
    createTableSQL <- paste0("CREATE table ",doseResponseMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as select * from api_dose_response")
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key responsesubjectvalueid"))
    primaryKey <- dbSendQuery(conn, paste0(" ALTER TABLE ",doseResponseMaterializedName," ADD PRIMARY KEY (responsesubjectvalueid) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions)))

    logger$info(paste0("adding index IDX_DOSE_RESPONSE_M_CURVEID"))
    curveidIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_DOSE_RESPONSE_M_CURVEID ON ",doseResponseMaterializedName," (curveid)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))

    logger$info(paste0("adding index IDX_DOSE_RESPONSE_M_VALUEID"))
    valueidIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_DOSE_RESPONSE_M_VALUEID ON ",doseResponseMaterializedName," (curvevalueid)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
  }
  
  logger$info(paste0("commiting transaction"))        
  commited <- dbCommit(conn)
  on.exit(dbDisconnect(conn))
  logger$info(paste0("materialization complete"))            
}
