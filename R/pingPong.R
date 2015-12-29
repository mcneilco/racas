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

materialize_analysis_group_results <- function(update = TRUE, createTableOptions = NA, createIndexOptions = NA) {
  logger <- createLogger(logName = "com.mcneilco.racas.materialize.analysisgroupresults", logToConsole = TRUE)  
  logger$info("materialize analysis group results initiated")  
  conn <- getDatabaseConnection()
  on.exit({dbRollback(conn);dbDisconnect(conn)})
  transaction <- startTransaction(conn)
  
  #For Optimization - set query optimizer version to 10.2
  
  agvIdsRemovedMaterializedName <-  "api_removed_agv_ids_tmp"
  agvIdsAddedMaterializedName <-  "api_added_agv_ids_tmp"
  agvIdsMaterializedName <-  "api_agv_ids_m"
  analysisGroupResultsMaterializedName <-  "api_analysis_group_results_m"
  
  apiAgvIdsRemovedAlreadyExisted <- dbExistsTable(conn, agvIdsRemovedMaterializedName)
  apiAgvIdsAddedAlreadyExisted <- dbExistsTable(conn, agvIdsAddedMaterializedName)
  apiAgvIdsAlreadyExisted <- dbExistsTable(conn, agvIdsMaterializedName)
  apiAnalysisGroupResultsAlreadyExisted <- dbExistsTable(conn, analysisGroupResultsMaterializedName)
  
  currentAgvIdsSQL <- "SELECT /*+ optimizer_features_enable('10.2.0.4') */ agv.id AS valueId
  FROM experiment e
  JOIN experiment_analysisgroup eag on e.id=eag.experiment_id
  JOIN analysis_GROUP ag ON eag.analysis_group_id = ag.id
  JOIN analysis_GROUP_state ags ON ags.analysis_GROUP_id = ag.id
  JOIN analysis_GROUP_value agv ON agv.analysis_state_id = ags.id AND agv.ls_kind <> 'batch code' AND agv.ls_kind <> 'time'
  JOIN analysis_GROUP_value agv2 ON agv2.analysis_state_id = ags.id and agv2.ls_kind = 'batch code'
  WHERE ag.ignored = '0' and
  ags.ignored = '0' and
  agv.ignored = '0' and
  e.ignored = '0'"
  
  if(apiAgvIdsRemovedAlreadyExisted & update == TRUE) {
    if(apiAgvIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT INTO ",agvIdsRemovedMaterializedName,"
                                          ( valueId
                                          )
                                          SELECT *
                                          FROM ",agvIdsMaterializedName,"
                                          WHERE NOT EXISTS (",
                                          currentAgvIdsSQL,
                                          " AND agv.id = ",agvIdsMaterializedName,".valueid
                                          )"))
    }
    } else {
      if(apiAgvIdsRemovedAlreadyExisted) {
        logger$info(paste0(agvIdsRemovedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",agvIdsRemovedMaterializedName))
      }
      apiAgvIdsRemovedCreated <- dbSendQuery(conn, paste0("create global temporary table ",agvIdsRemovedMaterializedName," (valueid NUMBER(19)) on commit delete rows"))    
    }
  if(apiAgvIdsAddedAlreadyExisted & update == TRUE) {
    if(apiAgvIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT
                                          INTO ",agvIdsAddedMaterializedName,"
                                          (
                                          valueId
                                          )
                                          SELECT a.valueId
                                            FROM (",currentAgvIdsSQL,") a
                                            WHERE NOT EXISTS
                                            (SELECT *
                                            FROM ",agvIdsMaterializedName," b
                                            WHERE b.valueId = a.valueId
                                          )"))
    }
    } else {
      if(apiAgvIdsAddedAlreadyExisted) {
        logger$info(paste0(agvIdsAddedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",agvIdsAddedMaterializedName))
      }
      apiCurveIdsAddedCreated <- dbSendQuery(conn, paste0("create global temporary table ",agvIdsAddedMaterializedName," (curveid VARCHAR2(4000), valueid NUMBER(19)) on commit delete rows"))    
    }
  #Analysis Group Value Ids
  if(apiAgvIdsAlreadyExisted & update == TRUE) {
    
    logger$info(paste0("updating ",agvIdsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",agvIdsMaterializedName,"
                                             WHERE valueid in (
                                             select valueid from ",agvIdsRemovedMaterializedName,"
                                             )"))
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",agvIdsMaterializedName,"
                                            (
                                            valueId
                                            )
                                            ( SELECT a.valueId
                                            FROM (",agvIdsAddedMaterializedName,") a
                                            )"))
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    if(apiAgvIdsAlreadyExisted) {
      logger$info(paste0(agvIdsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",agvIdsMaterializedName))
    }
    logger$info(paste0("creating ",agvIdsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",agvIdsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",currentAgvIdsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key valueid"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",agvIdsMaterializedName," ADD PRIMARY KEY (valueid) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
  }
  #Analysis Group Results
  selectAnalysisGroupResultsSQL <- "SELECT 'ACAS' as DB_MAP,
                                            'ACAS' as DB_SOURCE,
                                             'ACAS-' || p_api_analysis_group_results.agv_id as RESULT_ID,
                                             'ACAS-' || p_api_analysis_group_results.experiment_id as EXPERIMENT_ID,
                                             'ACAS-' || p_api_analysis_group_results.experiment_id as EXPERIMENT_BATCH_NUMBER,
                                             COALESCE(batch.id, X.batch_id) as batch_id,
                                             p_api_analysis_group_results.ls_kind,
                                             p_api_analysis_group_results.operator_kind,
                                             p_api_analysis_group_results.numeric_value,
                                             p_api_analysis_group_results.string_value,
                                             p_api_analysis_group_results.uncertainty,
                                             p_api_analysis_group_results.unit_kind,
                                             p_api_analysis_group_results.comments,
                                             p_api_analysis_group_results.tested_conc,
                                             p_api_analysis_group_results.tested_conc_unit,
                                             p_api_analysis_group_results.tested_lot,
                                             p_api_analysis_group_results.agv_id
                                            FROM p_api_analysis_group_results
                                            LEFT OUTER JOIN batch.batch ON p_api_analysis_group_results.tested_lot=batch.corp_batch_name
                                            LEFT OUTER JOIN
                                                (SELECT *
                                             	 FROM
                                               	 (SELECT v_api_batch_alias.*, row_number() over (partition BY alias order by batch_id) rn FROM BATCH.v_api_batch_alias
                                              	  )
                                             	 WHERE rn = 1
                                             	 ) X
                                            	ON ( p_api_analysis_group_results.tested_lot=X.alias )
                                           WHERE p_api_analysis_group_results.public_data = '1'"
  
  if(apiAnalysisGroupResultsAlreadyExisted & update == TRUE) {
    logger$info(paste0("updating ",analysisGroupResultsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",analysisGroupResultsMaterializedName,"
                                             WHERE agv_id in (
                                             select valueid from ",agvIdsRemovedMaterializedName,"
                                             )"))
    
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",analysisGroupResultsMaterializedName,"
                                            (
                                             DB_MAP,
                                             DB_SOURCE,
                                             RESULT_ID,
                                             EXPERIMENT_ID,
                                             EXPERIMENT_BATCH_NUMBER,
                                             BATCH_ID,
                                             LS_KIND,
                                             OPERATOR_KIND,
                                             NUMERIC_VALUE,
                                             STRING_VALUE,
                                             UNCERTAINTY,
                                             UNIT_KIND,
                                             COMMENTS,
                                             TESTED_CONC,
                                             TESTED_CONC_UNIT,
                                             TESTED_LOT,
                                             AGV_ID
                                            )
                                            (",selectAnalysisGroupResultsSQL,"
                                            AND agv_id in 
                                            ( SELECT a.valueId
                                            FROM (",agvIdsAddedMaterializedName,") a
                                            ))"))
    
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    
    if(apiAnalysisGroupResultsAlreadyExisted) {
      logger$info(paste0(analysisGroupResultsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",analysisGroupResultsMaterializedName))
    }
    logger$info(paste0("creating ",analysisGroupResultsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",analysisGroupResultsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",selectAnalysisGroupResultsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key result_id"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",analysisGroupResultsMaterializedName," ADD PRIMARY KEY (result_id) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
    logger$info(paste0("adding index IDX_API_AG_RESULTS_M_DB_MAP"))
    experimentIdIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_DB_MAP ON ",analysisGroupResultsMaterializedName," (db_map)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_AG_RESULTS_M_EXPT_ID"))
    experimentIdIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_EXPT_ID ON ",analysisGroupResultsMaterializedName," (experiment_id)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_AG_RESULTS_M_LOT"))
    testedLotIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_LOT ON ",analysisGroupResultsMaterializedName," (tested_lot)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_AG_RESULTS_M_BID"))
    batchIdIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_BID ON ",analysisGroupResultsMaterializedName," (batch_id)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_AG_RESULTS_M_LS_KIND"))
    lsKindIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_LS_KIND ON ",analysisGroupResultsMaterializedName," (ls_kind)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_AG_RESULTS_M_UNIT"))
    unitKindIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_UNIT ON ",analysisGroupResultsMaterializedName," (unit_kind)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_AG_RESULTS_M_AGV_ID"))
    unitKindIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_AGV_ID ON ",analysisGroupResultsMaterializedName," (agv_id)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("granting SELECT to SEURAT user"))
    granted <- dbSendQuery(conn,paste0("GRANT SELECT ON ",analysisGroupResultsMaterializedName," TO SEURAT WITH GRANT OPTION"))
  }
  
  logger$info(paste0("commiting transaction"))        
  commited <- dbCommit(conn)
  on.exit(dbDisconnect(conn))
  logger$info(paste0("materialization complete"))            
}

update_materialized_analysis_group_results_batch_alias <- function(update = TRUE, createTableOptions = NA, createIndexOptions = NA) {
  logger <- createLogger(logName = "com.mcneilco.racas.materialize.analysisgroupresultsbatchalias", logToConsole = TRUE)  
  logger$info("update analysis group results batch aliases initiated")  
  conn <- getDatabaseConnection()
  on.exit({dbRollback(conn);dbDisconnect(conn)})
  transaction <- startTransaction(conn)
  
  agvAliasIdsRemovedMaterializedName <-  "api_removed_agv_alias_ids_tmp"
  agvAliasIdsAddedMaterializedName <-  "api_added_agv_alias_ids_tmp"
  agvAliasIdsMaterializedName <-  "api_agv_alias_ids_m"
  analysisGroupResultsMaterializedName <-  "api_analysis_group_results_m"
  
  apiAgvIdsRemovedAlreadyExisted <- dbExistsTable(conn, agvAliasIdsRemovedMaterializedName)
  apiAgvIdsAddedAlreadyExisted <- dbExistsTable(conn, agvAliasIdsAddedMaterializedName)
  apiAgvIdsAlreadyExisted <- dbExistsTable(conn, agvAliasIdsMaterializedName)
  apiAnalysisGroupResultsAlreadyExisted <- dbExistsTable(conn, analysisGroupResultsMaterializedName)
  
  currentAgvBatchIdsSQL <- "SELECT DISTINCT agr.tested_lot, X.BATCH_ALIAS_ID 
                            FROM  BATCH.v_api_batch_alias X, ACAS.API_ANALYSIS_GROUP_RESULTS_M agr
                            WHERE X.alias = agr.tested_lot"
  
  if(apiAgvIdsRemovedAlreadyExisted & update == TRUE) {
    if(apiAgvIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT INTO ",agvAliasIdsRemovedMaterializedName,"
                                          (
                                          TESTED_LOT,
                                          BATCH_ALIAS_ID
                                          )
                                          SELECT *
                                          FROM ",agvAliasIdsMaterializedName,"
                                          WHERE NOT EXISTS (",
                                          currentAgvBatchIdsSQL,
                                          " AND X.batch_alias_id = ",agvAliasIdsMaterializedName,".batch_alias_id
                                          )"))
    }
    } else {
      if(apiAgvIdsRemovedAlreadyExisted) {
        logger$info(paste0(agvAliasIdsRemovedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",agvAliasIdsRemovedMaterializedName))
      }
      apiAgvIdsRemovedCreated <- dbSendQuery(conn, paste0("create global temporary table ",agvAliasIdsRemovedMaterializedName," (tested_lot VARCHAR2(255), batch_alias_id NUMBER(19)) on commit delete rows"))    
  }
  if(apiAgvIdsAddedAlreadyExisted & update == TRUE) {
    if(apiAgvIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT
                                          INTO ",agvAliasIdsAddedMaterializedName,"
                                          (
                                          tested_lot,
                                          batch_alias_id
                                          )
                                          SELECT a.tested_lot, a.batch_alias_id
                                          FROM (",currentAgvBatchIdsSQL,") a
                                          WHERE NOT EXISTS
                                          (SELECT *
                                          FROM ",agvAliasIdsMaterializedName," b
                                          WHERE b.batch_alias_id = a.batch_alias_id
                                          )"))
    }
    } else {
      if(apiAgvIdsAddedAlreadyExisted) {
        logger$info(paste0(agvAliasIdsAddedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",agvAliasIdsAddedMaterializedName))
      }
      apiCurveIdsAddedCreated <- dbSendQuery(conn, paste0("create global temporary table ",agvAliasIdsAddedMaterializedName," (tested_lot VARCHAR2(255), batch_alias_id NUMBER(19)) on commit delete rows"))    
    }
  #Batch Alias Ids Referenced by Analysis Group Values
  if(apiAgvIdsAlreadyExisted & update == TRUE) {
    
    logger$info(paste0("updating ",agvAliasIdsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",agvAliasIdsMaterializedName,"
                                             WHERE (tested_lot, batch_alias_id) in (
                                             select tested_lot, batch_alias_id from ",agvAliasIdsRemovedMaterializedName,"
                                             )"))
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",agvAliasIdsMaterializedName,"
                                            (
                                            tested_lot,
                                            batch_alias_id
                                            )
                                            ( SELECT a.tested_lot, a.batch_alias_id
                                            FROM (",agvAliasIdsAddedMaterializedName,") a
                                            )"))
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    if(apiAgvIdsAlreadyExisted) {
      logger$info(paste0(agvAliasIdsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",agvAliasIdsMaterializedName))
    }
    logger$info(paste0("creating ",agvAliasIdsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",agvAliasIdsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",currentAgvBatchIdsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key tested_lot, batch_alias_id"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",agvAliasIdsMaterializedName," ADD PRIMARY KEY (tested_lot, batch_alias_id) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
  }
  #Analysis Group Results  
  if(apiAnalysisGroupResultsAlreadyExisted & update == TRUE) {
    logger$info(paste0("updating ",analysisGroupResultsMaterializedName)) 
    missingData <- dbSendQuery(conn, paste0("UPDATE
                                            ACAS.API_ANALYSIS_GROUP_RESULTS_M agr
                                            SET batch_id =
                                            (SELECT COALESCE(batch.id, X.batch_id)
                                            FROM batch.batch 
                                            ,
                                            (SELECT *
                                            FROM
                                            (SELECT v_api_batch_alias.*, row_number() over (partition BY alias order by batch_id) rn FROM BATCH.v_api_batch_alias
                                            )
                                            WHERE rn = 1
                                            ) X
                                            WHERE agr.tested_lot=batch.corp_batch_name (+)
                                            AND agr.tested_lot=X.alias  (+))
                                            WHERE EXISTS 
                                            (SELECT * from 
                                            ( SELECT a.tested_lot
                                            FROM (api_added_agv_alias_ids_tmp) a
                                            UNION
                                            SELECT b.tested_lot
                                            FROM (api_removed_agv_alias_ids_tmp) b
                                            ) ab WHERE ab.tested_lot = agr.tested_lot)"))
    
    logger$info(paste0("updated ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    
#     if(apiAnalysisGroupResultsAlreadyExisted) {
#       logger$info(paste0(analysisGroupResultsMaterializedName, " already exists, dropping"))      
#       dbSendQuery(conn, paste0("DROP table ",analysisGroupResultsMaterializedName))
#     }
#     #TODO- add in SQL for api_agr creation
#     logger$info(paste0("creating ",analysisGroupResultsMaterializedName))          
#     createTableSQL <- paste0("CREATE table ",analysisGroupResultsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",selectAnalysisGroupResultsSQL)
#     logger$debug(paste0("executing sql ",createTableSQL))
#     finished <- dbSendQuery(conn, createTableSQL)
#     
#     logger$info(paste0("adding primary key agv_id"))  
#     primaryKeySQL <- paste0(" ALTER TABLE ",analysisGroupResultsMaterializedName," ADD PRIMARY KEY (agv_id) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
#     logger$debug(paste0("executing sql ",primaryKeySQL))
#     finished <- dbSendQuery(conn, primaryKeySQL)
#     
#     logger$info(paste0("adding index IDX_API_AG_RESULTS_M_EXPT_ID"))
#     experimentIdIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_EXPT_ID ON ",analysisGroupResultsMaterializedName," (experiment_id)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
#     
#     logger$info(paste0("adding index IDX_API_AG_RESULTS_M_LOT"))
#     testedLotIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_LOT ON ",analysisGroupResultsMaterializedName," (tested_lot)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
#     
#     logger$info(paste0("adding index IDX_API_AG_RESULTS_M_BID"))
#     batchIdIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_BID ON ",analysisGroupResultsMaterializedName," (batch_id)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
#     
#     logger$info(paste0("adding index IDX_API_AG_RESULTS_M_LS_KIND"))
#     lsKindIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_LS_KIND ON ",analysisGroupResultsMaterializedName," (ls_kind)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
#     
#     logger$info(paste0("adding index IDX_API_AG_RESULTS_M_UNIT"))
#     unitKindIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_AG_RESULTS_M_UNIT ON ",analysisGroupResultsMaterializedName," (unit_kind)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
#     
  }
  
  logger$info(paste0("commiting transaction"))        
  commited <- dbCommit(conn)
  on.exit(dbDisconnect(conn))
  logger$info(paste0("materialization complete"))            
  }

materialize_treatment_group_results <- function(update = TRUE, createTableOptions = NA, createIndexOptions = NA) {
  logger <- createLogger(logName = "com.mcneilco.racas.materialize.treatmentgroupresults", logToConsole = TRUE)  
  logger$info("materialize treatment group results initiated")  
  conn <- getDatabaseConnection()
  on.exit({dbRollback(conn);dbDisconnect(conn)})
  transaction <- startTransaction(conn)
  
  htsExperimentIdsRemovedMaterializedName <-  "api_removed_hts_expt_ids_tmp"
  htsExperimentIdsAddedMaterializedName <-  "api_added_hts_expt_ids_tmp"
  htsExperimentIdsMaterializedName <-  "api_hts_expt_ids_m"
  treatmentGroupResultsMaterializedName <-  "api_treatment_group_results_m"
  
  apiHtsExperimentIdsRemovedAlreadyExisted <- dbExistsTable(conn, htsExperimentIdsRemovedMaterializedName)
  apiHtsExperimentIdsAddedAlreadyExisted <- dbExistsTable(conn, htsExperimentIdsAddedMaterializedName)
  apiHtsExperimentIdsAlreadyExisted <- dbExistsTable(conn, htsExperimentIdsMaterializedName)
  apiTreatmentGroupResultsAlreadyExisted <- dbExistsTable(conn, treatmentGroupResultsMaterializedName)
  
  currentHtsExperimentIdsSQL <- "SELECT e.id as experiment_id, ev.id as analysis_result_value_id, ev.version as value_version
                                FROM experiment e
                                JOIN experiment_state es ON es.experiment_id = e.id
                                JOIN experiment_value ev ON es.id = ev.experiment_state_id
                                WHERE ev.ls_kind = 'analysis result html'
                                AND e.ignored='0'
                                AND es.ignored='0'
                                AND ev.ignored='0'"
  
  if(apiHtsExperimentIdsRemovedAlreadyExisted & update == TRUE) {
    if(apiHtsExperimentIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT INTO ",htsExperimentIdsRemovedMaterializedName,"
                                          (
                                          experiment_id,
                                          analysis_result_value_id,
                                          value_version
                                          )
                                          SELECT *
                                          FROM ",htsExperimentIdsMaterializedName,"
                                          WHERE NOT EXISTS ( ",
                                          currentHtsExperimentIdsSQL,"
                                          AND e.id = ",htsExperimentIdsMaterializedName,".experiment_id
                                          AND ev.id = ",htsExperimentIdsMaterializedName,".analysis_result_value_id
                                          AND ev.version = ",htsExperimentIdsMaterializedName,".value_version
                                          )"))
    }
    } else {
      if(apiHtsExperimentIdsRemovedAlreadyExisted) {
        logger$info(paste0(htsExperimentIdsRemovedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",htsExperimentIdsRemovedMaterializedName))
      }
      apiHtsExperimentIdsRemovedCreated <- dbSendQuery(conn, paste0("create global temporary table ",htsExperimentIdsRemovedMaterializedName," (experiment_id NUMBER(19), analysis_result_value_id NUMBER(19), value_version NUMBER(10)) on commit delete rows"))    
    }
  if(apiHtsExperimentIdsAddedAlreadyExisted & update == TRUE) {
    if(apiHtsExperimentIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT
                                          INTO ",htsExperimentIdsAddedMaterializedName,"
                                          (
                                          experiment_id,
                                          analysis_result_value_id,
                                          value_version
                                          )
                                          SELECT a.experiment_id, a.analysis_result_value_id, a.value_version
                                            FROM (",currentHtsExperimentIdsSQL,") a
                                            WHERE NOT EXISTS
                                            (SELECT *
                                            FROM ",htsExperimentIdsMaterializedName," b
                                            WHERE b.experiment_id = a.experiment_id
                                            AND b.analysis_result_value_id = a.analysis_result_value_id
                                            AND b.value_version = a.value_version
                                          )"))
    }
    } else {
      if(apiHtsExperimentIdsAddedAlreadyExisted) {
        logger$info(paste0(htsExperimentIdsAddedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",htsExperimentIdsAddedMaterializedName))
      }
      apiHtsExperimentIdsAddedCreated <- dbSendQuery(conn, paste0("create global temporary table ",htsExperimentIdsAddedMaterializedName," (experiment_id NUMBER(19), analysis_result_value_id NUMBER(19), value_version NUMBER(10)) on commit delete rows"))    
    }
  #HTS Experiment Ids, along with analysis status html value id and version
  if(apiHtsExperimentIdsAlreadyExisted & update == TRUE) {
    
    logger$info(paste0("updating ",htsExperimentIdsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",htsExperimentIdsMaterializedName,"
                                             WHERE experiment_id in (
                                             select experiment_id from ",htsExperimentIdsRemovedMaterializedName,"
                                             )"))
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",htsExperimentIdsMaterializedName,"
                                            (
                                            experiment_id,
                                            analysis_result_value_id,
                                            value_version
                                            )
                                            ( SELECT a.experiment_id,
                                              a.analysis_result_value_id,
                                              a.value_version
                                            FROM (",htsExperimentIdsAddedMaterializedName,") a
                                            )"))
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    if(apiHtsExperimentIdsAlreadyExisted) {
      logger$info(paste0(htsExperimentIdsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",htsExperimentIdsMaterializedName))
    }
    logger$info(paste0("creating ",htsExperimentIdsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",htsExperimentIdsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",currentHtsExperimentIdsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key experiment_id"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",htsExperimentIdsMaterializedName," ADD PRIMARY KEY (experiment_id) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
    logger$info(paste0("adding index IDX_API_HTS_EXPT_IDS_M_VALUEID"))
    valueidIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_HTS_EXPT_IDS_M_VALUEID ON ",htsExperimentIdsMaterializedName," (analysis_result_value_id)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_HTS_EXPT_IDS_M_VERSION"))
    versionIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_HTS_EXPT_IDS_M_VERSION ON ",htsExperimentIdsMaterializedName," (value_version)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
  }
  #Treatment Group Results
  selectTreatmentGroupResultsSQL <- "SELECT api_hts_treatment_results.experiment_id,
                                            api_hts_treatment_results.tested_lot,
                                            api_hts_treatment_results.concentration,
                                            api_hts_treatment_results.conc_unit,
                                            api_hts_treatment_results.tgv_id,
                                            api_hts_treatment_results.ls_kind,
                                            api_hts_treatment_results.operator_kind,
                                            api_hts_treatment_results.numeric_value,
                                            api_hts_treatment_results.uncertainty,
                                            api_hts_treatment_results.unit_kind,
                                            api_hts_treatment_results.string_value,
                                            api_hts_treatment_results.comments,
                                            api_hts_treatment_results.recorded_date,
                                            api_hts_treatment_results.public_data,
                                            api_hts_treatment_results.state_id,
                                            api_hts_treatment_results.treatment_group_id,
                                            COALESCE(batch.id, X.batch_id) as batch_id
                                            FROM api_hts_treatment_results
                                            LEFT OUTER JOIN batch.batch ON api_hts_treatment_results.tested_lot=batch.corp_batch_name
                                            LEFT OUTER JOIN
                                                (SELECT *
                                                FROM
                                                  (SELECT v_api_batch_alias.*, row_number() over (partition BY alias order by batch_id) rn FROM BATCH.v_api_batch_alias
                                              	  )
                                             	 WHERE rn = 1
                                             	 ) X
                                            	ON ( api_hts_treatment_results.tested_lot=X.alias )
                                            "
  #WHERE api_hts_treatment_results.public_data = '1'"
  
  if(apiTreatmentGroupResultsAlreadyExisted & update == TRUE) {
    logger$info(paste0("updating ",treatmentGroupResultsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",treatmentGroupResultsMaterializedName,"
                                             WHERE experiment_id in (
                                             select experiment_id from ",htsExperimentIdsRemovedMaterializedName,"
                                             )"))
    
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",treatmentGroupResultsMaterializedName,"
                                            (
                                            experiment_id,
                                            tested_lot,
                                            concentration,
                                            conc_unit,
                                            tgv_id,
                                            ls_kind,
                                            operator_kind,
                                            numeric_value,
                                            uncertainty,
                                            unit_kind,
                                            string_value,
                                            comments,
                                            recorded_date,
                                            public_data,
                                            state_id,
                                            treatment_group_id,
                                            batch_id
                                            )
                                            (", selectTreatmentGroupResultsSQL, "
                                            AND experiment_id in 
                                            ( SELECT a.experiment_id
                                            FROM (",htsExperimentIdsAddedMaterializedName,") a
                                            ))"))
    
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    
    if(apiTreatmentGroupResultsAlreadyExisted) {
      logger$info(paste0(treatmentGroupResultsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",treatmentGroupResultsMaterializedName))
    }
    logger$info(paste0("creating ",treatmentGroupResultsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",treatmentGroupResultsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",selectTreatmentGroupResultsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key tgv_id"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",treatmentGroupResultsMaterializedName," ADD PRIMARY KEY (tgv_id) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
    logger$info(paste0("adding index IDX_API_TG_RESULTS_M_EXPT_ID"))
    experimentIdIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_TG_RESULTS_M_EXPT_ID ON ",treatmentGroupResultsMaterializedName," (experiment_id)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_TG_RESULTS_M_LOT"))
    testedLotIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_TG_RESULTS_M_LOT ON ",treatmentGroupResultsMaterializedName," (tested_lot)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_TG_RESULTS_M_LS_KIND"))
    lsKindIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_TG_RESULTS_M_LS_KIND ON ",treatmentGroupResultsMaterializedName," (ls_kind)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_TG_RESULTS_M_UNIT"))
    unitKindIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_TG_RESULTS_M_UNIT ON ",treatmentGroupResultsMaterializedName," (unit_kind)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("granting SELECT to SEURAT user"))
    granted <- dbSendQuery(conn,paste0("GRANT SELECT ON ",treatmentGroupResultsMaterializedName," TO SEURAT WITH GRANT OPTION"))
    
  }
  
  logger$info(paste0("commiting transaction"))        
  commited <- dbCommit(conn)
  on.exit(dbDisconnect(conn))
  logger$info(paste0("materialization complete"))            
  }

update_materialized_treatment_group_results_batch_alias <- function(update = TRUE, createTableOptions = NA, createIndexOptions = NA) {
  logger <- createLogger(logName = "com.mcneilco.racas.materialize.treatmentgroupresultsbatchalias", logToConsole = TRUE)  
  logger$info("update treatment group results batch aliases initiated")  
  conn <- getDatabaseConnection()
  on.exit({dbRollback(conn);dbDisconnect(conn)})
  transaction <- startTransaction(conn)
  
  tgvAliasIdsRemovedMaterializedName <-  "api_removed_tgv_alias_ids_tmp"
  tgvAliasIdsAddedMaterializedName <-  "api_added_tgv_alias_ids_tmp"
  tgvAliasIdsMaterializedName <-  "api_tgv_alias_ids_m"
  treatmentGroupResultsMaterializedName <-  "api_treatment_group_results_m"
  
  apiTgvIdsRemovedAlreadyExisted <- dbExistsTable(conn, tgvAliasIdsRemovedMaterializedName)
  apiTgvIdsAddedAlreadyExisted <- dbExistsTable(conn, tgvAliasIdsAddedMaterializedName)
  apiTgvIdsAlreadyExisted <- dbExistsTable(conn, tgvAliasIdsMaterializedName)
  apiTreatmentGroupResultsAlreadyExisted <- dbExistsTable(conn, treatmentGroupResultsMaterializedName)
  
  currentTgvBatchIdsSQL <- "SELECT DISTINCT tgr.tested_lot, X.BATCH_ALIAS_ID 
  FROM  BATCH.v_api_batch_alias X, ACAS.API_TREATMENT_GROUP_RESULTS_M tgr
  WHERE X.alias = tgr.tested_lot"
  
  if(apiTgvIdsRemovedAlreadyExisted & update == TRUE) {
    if(apiTgvIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT INTO ",tgvAliasIdsRemovedMaterializedName,"
                                          (
                                          TESTED_LOT,
                                          BATCH_ALIAS_ID
                                          )
                                          SELECT *
                                          FROM ",tgvAliasIdsMaterializedName,"
                                          WHERE NOT EXISTS (",
                                          currentTgvBatchIdsSQL,
                                          " AND X.batch_alias_id = ",tgvAliasIdsMaterializedName,".batch_alias_id
                                          )"))
    }
    } else {
      if(apiTgvIdsRemovedAlreadyExisted) {
        logger$info(paste0(tgvAliasIdsRemovedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",tgvAliasIdsRemovedMaterializedName))
      }
      apiTgvIdsRemovedCreated <- dbSendQuery(conn, paste0("create global temporary table ",tgvAliasIdsRemovedMaterializedName," (tested_lot VARCHAR2(255), batch_alias_id NUMBER(19)) on commit delete rows"))    
  }
  if(apiTgvIdsAddedAlreadyExisted & update == TRUE) {
    if(apiTgvIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT
                                          INTO ",tgvAliasIdsAddedMaterializedName,"
                                          (
                                          tested_lot,
                                          batch_alias_id
                                          )
                                          SELECT a.tested_lot, a.batch_alias_id
                                          FROM (",currentTgvBatchIdsSQL,") a
                                          WHERE NOT EXISTS
                                          (SELECT *
                                          FROM ",tgvAliasIdsMaterializedName," b
                                          WHERE b.batch_alias_id = a.batch_alias_id
                                          )"))
    }
    } else {
      if(apiTgvIdsAddedAlreadyExisted) {
        logger$info(paste0(tgvAliasIdsAddedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",tgvAliasIdsAddedMaterializedName))
      }
      apiCurveIdsAddedCreated <- dbSendQuery(conn, paste0("create global temporary table ",tgvAliasIdsAddedMaterializedName," (tested_lot VARCHAR2(255), batch_alias_id NUMBER(19)) on commit delete rows"))    
    }
  #Batch Alias Ids Referenced by Treatment Group Values
  if(apiTgvIdsAlreadyExisted & update == TRUE) {
    
    logger$info(paste0("updating ",tgvAliasIdsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",tgvAliasIdsMaterializedName,"
                                             WHERE (tested_lot, batch_alias_id) in (
                                             select tested_lot, batch_alias_id from ",tgvAliasIdsRemovedMaterializedName,"
                                             )"))
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",tgvAliasIdsMaterializedName,"
                                            (
                                            tested_lot,
                                            batch_alias_id
                                            )
                                            ( SELECT a.tested_lot, a.batch_alias_id
                                            FROM (",tgvAliasIdsAddedMaterializedName,") a
                                            )"))
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    if(apiTgvIdsAlreadyExisted) {
      logger$info(paste0(tgvAliasIdsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",tgvAliasIdsMaterializedName))
    }
    logger$info(paste0("creating ",tgvAliasIdsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",tgvAliasIdsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",currentTgvBatchIdsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key tested_lot, batch_alias_id"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",tgvAliasIdsMaterializedName," ADD PRIMARY KEY (tested_lot, batch_alias_id) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
  }
  #Treatment Group Results  
  if(apiTreatmentGroupResultsAlreadyExisted & update == TRUE) {
    logger$info(paste0("updating ",treatmentGroupResultsMaterializedName)) 
    missingData <- dbSendQuery(conn, paste0("UPDATE
                                            ACAS.API_TREATMENT_GROUP_RESULTS_M tgr
                                            SET batch_id =
                                            (SELECT COALESCE(batch.id, X.batch_id)
                                            FROM batch.batch 
                                            ,
                                            (SELECT *
                                            FROM
                                            (SELECT v_api_batch_alias.*, row_number() over (partition BY alias order by batch_id) rn FROM BATCH.v_api_batch_alias
                                            )
                                            WHERE rn = 1
                                            ) X
                                            WHERE tgr.tested_lot=batch.corp_batch_name (+)
                                            AND tgr.tested_lot=X.alias  (+))
                                            WHERE EXISTS 
                                            (SELECT * from 
                                            ( SELECT a.tested_lot
                                            FROM (api_added_tgv_alias_ids_tmp) a
                                            UNION
                                            SELECT b.tested_lot
                                            FROM (api_removed_tgv_alias_ids_tmp) b
                                            ) ab WHERE ab.tested_lot = tgr.tested_lot)"))
    
    logger$info(paste0("updated ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    
   #TODO: throw error or create api_treatment_group_results_M table    
  }
  
  logger$info(paste0("commiting transaction"))        
  commited <- dbCommit(conn)
  on.exit(dbDisconnect(conn))
  logger$info(paste0("materialization complete"))            
  }

materialize_experiments <- function(update = TRUE, createTableOptions = NA, createIndexOptions = NA) {
  logger <- createLogger(logName = "com.mcneilco.racas.materialize.experiments", logToConsole = TRUE)  
  logger$info("materialize experiments initiated")  
  conn <- getDatabaseConnection()
  on.exit({dbRollback(conn);dbDisconnect(conn)})
  transaction <- startTransaction(conn)
  
  exptIdsRemovedMaterializedName <-  "api_removed_expt_ids_tmp"
  exptIdsAddedMaterializedName <-  "api_added_expt_ids_tmp"
  exptIdsMaterializedName <-  "api_expt_ids_m"
  experimentsMaterializedName <-  "api_experiment_m"
  
  apiExptIdsRemovedAlreadyExisted <- dbExistsTable(conn, exptIdsRemovedMaterializedName)
  apiExptIdsAddedAlreadyExisted <- dbExistsTable(conn, exptIdsAddedMaterializedName)
  apiExptIdsAlreadyExisted <- dbExistsTable(conn, exptIdsMaterializedName)
  apiExperimentsAlreadyExisted <- dbExistsTable(conn, experimentsMaterializedName)
  
  currentExptIdsSQL <- "SELECT /*+ FIRST_ROWS(1) */  e.id AS expt_id, e.version as version
  FROM experiment e
  JOIN experiment_label el
  ON e.id         =el.experiment_id
  JOIN experiment_state es
  ON e.id=es.experiment_id
  JOIN experiment_value ev
  ON Es.Id=Ev.Experiment_State_Id
  WHERE e.ignored ='0'
  AND el.preferred='1'
  AND el.ignored  ='0'
  AND ev.ignored = '0'
  AND es.ls_kind='experiment metadata'
  AND ev.ls_kind='experiment status'
  AND ev.code_value IN ('approved', 'complete') GROUP BY e.id, e.version"
  
  if(apiExptIdsRemovedAlreadyExisted & update == TRUE) {
    if(apiExptIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT INTO ",exptIdsRemovedMaterializedName,"
                                          ( expt_id,
                                          version
                                          )
                                          SELECT *
                                          FROM ",exptIdsMaterializedName,"
                                          WHERE NOT EXISTS
                                          (SELECT *
                                          FROM
                                          (",
                                          currentExptIdsSQL,
                                          ") a
                                          WHERE ",exptIdsMaterializedName,".expt_id = a. expt_id
                                          AND ",exptIdsMaterializedName,".version = a. version
                                          )"))
    }
    } else {
      if(apiExptIdsRemovedAlreadyExisted) {
        logger$info(paste0(exptIdsRemovedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",exptIdsRemovedMaterializedName))
      }
      apiExptIdsRemovedCreated <- dbSendQuery(conn, paste0("create global temporary table ",exptIdsRemovedMaterializedName," (expt_id NUMBER(19), version NUMBER(19)) on commit delete rows"))    
    }
  if(apiExptIdsAddedAlreadyExisted & update == TRUE) {
    if(apiExptIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT
                                          INTO ",exptIdsAddedMaterializedName,"
                                          (
                                          expt_id,
                                          version
                                          )
                                          ( SELECT a.expt_id, a.version
                                          FROM (",currentExptIdsSQL,") a
                                          WHERE NOT EXISTS
                                          (SELECT *
                                          FROM ",exptIdsMaterializedName," b
                                          WHERE b.expt_id = a.expt_id
                                          AND b.version = a.version
                                          ))"))
    }
    } else {
      if(apiExptIdsAddedAlreadyExisted) {
        logger$info(paste0(exptIdsAddedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",exptIdsAddedMaterializedName))
      }
      apiCurveIdsAddedCreated <- dbSendQuery(conn, paste0("create global temporary table ",exptIdsAddedMaterializedName," (expt_id NUMBER(19), version NUMBER(19)) on commit delete rows"))    
    }
  #Experiment Ids
  if(apiExptIdsAlreadyExisted & update == TRUE) {
    
    logger$info(paste0("updating ",exptIdsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",exptIdsMaterializedName,"
                                             WHERE (expt_id, version) in (
                                             select expt_id, version from ",exptIdsRemovedMaterializedName,"
                                             )"))
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",exptIdsMaterializedName,"
                                            (
                                            expt_id,
                                            version
                                            )
                                            ( SELECT a.expt_id, a.version
                                            FROM (",exptIdsAddedMaterializedName,") a
                                            )"))
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    if(apiExptIdsAlreadyExisted) {
      logger$info(paste0(exptIdsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",exptIdsMaterializedName))
    }
    logger$info(paste0("creating ",exptIdsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",exptIdsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",currentExptIdsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key expt_id"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",exptIdsMaterializedName," ADD PRIMARY KEY (expt_id) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
    logger$info(paste0("adding version index"))  
    indexSQL <- paste0("CREATE INDEX IDX_EXPT_IDS_M_VERSION ON ",exptIdsMaterializedName," (version)",ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",indexSQL))
    curveidIndex <- dbSendQuery(conn,indexSQL)
    
  }
  
  #Experiments
  selectExperimentsSQL <- "SELECT 
                                            'ACAS' as DB_MAP,
                                            'ACAS' as DB_SOURCE,
                                            api_experiment.HTS_FORMAT,
                                            'ACAS-' || api_experiment.ID as EXPERIMENT_ID,
                                            api_experiment.ID as EXPERIMENT_ID_ACAS,
                                            api_experiment.LABEL_TEXT AS EXPERIMENT_NAME,
                                            api_experiment.RECORDED_BY,
                                            api_experiment.COMPLETION_DATE,                                            
                                            'Dart NeuroScience, No Location' AS EXPERIMENT_LOCATION,
                                            api_experiment.NOTEBOOK,
                                            api_experiment.NOTEBOOK_PAGE,
                                            api_experiment.SHORT_DESCRIPTION,
                                            api_experiment.STATUS,
                                            api_experiment.PROTOCOL_ID,
                                            api_experiment.VERSION
                                            FROM api_experiment
                                            WHERE api_experiment.STATUS IN ('approved', 'complete')"
  if(apiExperimentsAlreadyExisted & update == TRUE) {
    logger$info(paste0("updating ",experimentsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",experimentsMaterializedName," a
                                            WHERE EXISTS 
                                            ( SELECT * 
                                            FROM (",exptIdsRemovedMaterializedName,") b
                                            WHERE b.expt_id = a.EXPERIMENT_ID_ACAS
                                            AND b.version = a.VERSION
                                             )"))
    
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",experimentsMaterializedName,"
                                            (
                                             DB_MAP,
                                             DB_SOURCE,
                                             HTS_FORMAT,
                                             EXPERIMENT_ID,
                                             EXPERIMENT_ID_ACAS,
                                             EXPERIMENT_NAME,
                                             RECORDED_BY,
                                             COMPLETION_DATE,
                                             EXPERIMENT_LOCATION,
                                             NOTEBOOK,
                                             NOTEBOOK_PAGE,
                                             SHORT_DESCRIPTION,
                                             STATUS,
                                             PROTOCOL_ID,
                                             VERSION
                                            )
                                            (",selectExperimentsSQL,"
                                            AND EXISTS 
                                            ( SELECT * 
                                            FROM (",exptIdsAddedMaterializedName,") a
                                            WHERE a.expt_id = api_experiment.ID
                                            AND a.version = api_experiment.VERSION
                                            ))"))
    
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
  } else {
    
    if(apiExperimentsAlreadyExisted) {
      logger$info(paste0(experimentsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",experimentsMaterializedName))
    }
    logger$info(paste0("creating ",experimentsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",experimentsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",selectExperimentsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key expt_id"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",experimentsMaterializedName," ADD PRIMARY KEY (experiment_id) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
    logger$info(paste0("adding version index"))  
    indexSQL <- paste0("CREATE INDEX IDX_API_EXPERIMENT_M_VERSION ON ",experimentsMaterializedName," (version)",ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",indexSQL))
    versionIndex <- dbSendQuery(conn,indexSQL)
    
    logger$info(paste0("adding db_map index"))  
    indexSQL <- paste0("CREATE INDEX IDX_API_EXPERIMENT_M_DB_MAP ON ",experimentsMaterializedName," (db_map)",ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",indexSQL))
    versionIndex <- dbSendQuery(conn,indexSQL)
    
    logger$info(paste0("adding index IDX_API_EXPERIMENT_M_ID_ACAS"))
    experimentNameIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_EXPERIMENT_M_ID_ACAS ON ",experimentsMaterializedName," (experiment_id_acas)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_EXPERIMENT_M_NAME"))
    experimentNameIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_EXPERIMENT_M_NAME ON ",experimentsMaterializedName," (experiment_name)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("granting SELECT to SEURAT user"))
    granted <- dbSendQuery(conn,paste0("GRANT SELECT ON ",experimentsMaterializedName," TO SEURAT WITH GRANT OPTION"))
    
  }
  
  logger$info(paste0("commiting transaction"))        
  commited <- dbCommit(conn)
  on.exit(dbDisconnect(conn))
  logger$info(paste0("materialization complete"))            
}

materialize_protocols <- function(update = TRUE, createTableOptions = NA, createIndexOptions = NA) {
  logger <- createLogger(logName = "com.mcneilco.racas.materialize.protocols", logToConsole = TRUE)  
  logger$info("materialize protocols initiated")  
  conn <- getDatabaseConnection()
  on.exit({dbRollback(conn);dbDisconnect(conn)})
  transaction <- startTransaction(conn)
  
  protocolIdsRemovedMaterializedName <-  "api_removed_protocol_ids_tmp"
  protocolIdsAddedMaterializedName <-  "api_added_protocol_ids_tmp"
  protocolIdsMaterializedName <-  "api_protocol_ids_m"
  protocolsMaterializedName <-  "api_protocol_m"
  
  apiProtocolIdsRemovedAlreadyExisted <- dbExistsTable(conn, protocolIdsRemovedMaterializedName)
  apiProtocolIdsAddedAlreadyExisted <- dbExistsTable(conn, protocolIdsAddedMaterializedName)
  apiProtocolIdsAlreadyExisted <- dbExistsTable(conn, protocolIdsMaterializedName)
  apiProtocolsAlreadyExisted <- dbExistsTable(conn, protocolsMaterializedName)
  
  currentProtocolIdsSQL <- "SELECT /*+ FIRST_ROWS(1) */  p.id AS protocol_id, p.version as version
  FROM protocol p
  JOIN protocol_label pl ON p.id=pl.protocol_id
  LEFT JOIN protocol_state ps ON p.id = ps.protocol_id AND ps.ls_kind = 'name modifier'
  LEFT JOIN protocol_value pv ON ps.id = pv.protocol_state_id AND pv.ls_kind = 'postfix'
  WHERE p.ignored ='0'
  AND pl.preferred='1'
  AND pl.ignored  ='0' GROUP BY p.id, p.version"
  
  if(apiProtocolIdsRemovedAlreadyExisted & update == TRUE) {
    if(apiProtocolIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT INTO ",protocolIdsRemovedMaterializedName,"
                                          ( protocol_id,
                                          version
                                          )
                                          SELECT *
                                          FROM ",protocolIdsMaterializedName,"
                                          WHERE NOT EXISTS
                                          (SELECT *
                                          FROM
                                          (",
                                          currentProtocolIdsSQL,
                                          ") a
                                          WHERE ",protocolIdsMaterializedName,".protocol_id = a. protocol_id
                                          AND ",protocolIdsMaterializedName,".version = a. version
                                          )"))
    }
    } else {
      if(apiProtocolIdsRemovedAlreadyExisted) {
        logger$info(paste0(protocolIdsRemovedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",protocolIdsRemovedMaterializedName))
      }
      apiProtocolIdsRemovedCreated <- dbSendQuery(conn, paste0("create global temporary table ",protocolIdsRemovedMaterializedName," (protocol_id NUMBER(19), version NUMBER(19)) on commit delete rows"))    
    }
  if(apiProtocolIdsAddedAlreadyExisted & update == TRUE) {
    if(apiProtocolIdsAlreadyExisted) {
      updated <- dbSendQuery(conn, paste0("INSERT
                                          INTO ",protocolIdsAddedMaterializedName,"
                                          (
                                          protocol_id,
                                          version
                                          )
                                          ( SELECT a.protocol_id, a.version
                                          FROM (",currentProtocolIdsSQL,") a
                                          WHERE NOT EXISTS
                                          (SELECT *
                                          FROM ",protocolIdsMaterializedName," b
                                          WHERE b.protocol_id = a.protocol_id
                                          AND b.version = a.version
                                          ))"))
    }
    } else {
      if(apiProtocolIdsAddedAlreadyExisted) {
        logger$info(paste0(protocolIdsAddedMaterializedName, " already exists, dropping"))            
        dbSendQuery(conn, paste0("drop table ",protocolIdsAddedMaterializedName))
      }
      apiCurveIdsAddedCreated <- dbSendQuery(conn, paste0("create global temporary table ",protocolIdsAddedMaterializedName," (protocol_id NUMBER(19), version NUMBER(19)) on commit delete rows"))    
    }
  #Protocol Ids
  if(apiProtocolIdsAlreadyExisted & update == TRUE) {
    
    logger$info(paste0("updating ",protocolIdsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",protocolIdsMaterializedName,"
                                             WHERE (protocol_id, version) in (
                                             select protocol_id, version from ",protocolIdsRemovedMaterializedName,"
                                             )"))
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",protocolIdsMaterializedName,"
                                            (
                                            protocol_id,
                                            version
                                            )
                                            ( SELECT a.protocol_id, a.version
                                            FROM (",protocolIdsAddedMaterializedName,") a
                                            )"))
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
    
  } else {
    if(apiProtocolIdsAlreadyExisted) {
      logger$info(paste0(protocolIdsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",protocolIdsMaterializedName))
    }
    logger$info(paste0("creating ",protocolIdsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",protocolIdsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",currentProtocolIdsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key protocol_id"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",protocolIdsMaterializedName," ADD PRIMARY KEY (protocol_id) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
    logger$info(paste0("adding version index"))  
    indexSQL <- paste0("CREATE INDEX IDX_PROTOCOL_IDS_M_VERSION ON ",protocolIdsMaterializedName," (version)",ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",indexSQL))
    versionIndex <- dbSendQuery(conn,indexSQL)
    
  }
  #Protocols
  selectProtocolsSQL <- "SELECT 
                                            'ACAS' AS DB_MAP,
                                            api_protocol.PROTOCOL_ID,
                                            api_protocol.LABEL_TEXT AS PROTOCOL_NAME,
                                            api_protocol.LABEL_TEXT || '_Screening' AS PROTOCOL_NAME_HTS,
                                            api_protocol.VERSION
                                            FROM api_protocol"
  if(apiProtocolsAlreadyExisted & update == TRUE) {
    logger$info(paste0("updating ",protocolsMaterializedName))
    removed_data <- dbSendQuery(conn, paste0("DELETE FROM ",protocolsMaterializedName,"
                                             WHERE (protocol_id, version) in (
                                             select protocol_id, version from ",protocolIdsRemovedMaterializedName,"
                                             )"))
    
    logger$info(paste0("removed ",dbGetInfo(removed_data)$rowsAffected, " rows"))  
    missingData <- dbSendQuery(conn, paste0("INSERT
                                            INTO ",protocolsMaterializedName,"
                                            (
                                            DB_MAP,
                                            PROTOCOL_ID,
                                            PROTOCOL_NAME,
                                            PROTOCOL_NAME_HTS,
                                            VERSION
                                            )
                                            (",selectProtocolsSQL,"
                                            WHERE (protocol_id, version) in 
                                            ( SELECT a.protocol_id, a.version
                                            FROM (",protocolIdsAddedMaterializedName,") a
                                            ))"))
    
    logger$info(paste0("added ",dbGetInfo(missingData)$rowsAffected, " rows"))
  } else {
    
    if(apiProtocolsAlreadyExisted) {
      logger$info(paste0(protocolsMaterializedName, " already exists, dropping"))      
      dbSendQuery(conn, paste0("DROP table ",protocolsMaterializedName))
    }
    logger$info(paste0("creating ",protocolsMaterializedName))          
    createTableSQL <- paste0("CREATE table ",protocolsMaterializedName," ",ifelse(is.na(createTableOptions),"",createTableOptions), " as ",selectProtocolsSQL)
    logger$debug(paste0("executing sql ",createTableSQL))
    finished <- dbSendQuery(conn, createTableSQL)
    
    logger$info(paste0("adding primary key protocol_id"))  
    primaryKeySQL <- paste0(" ALTER TABLE ",protocolsMaterializedName," ADD PRIMARY KEY (protocol_id) USING INDEX ", ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",primaryKeySQL))
    finished <- dbSendQuery(conn, primaryKeySQL)
    
    logger$info(paste0("adding version index"))  
    indexSQL <- paste0("CREATE INDEX IDX_API_PROTOCOL_M_VERSION ON ",protocolsMaterializedName," (version)",ifelse(is.na(createIndexOptions),"",createIndexOptions))
    logger$debug(paste0("executing sql ",indexSQL))
    curveidIndex <- dbSendQuery(conn,indexSQL)
    
    logger$info(paste0("adding index IDX_API_PROTOCOL_M_NAME"))
    protocolNameIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_PROTOCOL_M_NAME ON ",protocolsMaterializedName," (protocol_name)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("adding index IDX_API_PROTOCOL_M_DB_MAP"))
    protocolNameIndex <- dbSendQuery(conn,paste0("CREATE INDEX IDX_API_PROTOCOL_M_DB_MAP ON ",protocolsMaterializedName," (db_map)",ifelse(is.na(createIndexOptions),"",createIndexOptions)))
    
    logger$info(paste0("granting SELECT to SEURAT user"))
    granted <- dbSendQuery(conn,paste0("GRANT SELECT ON ",protocolsMaterializedName," TO SEURAT WITH GRANT OPTION"))
    
  }
  
  logger$info(paste0("commiting transaction"))        
  commited <- dbCommit(conn)
  on.exit(dbDisconnect(conn))
  logger$info(paste0("materialization complete"))            
}