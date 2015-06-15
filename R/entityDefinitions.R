query_definition_list_to_sql <- function(queryDefinitionList, dbType = NA) {
  if(is.na(dbType)) {
    dbType <- getDBType()
  }
  getSelect <- function(table,field,alias) {
    if(length(field) > 1) {
      select <- switch(dbType,
                       "Postgres" = paste0("COALESCE(",paste0(gsub(" ","_",table),".",field, collapse = ","),") AS \"", alias,"\""),
                       "Oracle" = paste0("COALESCE(",paste0("TO_CHAR(",gsub(" ","_",table),".",field,")", collapse = ","),") AS \"", alias,"\""),
      )
    } else {
      select <- paste0(gsub(" ","_",table),".",field," AS \"", alias,"\"")
    }
    return(select)
  }
  getSelects <- function(x) {
    selects <- rbindlist(do.call(c, lapply(x, function(x) lapply(x[["select"]], function(x,parentName) {x$parentName <- parentName;x}, parentName = x$name))), fill = TRUE)
    if(nrow(selects) != 0) {
      if(dbType == "Postgres") {
        selects[field!="clob_value", table := parentName]
        selects[field=="clob_value", c('table','field') := list(paste0('convert_from(loread(lo_open(',name),paste0(field,"::int, x'40000'::int), x'40000'::int),\'SQL_ASCII\')"))]
        selects[ , field := ifelse(.N > 1, paste0(field,"::text"),field), by = name]
      } else {
        selects[, table := parentName]
      }
      valueSelects <- selects[ , getSelect(table, field, name), by = name]$V1
      valueSelects <- paste0(valueSelects,collapse = ",\n")
      valueSelects <- paste0(valueSelects,   collapse = ",\n")
    } else {
      valueSelects <- NULL
    }
    return(valueSelects)
  }
  
  if("experiment" %in% names(queryDefinitionList$entryPoint)) {
    protocolToExperimentJoin <- paste0("experiment e\nINNER JOIN experiment_analysisgroup eag ON eag.experiment_id=e.id")
  } else {
    protocolToExperimentJoin <- ""
  }
  
  groups <- queryDefinitionList[["analysis_group"]]
  groupSelects <- paste0(unlist(lapply(groups[[1]]$select,function(x) getSelect(groups[[1]]$name,x$field,x$name))),collapse = ",\n")
  if(groupSelects == "") {
    groupSelects <- NULL
  } else {
    groupSelects[groupSelects==""] <- NULL
    groupSelects <- paste0(groupSelects,   collapse = ",\n")
  }

  groups$parentName <- "eag"
  if(protocolToExperimentJoin != "") {
    groupJoins <- paste0("LEFT OUTER JOIN analysis_group ",gsub(" ","_",groups[[1]]$name)," \nON ",gsub(" ","_",groups$parentName),".analysis_group_id = ",gsub(" ","_",groups[[1]]$name),".id")
  } else {
    groupJoins <- paste0("analysis_group ",gsub(" ","_",groups[[1]]$name))
  }
  
  
  if(!is.null(groups$where)) {
    groupWhere <- paste0(groups$name,".",names(groups$where),"=",groups$where)
  } else {
    groupWhere <- ""
  }
  groupWhere <- groupWhere[groupWhere!=""]
  if(length(groupWhere) != 0 && groupWhere != "")  {
    groupWhere <- paste0(groupWhere, collapse = " and\n")
  }
  
  states <- do.call(c, lapply(queryDefinitionList$analysis_group, function(x) lapply(x[["analysis_group_state"]], function(x,parentName) {x$parentName <- parentName;x}, parentName = x$name)))
  stateSelects <- getSelects(states)

  stateJoins <- lapply(states, function(x) {
    joins <- paste0("LEFT OUTER JOIN analysis_group_state ",gsub(" ","_",x$name)," \nON ",gsub(" ","_",x$parentName),".id = ",gsub(" ","_",x$name),".analysis_group_id \n","AND (",gsub(" ","_",x$name),".ls_kind='",x$ls_kind,"' AND ",gsub(" ","_",x$name),".ls_type='",x$ls_type,"')")
    return(joins)
  })
  stateJoins <- paste0(stateJoins,collapse = " \n")
  
  stateWhere <- unlist(lapply(states, function(x) {
    if(!is.null(x$where)) {
      return(paste0(x$name,".",names(x$where),"=",x$where))
    } else {
      return("")
    }
  }))
  stateWhere <- stateWhere[stateWhere!=""]
  if(length(stateWhere) > 0 && stateWhere != "")  {
    stateWhere <- paste0(stateWhere, collapse = " and\n")
  }

  values <- do.call(c, lapply(states, function(x) lapply(x[["analysis_group_value"]], function(x,parentName) {x$parentName <- parentName;x}, parentName = x$name)))
  valueSelects <- getSelects(values)

  valueWhere <- unlist(lapply(values, function(x) {
    if(!is.null(x$where)) {
      return(paste0(x$name,".",names(x$where),"=",x$where))
    } else {
      return("")
    }
  }))
  valueWhere <- valueWhere[valueWhere!=""]
  if(length(valueWhere) > 0 && valueWhere != "")  {
    valueWhere <- paste0(valueWhere, collapse = " and\n")
  }
  
  valueJoins <- lapply(values, function(x) {
    joins <- paste0("LEFT OUTER JOIN analysis_group_value ",gsub(" ","_",x$name)," \nON ",gsub(" ","_",x$parentName),".id = ",gsub(" ","_",x$name),".analysis_state_id \n","AND (",gsub(" ","_",x$name),".ls_kind='",x$ls_kind,"')")
    return(joins)
  })
  valueJoins <- paste0(valueJoins,collapse = " \n")
  entryWherePointClause <- paste0(queryDefinitionList$entryPoint[[length(queryDefinitionList$entryPoint)-1]],".",queryDefinitionList$entryPoint$field)
  whereClause <- c(groupWhere, stateWhere, valueWhere, entryWherePointClause)
  whereClause <- paste0(whereClause, collapse = " and \n")
  selectsClause <- c(groupSelects, stateSelects, valueSelects)
  selectsClause <- paste0(selectsClause, collapse = ",\n")
  sql <- paste0("SELECT ",selectsClause,"\nFROM ",protocolToExperimentJoin, "\n", groupJoins, "\n",stateJoins, "\n",valueJoins, " \nWHERE ", whereClause, " in (REPLACEME)" ,collapse = "\n")
  return(sql)
}
read_json_file <- function(jsonFile) {
  jsonCharacter <- readChar(jsonFile, file.info(jsonFile)$size)
  jsonList <- fromJSON(jsonCharacter)
  return(jsonList)
}

get_fit_data_curve_id2 <- function(curveids, full_object = TRUE, ...) {
  renderingHint <- get_curve_id_rendering_hint(curveids, ...)
  modelFit <- racas::get_model_fit_from_type_code(renderingHint)
  qu <- modelFit$curveid_query
  fitData <- curve_fit_controller_fitData_dataTable_to_fitData(rbindlist(query_replace_string_with_values(qu, "REPLACEME", curveids, ...)))
  setkey(fitData,"curveId")
  if(full_object) {
    curveFitController_rawDataResponse <- curve_fit_controller_getRawDataByCurveId(curveids)
    rawData <- curve_fit_controller_rawData_response_to_data_table(curveFitController_rawDataResponse)
    rawData[ ,tempFlagStatus := ""]
    rawData[ , flagchanged := FALSE]
    rawData <- rawData[ , list(list(.SD)), .SDcols = 1:ncol(rawData), keyby = "curveId"]
    setnames(rawData, "V1", "points")
    fitData <- fitData[rawData]
  }
  return(fitData)
}

get_curve_id_state_id <- function(curveid, ...) {
  qu <- paste0("SELECT analysis_state_id
  FROM analysis_group_value agv
  INNER JOIN analysis_group_state ags
  ON agv.analysis_state_id=ags.id
  WHERE agv.ls_type       = 'stringValue'
  AND agv.ls_kind         = 'curve id'
  AND agv.string_value = ",sqliz(curveid),"
  AND agv.ignored = '0'
  AND ags.ignored = '0'
  AND ags.ignored = '0'
  AND ags.ls_type = 'data'
  AND ags.ls_kind = 'dose response'")
  query(qu, ...)[[1]]
}
get_curve_id_rendering_hint <- function(curveid, ...) {
  state_id <- get_curve_id_state_id(curveid, ...)
  qu <- paste0("select string_value from analysis_group_value where ls_kind = 'Rendering Hint' and analysis_state_id = ", state_id)
  query(qu, ...)[[1]]
}
get_fit_data_experiment_code2 <- function(experimentCode, modelFitType, full_object = FALSE, modelFit,...) {
  myMessenger <- messenger()
  myMessenger$logger$debug("getting fitData2")
  qu <- modelFit$experiment_query
  queryResults <- rbindlist(query_replace_string_with_values(qu, "REPLACEME", experimentCode, ...))
  if(nrow(queryResults) == 0) {
    msg <- "no experiment results found"
    myMessenger$logger$error(msg)
    stop(msg)
  }
  myMessenger$logger$debug("converting service return to fit_data object") 
  fitData <- curve_fit_controller_fitData_dataTable_to_fitData(queryResults)
  #Treatmeng Groups and Subject Groups
  setkey(fitData, "curveId")
  if(full_object) {
    myMessenger$logger$debug("getting rawData")
    curveFitController_rawDataResponse <- curve_fit_controller_getRawDataByExperimentIdOrCodeName(experimentCode)
    rawData <- curve_fit_controller_rawData_response_to_data_table(curveFitController_rawDataResponse)
    rawData[ ,tempFlagStatus := ""]
    rawData[ ,flagchanged := FALSE]
    rawData <- rawData[ , list(list(.SD)), keyby = "curveId"]
    setnames(rawData, "V1", "points")
    fitData <- fitData[rawData]
  }
  myMessenger$logger$debug(paste0("returning with ", nrow(fitData), " curves"))
  return(fitData)
}
# conn <- getDatabaseConnection()
# fitData <- get_fit_data_curve_id2(curveids, full_object = TRUE, query = qu, conn = conn)
# 
# system.time(get_fit_data_curve_id(curveids))