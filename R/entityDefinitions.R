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
  getLogicalValue <- function(x) {
    if(class(x) == "logical") {
      if(x) {
        x <- switch(dbType,
                    "Postgres" = "1",
                    "Oracle" = 1)
      } else {
        x <- switch(dbType,
                    "Postgres" = "0",
                    "Oracle" = 0)          
      }
    }
    return(x)
  }
  getWhere <- function(table) {
    where <- lapply(table$where, function(x) {
      if(class(x) == "logical") {
        x <- getLogicalValue(x)
      }
      return(sqliz(x))
    })
    where <- paste0(table$name,".",names(where),"=",(where))
    return(where)
  }
  getSelects <- function(x) {
    selects <- rbindlist(do.call(c, lapply(x, function(x) lapply(x[["select"]], function(x,parentName) {x$parentName <- parentName;x}, parentName = x$name))), fill = TRUE)
    if(nrow(selects) != 0) {
      if(dbType == "Postgres") {
        selects[ , field := ifelse(.N > 1, paste0(field,"::text"),field), by = name]
      }
      selects[, table := parentName]

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
  
  if(!is.null(groups[[1]]$where)) {
    groupWhere <- getWhere(groups[[1]])
  } else {
    groupWhere <- ""
  }
 
  states <- do.call(c, lapply(queryDefinitionList$analysis_group, function(x) lapply(x[["analysis_group_state"]], function(x,parentName) {x$parentName <- parentName;x}, parentName = x$name)))
  stateSelects <- getSelects(states)

  stateJoins <- lapply(states, function(x) {
    joins <- paste0("LEFT OUTER JOIN analysis_group_state ",gsub(" ","_",x$name)," \nON ",gsub(" ","_",x$parentName),".id = ",gsub(" ","_",x$name),".analysis_group_id \n","AND (",gsub(" ","_",x$name),".ls_kind='",x$ls_kind,"' AND ",gsub(" ","_",x$name),".ls_type='",x$ls_type,"'",ifelse(is.null(x$ignored),"",paste0(" AND ",gsub(" ","_",x$name),".ignored=",getLogicalValue(x$ignored))),")")
    return(joins)
  })
  stateJoins <- paste0(stateJoins,collapse = " \n")
  
  stateWhere <- unlist(lapply(states, function(x) {
    if(!is.null(x$where)) {
      return(getWhere(x))
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
      return(getWhere(x))
    } else {
      return("")
    }
  }))
  valueWhere <- valueWhere[valueWhere!=""]
  if(length(valueWhere) > 0 && valueWhere != "")  {
    valueWhere <- paste0(valueWhere, collapse = " and\n")
  }
  
  valueJoins <- lapply(values, function(x) {
    alias <- gsub(" ","_",x$name)
    joins <- paste0(
      "LEFT OUTER JOIN analysis_group_value ", alias," \nON ",
      gsub(" ","_",x$parentName),".id = ", alias, ".analysis_state_id \n",
      "AND (", 
        alias,".ls_kind='", x$ls_kind,
        "' AND ",
        alias,".ignored=", sqliz(getLogicalValue(FALSE)),
      ")"
    )
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
entityDefinitionToQueriesAndTypeMap <- function(queryDefinition, dbType) {
  curveQueryDefinition <- queryDefinition
  experimentQueryDefinition <- queryDefinition
  curveQueryDefinition$entryPoint <- list(analysis_group = "ag", analysis_group_state = "ags1",analysis_group_value = "curveId", field = "string_value")
  experimentQueryDefinition$entryPoint <- list(experiment = "e", field = "code_name")
  states <- Reduce(function(x,y) rbind(x,y, fill = TRUE, use.names = TRUE), lapply(queryDefinition$analysis_group[[1]]$analysis_group_state, as.data.table))
  values <- flatten_list_in_data_table(states, "analysis_group_value", c("ls_type", "ls_kind"), c("state_type", "state_kind"))
  typeMap <- flatten_list_in_data_table(values, "select", c("state_type", "state_kind","ls_kind"))
  typeMap[field == 'clob_value', lsType := 'clobValue']
  typeMap[field == 'string_value', lsType := 'stringValue']
  typeMap[field == 'code_value', lsType := 'codeValue']
  typeMap[field == 'numeric_value', lsType := 'numericValue']
  
  curveSQL <- query_definition_list_to_sql(curveQueryDefinition, dbType = dbType)
  experimentSQL <- query_definition_list_to_sql(experimentQueryDefinition, dbType = dbType)
  return(list(typeMap = typeMap, curveSQL = curveSQL, experimentSQL = experimentSQL))
}
