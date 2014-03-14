

getAnalysisGroupValues <- function(reportedParameters, fixedParameters, fittedParameters, goodnessOfFit.model, category, approved, tested_lot, recordedBy, lsTransaction, doseUnits, responseUnits, analysisGroupCode, renderingHint) {
  fitParameters <- c(fixedParameters,fittedParameters)
  names(fitParameters) <- paste0("fitted_",names(fitParameters))
  reportedParameters[unlist(lapply(reportedParameters, function(x) is.NULLorNA(x$value)))] <- NULL
  publicAnalysisGroupValues <- c(reportedParameters, list(tested_lot = list(value = tested_lot, operator = NULL), curveid = list(value = paste0(analysisGroupCode,"_", lsTransaction), operator = NULL, stdErr = NULL)))
  privateAnalysisGroupValues <- c(fitParameters, goodnessOfFit.model, list('Rendering Hint' = renderingHint), c(list(category = category), list(flag = "algorithm")[!approved]))
  privateAnalysisGroupValues[unlist(lapply(privateAnalysisGroupValues, is.NULLorNA))] <- NULL
  privateAnalysisGroupValues <- lapply(privateAnalysisGroupValues, function(x) list(value = x, operator = NULL, stdErr = NULL))
  
  x <- c(publicAnalysisGroupValues,privateAnalysisGroupValues)   
  public <- c(rep(TRUE, length(publicAnalysisGroupValues)), rep(FALSE, length(privateAnalysisGroupValues)))
  lsTypes <- unlist(lapply(x, function(x) ifelse(class(x$value)=="numeric", "numericValue", "stringValue")))
  lsTypes[names(lsTypes) == "tested_lot"] <- "codeValue"
  valueUnits <- rep(list(NULL),length(lsTypes))
  valueUnits[names(lsTypes) %in% c("min", "max","fitted_min", "fitted_max")] <- responseUnits
  valueUnits[names(lsTypes) %in% c("ec50", "fitted_ec50")] <- doseUnits
  kindMap <- list(min = "Min" ,
                  max = "Max" ,
                  slope = "Slope" ,
                  ec50 = "EC50" ,
                  tested_lot = "batch code" ,
                  curveid = "curve id" ,
                  fitted_slope = "Fitted Slope",
                  fitted_min = "Fitted Min" ,
                  fitted_max = "Fitted Max" ,
                  fitted_ec50 = "Fitted EC50" ,
                  SSE = "SSE" ,
                  SST = "SST" ,
                  rSquared = "rSquared" ,
                  category = "category" ,
                  flag = "flag")
  matches <- match(names(kindMap), names(x))
  names(x)[matches[!is.na(matches)]] <- kindMap[which(!is.na(matches))]
  lsKinds <- names(x)
  stringValues <- ifelse(lsTypes=="stringValue", lapply(x, function(x) x$value), list(NULL))
  codeValues <- ifelse(lsTypes=="codeValue", lapply(x, function(x) x$value), list(NULL))
  numericValues <- ifelse(lsTypes=="numericValue", lapply(x, function(x) x$value), list(NULL))
  operatorValues <- lapply(x, function(x) x$operator)
  uncertanties <- lapply(x, function(x) x$stdErr)
  uncertantyTypes <- uncertanties
  uncertantyTypes[!unlist(lapply(uncertanties,is.null))] <- "Standard Error"
  analysisGroupValues <- lapply(1:length(x), 
                                function(x) {
                                  createStateValue(lsType = lsTypes[[x]],
                                                   lsKind = lsKinds[[x]],
                                                   valueUnit = valueUnits[[x]],
                                                   stringValue = stringValues[[x]],
                                                   numericValue = numericValues[[x]],
                                                   valueOperator = operatorValues[[x]],
                                                   uncertaintyType = uncertantyTypes[[x]],
                                                   uncertainty = uncertanties[[x]],
                                                   codeValue = codeValues[[x]],
                                                   publicData = public[[x]],
                                                   lsTransaction=lsTransaction,
                                                   recordedBy = as.character(recordedBy))
                                }
  )
  return(list(analysisGroupCode = analysisGroupCode, analysisGroupValues = analysisGroupValues))
}

saveDoseResponseData <- function(fitData, recordedBy, experimentCode = NULL) {
  
  lsTransaction <- createLsTransaction()$id
  fitData[ , analysisGroupValues := list(list(getAnalysisGroupValues(reportedParameters[[1]],
                                                                                 fixedParameters[[1]],
                                                                                 fittedParameters[[1]],
                                                                                 goodnessOfFit.model[[1]],
                                                                                 category,
                                                                                 approved,
                                                                                 tested_lot,
                                                                                 recordedBy,
                                                                                 lsTransaction,
                                                                                 doseUnits = as.character(points[[1]][1]$doseUnits), 
                                                                                 responseUnits = as.character(points[[1]][1]$responseUnits), 
                                                                                 analysisGroupCode = as.character(parameters[[1]]$ag_code_name), 
                                                                                 as.character(parameters[[1]]$renderingHint)))), by = curveid]
  if(!is.null(experimentCode)) {
    savedStates <- fitData[,  list(saveDoseResponseCurve(recordedBy, analysisGroupValues, points = points, lsTransaction, experimentCode = experimentCode))][[1]]
  } else {
    if("parameters" %in% names(fitData)) {
      savedStates <- fitData[,  list(saveDoseResponseCurve(recordedBy, analysisGroupValues, points, lsTransaction, ignoreDoseResponseState.analysisGroupIds = rbindlist(fitData$parameters)$ag_id))]
    } else {
      savedStates <- fitData[,  list(saveDoseResponseCurve(recordedBy, analysisGroupValues, points, lsTransaction))]
    }
  }
  return(list(lsStates = savedStates, lsTransaction = lsTransaction))
  
} # model.R in curve branch has data.table examples

saveDoseResponseCurve <- function(recordedBy, analysisGroupValues, points, lsTransaction, experimentCode = NULL, ignoreDoseResponseState.analysisGroupIds = NULL) {
  #analysisGroupValues <- fitData$analysisGroupValues
  #points <- fitData$points
  #experimentCode <- experimentCode
  #recordedBy <- "bbolt"
  
  #First ignore the analysis group states for the analysis group values we are going to update
  #If experimentCode is provided, we are ignoring all Dose Response states and then adding new ones
  if(!is.null(experimentCode)) {
    experiment <- getURL(paste0(racas::applicationSettings$client.service.persistence.fullpath, "experiments/codename/", experimentCode, "?with=analysisgroupvalues"))
    experiment <- fromJSON(experiment)[[1]]
    analysisGroups <- experiment$analysisGroups  
  } else {
    #Else if no experiment code is provided, then we are only ignoring Dose Response states for ag_ids that are provided
    if(is.null(ignoreDoseResponseState.analysisGroupIds)) {
      analysisGroups <- list()
    } else {
      analysisGroups <- lapply(ignoreDoseResponseState.analysisGroupIds, getEntityById, "analysisgroups")
    }
  }
  analysisGroupStatesToIgnore <- getLSStateFromEntity(analysisGroups, lsType = "data", lsKind = "Dose Response", ignored = "FALSE")
  ignoredAnalysisGroupStates <- lapply(analysisGroupStatesToIgnore, function(x) {
    x$ignored <- TRUE
    updateAcasEntity(x, "analysisgroupstates")
  })
  #need to add an analysis group value curve id!
  matches <- match(unlist(lapply(analysisGroupValues, function(x) x$analysisGroupCode)),unlist(lapply(analysisGroups,function(x) x$codeName)))
  analysisGroupStates <- lapply(1:length(analysisGroups),
                                    function(x) {
                                      state <- createAnalysisGroupState(
                                        analysisGroup = list(id =  analysisGroups[[x]]$id, version = analysisGroups[[x]]$version),
                                        analysisGroupValues = analysisGroupValues[[matches[[x]]]]$analysisGroupValues,
                                        recordedBy = recordedBy,
                                        lsType = "data",
                                        lsKind = "Dose Response",
                                        lsTransaction = lsTransaction
                                      )
                                      return(state)
                                    }
                                    
  )
  savedAnalysisGroupStates <- saveAcasEntities(analysisGroupStates, "analysisgroupstates")
  return(savedAnalysisGroupStates)
}

updateDoseResponseCurve <- function(fitData) {
  # This takes one curve
  
  # Need flag_value_id in points
  # Need flag to be a value or NA
  # Need response_ss_version
  
  # This will ignore and replace states that are there
  # Save new, ignore old
  # If passed analysisGroupId, it is already saved
  # will be passing flags to update
  # Idea: one for analysis groups, one for flags
  # Need to recalculate treatment groups
  # Ignore states for whole object? just states
  # TODO: deal with empty 
  
  fitData[, updatePointFlags(points[[1]]), by = curveid]
  
  fitData[, saveNewAnalysisGroupData(parameters[[1]])]
}

getLSStateFromEntity <- function(entities, ...) {
  #listCriteria <<- list(...)
  unlistEntities <- unlist(entities, recursive = FALSE)
  lsStatesList <- unlistEntities[names(unlistEntities) == "lsStates"]
  lsStates <- do.call("c", lsStatesList)
  matchListCriteria <- function(lsState, listCriteria) {
    #lsState <- lsStates[[1]]
    unlistedLSState <- unlist(lsState)
    matchCriteria <- function(unlistedLSState, criteria) {
      any(names(unlistedLSState) == names(criteria) & unlistedLSState == criteria[[1]])
    }
    return(all(unlist(lapply(1:length(listCriteria), function(x) matchCriteria(unlistedLSState,listCriteria[x])))))
  }
  matches <- unlist(lapply(lsStates, matchListCriteria, listCriteria = list(...)))
  #matches <- unlist(lapply(lsStates, matchListCriteria, listCriteria = listCriteria))
  lsStates[!matches] <- NULL
  return(lsStates)
}

saveNewAnalysisGroupData <- function(parameters) {
  parameters <- cbind(rbindlist(fitData$analysisGroupValues),ag_id = rbindlist(fitData$parameters)$ag_id)
  
  lsTransaction <- createLsTransaction()$id
  analysisGroups <- lapply(parameters$ag_id, getEntityById, "analysisgroups")
  analysisGroupStatesToIgnore <- getLSStateFromEntity(analysisGroups, lsType = "data", lsKind = "Dose Response", ignored = "FALSE")
  ignoredAnalysisGroupStates <- lapply(analysisGroupStatesToIgnore, function(x) {
    x$ignored <- TRUE
    updateAcasEntity(x, "analysisgroupstates")
  })
  #analysisGroupsStates lapply(analysisGroups, function(x) x$)
  analysisGroupStateGroups <- list(list(entityKind = "analysisgroups",
                                        stateType = "data", 
                                        stateKind = "Dose Response",
                                        includesOthers = TRUE,
                                        includesCorpName = TRUE))
  
  analysisGroupData$stateGroupIndex <- 1
  analysisGroupData$publicData <- TRUE
  
  analysisGroupsToIgnore <- unique(analysisGroupData$analysisGroupID)
  
  sqlAnalysisGroupIds <- paste(analysisGroupData$analysisGroupID, collapse = ",")
  query(paste0(
    "UPDATE analysis_group_state
    SET ignored               = 1,
    version                   = version+1
    WHERE state_type_and_kind = 'data_Dose Response'
    AND analysis_group_id     IN (", sqlAnalysisGroupIds, ")"))
  query(paste0(
    "UPDATE analysis_group_value
    SET ignored               = 1,
    version                   = version+1
    WHERE analysis_state_id    IN
    (SELECT id
    FROM analysis_group_state
    WHERE state_type_and_kind = 'data_Dose Response'
    AND analysis_group_id IN (", sqlAnalysisGroupIds, "))"))
  
  analysisGroupData$stateID <- saveStatesFromLongFormat(entityData = analysisGroupData, entityKind = "analysisgroup", 
                                                        stateGroups=analysisGroupStateGroups, stateGroupIndices = 1, 
                                                        idColumn = "analysisGroupID", recordedBy = "curveCuration", lsTransaction = lsTransaction)[['entityStateId']]
  analysisGroupData$stateVersion <- 0
  analysisGroupData <- rbind.fill(analysisGroupData, meltBatchCodes(analysisGroupData, 1))
  saveValuesFromLongFormat(entityData = analysisGroupData, entityKind = "analysisgroup", 
                           stateGroups = analysisGroupStateGroups, stateGroupIndices = 1,
                           lsTransaction = lsTransaction)
  
}
updatePointFlags <- function(pointData, recordedBy) {
  library(RCurl)
  library(rjson)
  
  lsTransaction <- createLsTransaction()$id
  updateTheseFlags <- pointData[!is.na(pointData$flag)]
  changedFlags <- pointData[!is.na(flag_sv_id), list(flag_sv_id, response_ss_id, response_ss_version, tg_id, flag)]
  flagsToIgnore <- lapply(changedFlags$flag_sv_id, getEntityById, "subjectvalues")
  ignoredFlags <- lapply(flagsToIgnore, function(x) {
    x$ignored <- TRUE
    updateAcasEntity(x, "subjectvalues")
  })
  newFlags <- updateTheseFlags[, list(list(createStateValue(lsType = "stringValue", lsKind = "flag", stringValue = flag, lsTransaction=lsTransaction,recordedBy=recordedBy, lsState=list(id=response_ss_id, version=response_ss_version)))), by = response_sv_id]$V1
  saveAcasEntities(newFlags, "subjectvalues")
  
  #Treatment group value updates
  update_tg_id <- unique(changedFlags$tg_id)
  updateTheseValues <- pointData[pointData$tg_id %in% update_tg_id]
  treatmentGroups <- lapply(update_tg_id, getEntityById, "treatmentgroups")
  treatmentGroupDF <- ldply(treatmentGroups, flattenEntity, acasCategory= "treatmentGroup", includeFromState = c("id", "lsType", "lsKind", "version"))
  valuesToIgnoreDF <- treatmentGroupDF[treatmentGroupDF$lsKind == "Response", ]
  valuesToIgnore <- lapply(valuesToIgnoreDF$id, getEntityById, "treatmentgroupvalues")
  ignoredValues <- lapply(valuesToIgnore, function(x) {
    x$ignored <- T
    updateAcasEntity(x, "treatmentgroupvalues")
  })
  updateTheseValues$tgs_id <- treatmentGroupDF$stateId[match(updateTheseValues$tg_id, treatmentGroupDF$treatmentGroupId)]
  updateTheseValues$tgs_version <- treatmentGroupDF$stateVersion[match(updateTheseValues$tg_id, treatmentGroupDF$treatmentGroupId)]
  #updateTheseValues$tgv_id <- treatmentGroupDF$id[match(updateTheseValues$tg_id, valuesToIgnoreDF$TreatmentGroupId)]
  
  newValues <- updateTheseValues[, list(list(createStateValue(lsType = "numericValue",
                                                              lsKind = "Response", 
                                                              numericValue = NAtoNULL(suppressWarnings(mean(response))), 
                                                              numberOfReplicates=length(response), 
                                                              uncertaintyType="standard deviation", 
                                                              uncertainty = NAtoNULL(sd(response)), 
                                                              lsTransaction=lsTransaction,
                                                              recordedBy=recordedBy, 
                                                              lsState=list(id=unique(tgs_id), 
                                                                           version=unique(tgs_version))))), 
                                 by = tg_id]$V1
  saveAcasEntities(newValues, "treatmentgroupvalues")
}

getEntityById <- function(id, acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(paste0(racas::applicationSettings$client.service.persistence.fullpath, acasCategory, "/", id))
  if (grepl("^<", response)) {
    myLogger <- createLogger(logName = "com.acas.sel", logFileName = "racas.log")
    myLogger$error(response)
    stop(paste0("The loader was unable to find ", acasCategory, 
                " ", id, ". Check the logs at ", Sys.time()))
  }
  response <- fromJSON(response)
  return(response)
}
is.NULLorNA <- function(value) {
  if(is.null(value)) return(TRUE)
  return(is.na(value))
}
NAtoNULL <- function(x) {
  if(is.NULLorNA(x)) return(NULL)
  return(x)
}