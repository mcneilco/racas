

getAnalysisGroupValues <- function(reportedParameters, fixedParameters, fittedParameters, goodnessOfFit.model, category, approved, tested_lot, recordedBy, lsTransaction, doseUnits, responseUnits, analysisGroupCode, renderingHint, reportedValuesClob, fitSummaryClob, parameterStdErrorsClob, curveErrorsClob, simpleFitSettings) {
  fitParameters <- c(fixedParameters,fittedParameters)
  names(fitParameters) <- paste0("fitted_",names(fitParameters))
  reportedParameters[unlist(lapply(reportedParameters, function(x) is.NULLorNA(x$value)))] <- NULL
  publicAnalysisGroupValues <- c(reportedParameters, list(tested_lot = list(value = tested_lot, operator = NULL), curveid = list(value = paste0(analysisGroupCode,"_", lsTransaction), operator = NULL, stdErr = NULL)))
  privateAnalysisGroupValues <- c(fitParameters, goodnessOfFit.model, list('Rendering Hint' = renderingHint), c(list(category = category), list(flag = "algorithm")[!approved], list(reportedValuesClob = reportedValuesClob), list(fitSummaryClob = fitSummaryClob), list(parameterStdErrorsClob = parameterStdErrorsClob), list(curveErrorsClob = curveErrorsClob),  list(simpleFitSettings = simpleFitSettings)))
  privateAnalysisGroupValues[unlist(lapply(privateAnalysisGroupValues, is.NULLorNA))] <- NULL
  privateAnalysisGroupValues <- lapply(privateAnalysisGroupValues, function(x) list(value = x, operator = NULL, stdErr = NULL))
  
  x <- c(publicAnalysisGroupValues,privateAnalysisGroupValues)   
  public <- c(rep(TRUE, length(publicAnalysisGroupValues)), rep(FALSE, length(privateAnalysisGroupValues)))
  lsTypes <- unlist(lapply(x, function(x) ifelse(class(x$value)=="numeric", "numericValue", "stringValue")))
  lsTypes[names(lsTypes) == "tested_lot"] <- "codeValue"
  lsTypes[names(lsTypes) %in% c("reportedValuesClob", "fitSummaryClob", "parameterStdErrorsClob", "curveErrorsClob", "simpleFitSettings")] <- "clobValue"
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
                  flag = "flag",
                  simpleFitSettings = "fitSettings")
  matches <- match(names(kindMap), names(x))
  names(x)[matches[!is.na(matches)]] <- kindMap[which(!is.na(matches))]
  lsKinds <- names(x)
  stringValues <- ifelse(lsTypes=="stringValue", lapply(x, function(x) x$value), list(NULL))
  codeValues <- ifelse(lsTypes=="codeValue", lapply(x, function(x) x$value), list(NULL))
  numericValues <- ifelse(lsTypes=="numericValue", lapply(x, function(x) x$value), list(NULL))
  clobValues <- ifelse(lsTypes=="clobValue", lapply(x, function(x) x$value), list(NULL))
  operatorValues <- lapply(x, function(x) x$operator)
  uncertanties <- lapply(x, function(x) x$stdErr)
  uncertantyTypes <- uncertanties
  uncertantyTypes[!unlist(lapply(uncertanties,is.null))] <- "Standard Error"
  agValues <- lapply(1:length(x), 
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
                                                   clobValue = clobValues[[x]],
                                                   publicData = public[[x]],
                                                   lsTransaction=lsTransaction,
                                                   recordedBy = as.character(recordedBy))
                                }
  )
  return(list(analysisGroupCode = analysisGroupCode, analysisGroupValues = agValues))
}

saveDoseResponseData <- function(fitData, recorded_by) {
  myMessenger <- messenger()
  myMessenger$logger <- logger(logName = "com.acas.api.doseresponse.save")
  
  myMessenger$logger$debug("getting transaction id")
  transactionID <- createLsTransaction()$id
  myMessenger$logger$debug("getting analysis group values from fit data")
  fitData[ , analysisGroupValues := list(list(getAnalysisGroupValues(reportedParameters[[1]],
                                                                                  fixedParameters[[1]],
                                                                                  fittedParameters[[1]],
                                                                                  goodnessOfFit.model[[1]],
                                                                                  category[[1]],
                                                                                  approved[[1]],
                                                                                  tested_lot = parameters[[1]][lsKind=="batch code",]$codeValue,
                                                                                  recorded_by[[1]],
                                                                                  transactionID,
                                                                                  doseUnits = as.character(points[[1]][1]$doseunits), 
                                                                                  responseUnits = as.character(points[[1]][1]$responseunits), 
                                                                                  analysisGroupCode = codeName[[1]], 
                                                                                  as.character(parameters[[1]][lsKind=="Rendering Hint"]$stringValue),
                                                                                  reportedValuesClob = reportedValuesClob[[1]],
                                                                                  fitSummaryClob = fitSummaryClob[[1]],
                                                                                  parameterStdErrorsClob = parameterStdErrorsClob[[1]],
                                                                                  curveErrorsClob = curveErrorsClob[[1]],
                                                                                  simpleFitSettings = simpleFitSettings[[1]]
                                                                     )
                                              
                                              ))
          , by = curveid]
  myMessenger$logger$debug("saving dose response parameter data")
  savedStates <- saveDoseResponseCurve(fitData, recorded_by, transactionID)
  myMessenger$logger$debug("saving dose response point data")
  savedPoints <- fitData[,  list(updatePointFlags(points, recorded_by, transactionID))][[1]]  
  myMessenger$logger$debug("returning response")
  return(list(lsStates = savedStates, lsTransaction = transactionID))
  
}

saveDoseResponseCurve <- function(fitData, recordedBy, lsTransaction) {
  
  ignoredAnalysisGroupStates <- lapply(fitData$lsStates, function(x) {
    x <- as.list(x)
    x$lsValues <- NULL
    x$ignored <- TRUE
    updateAcasEntity(x, "analysisgroupstates")
  })
  fitData[ , newStates := list(list(createAnalysisGroupState(
    analysisGroup = list(id =  id[[1]], version = version),
    analysisGroupValues = analysisGroupValues[[1]]$analysisGroupValues,
    recordedBy = recordedBy,
    lsType = "data",
    lsKind = "Dose Response",
    lsTransaction = lsTransaction    
    ))), by = curveid]
  savedAnalysisGroupStates <- saveAcasEntities(fitData$newStates, "analysisgroupstates")
  return(fitData$newStates)
}

getLSStateFromEntity <- function(entities, ...) {
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
  lsStates[!matches] <- NULL
  return(lsStates)
}

updatePointFlags <- function(points, recordedBy, lsTransaction) {  
  saveSession("~/Desktop/blah")
  pointData <- Reduce(function(x,y) rbind(x,y,fill = TRUE), points)
  pointData <- pointData[flagchanged == TRUE, ]
  addTheseFlags <- pointData[ !is.na(flag_user) | !is.na(flag_on.load) | !is.na(flag_algorithm)]
  ignoreTheseFlags <- pointData[!is.na(flag_sv_id), list(flag_sv_id, response_ss_id, response_ss_version, tg_id, flag_user, flag_on.load, flag_algorithm)]
  flagsToIgnore <- lapply(ignoreTheseFlags$flag_sv_id, getEntityById, "subjectvalues")
  ignoredFlags <- lapply(flagsToIgnore, function(x) {
    x$ignored <- TRUE
    updateAcasEntity(x, "subjectvalues")
  })
  if(nrow(addTheseFlags) > 0) {
    addTheseFlags <- reshape2::melt(addTheseFlags,  measure.vars = c("flag_user", "flag_on.load", "flag_algorithm"), value.name = "comments", variable.name = "stringValue")[!is.na(comments),]
    addTheseFlags[ , stringValue := gsub("flag_","" ,stringValue)]
    addTheseFlags[ , stringValue := gsub("on.load","on load" ,stringValue)]
    newFlags <- addTheseFlags[, list(list(createStateValue(lsType = "stringValue", lsKind = "flag", stringValue = stringValue, comments = comments, lsTransaction=lsTransaction,recordedBy=recordedBy, lsState=list(id=response_ss_id[[1]], version=response_ss_version[[1]])))), by = response_sv_id]$V1
    saveAcasEntities(newFlags, "subjectvalues")
  }
#   #Treatment group value updates
#   update_tg_id <- unique(pointData$tg_id)
#   updateTheseValues <- pointData[pointData$tg_id %in% update_tg_id]
#   treatmentGroups <- lapply(update_tg_id, getEntityById, "treatmentgroups")
#   treatmentGroupDF <- ldply(treatmentGroups, flattenEntity, acasCategory= "treatmentGroup", includeFromState = c("id", "lsType", "lsKind", "version"))
#   valuesToIgnoreDF <- treatmentGroupDF[treatmentGroupDF$lsKind == "Response", ]
#   valuesToIgnore <- lapply(valuesToIgnoreDF$id, getEntityById, "treatmentgroupvalues")
#   ignoredValues <- lapply(valuesToIgnore, function(x) {
#     x$ignored <- T
#     updateAcasEntity(x, "treatmentgroupvalues")
#   })
#   updateTheseValues$tgs_id <- treatmentGroupDF$stateId[match(updateTheseValues$tg_id, treatmentGroupDF$treatmentGroupId)]
#   updateTheseValues$tgs_version <- treatmentGroupDF$stateVersion[match(updateTheseValues$tg_id, treatmentGroupDF$treatmentGroupId)]
#   #updateTheseValues$tgv_id <- treatmentGroupDF$id[match(updateTheseValues$tg_id, valuesToIgnoreDF$TreatmentGroupId)]
#   
#   newValues <- updateTheseValues[, list(list(createStateValue(lsType = "numericValue",
#                                                               lsKind = "Response", 
#                                                               numericValue = NAtoNULL(suppressWarnings(mean(response))), 
#                                                               numberOfReplicates=length(response), 
#                                                               uncertaintyType="standard deviation", 
#                                                               uncertainty = NAtoNULL(sd(response)), 
#                                                               lsTransaction=lsTransaction,
#                                                               recordedBy=recordedBy, 
#                                                               lsState=list(id=unique(tgs_id), 
#                                                                            version=unique(tgs_version))))), 
#                                  by = tg_id]$V1
#   saveAcasEntities(newValues, "treatmentgroupvalues")
  
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