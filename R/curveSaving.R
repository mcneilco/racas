#highestLevelFunction <- 
saveDoseResponseData <- function(fitData) {
  # Fitting already done
  returnDataTable <- fitData[,  saveDoseResponseCurve(points[[1]], fixedParameters[[1]], fittedParameters[[1]], approved) , by = curveid]
  return(returnDataTable)
  
} # model.R in curve branch has data.table examples

saveDoseResponseCurve <- function() {
  # This will save new, adding to current values (this is low priority)
  # Brian creates an object
  
  #Either whole curve set or just updates
  #Needs to send to a generic saving function (sending analysisGroupValues and subjectValues)
  
  # Sam saves data (curve, points)
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