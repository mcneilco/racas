#' Function to save dose response curve data
#' 
#' This function takes in changes to an already saved curve and updates the flags and fitted parameters
#'
#' @param subjectData A data frame of the changed flag ids (see examples for more on format)
#' 
#' Format:
#' \itemize{
#'   \item analysisGroupdID  The analysisGroupID of the flag
#'   \item valueKind A string that says "Flag"
#'   \item valueType A string that says "stringValue"
#'   \item subjectID The subjedtID of the flag
#'   \item subjectStateID The subjectStateID of the flag
#'   \item treatmentGroupID The treatmentGroupdID of the flag 
#'   \item stringValue A string value for the flag (E.G. "User" or "Algorithm")
#' }
#' @param analysisGroupData A data frame of the parameters to be changed (see examples for more on format)
#' 
#' Format: 
#' \itemize{
#'   \item analysisGroupdID
#'   \item batchCode
#'   \item valueType
#'   \item valueKind
#'   \item resultUnits
#'   \item numericValue
#'   \item StringValue
#' }
#' @keywords save, update, dose, response, curve
#' @export
#' 
updateDoseResponseCurve <- function (analysisGroupData, subjectData) {
  require(racas)
  require(RCurl)
  require(rjson)
  require(plyr)
  
  lsServerURL <<- racas::applicationSettings$serverPath
  
  lsTransaction <- createLsTransaction()
  
  ##### Subject States ====================================================================================
  
  updateFlags <- function (valueTable, newFlags) {
    require(plyr)
    
    addFlags <- function(stateID, stringValue) {
      newRow <- data.frame(ignored = FALSE, publicData = TRUE, stringValue = stringValue, valueKind = "flag", 
                           valueType = "stringValue", stateID = stateID, stringsAsFactors=FALSE)
    }
    
    valueTableWithoutFlags <- valueTable[valueTable$valueKind != "flag", ]
    flagValueTable <- mdply(newFlags, .fun = addFlags)
    
    return(rbind.fill(valueTableWithoutFlags, flagValueTable))
  }
  
  # This is a closure. Don't move it out without pulling lsTransaction through
  copyStates <- function(subject) {
    # Args:
    #   subject:    A list that is a subject
    #
    # Returns:
    #   a list of new states that are copies of the old ones
    
    
    # Copy and update these states
    stateTypeAndKinds <- sapply(subject$subjectStates, function(x) x$stateTypeAndKind)
    nonIgnoredStates <- sapply(subject$subjectStates, function(x) !x$ignored)
    statesToChange <- subject$subjectStates[stateTypeAndKinds =="data_results" & nonIgnoredStates]
    
    setTransaction <- function(stateToChange, subjectId) {
      stateToChange$lsTransaction <- lsTransaction
      stateToChange$subjectValues <- NULL
      stateToChange$subject = list(id = subjectId, version = 0)
      return(stateToChange)
    }
    statesToChange <- lapply(statesToChange, setTransaction, subject$id)
    return (statesToChange)
  }
  
  subjects <- fromJSON(getURL(
    paste0(lsServerURL, "subjects/stateTypeAndKind/data_results/jsonArray"),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(lapply(unique(subjectData$subjectID), function(x) list(id = x))))
  )
  
  newStates <- unlist(lapply(subjects, copyStates), recursive = FALSE)
  originalStateIds <- sapply(newStates, function(x) x$id)
  newStates <- lapply(newStates, function(x) {x$id <- NULL; return(x)})
  
  savedSubjectStates <- saveAcasEntities(newStates, "subjectstates")
  subjectStateIds <- sapply(savedSubjectStates, function(x) x$id)
  
  stateIdsToIgnore <- unique(originalStateIds)
  
  subjectStateTranslation <- data.frame(subjectStateId=subjectStateIds, originalStateId=originalStateIds)
  
  ##### Subject Values ====================================================================================
  
  valuesWithStatesIncluded <- ldply(subjects, getValuesFromSubject)
  
  valuesToReplace <- valuesWithStatesIncluded[valuesWithStatesIncluded$stateID %in% subjectData$subjectStateID & 
                                                !(valuesWithStatesIncluded$ignored), ]
  
  valueIdsToIgnore <- unique(valuesToReplace$id)
  
  # Not sure why this is here anymore...
  valuesToReplace <- valuesToReplace[!(is.na(valuesToReplace$stateID)), ]
  
  valuesToReplace$id <- NULL
  
  newFlags <- subjectData[subjectData$valueKind == "flag", c("subjectStateID", "stringValue")]
  names(newFlags) <- c("stateID","stringValue")
  valuesToReplace <- updateFlags(valuesToReplace, newFlags)
  
  valuesToReplace$oldSubjectStateID <- valuesToReplace$stateID
  valuesToReplace$stateID <- subjectStateTranslation$subjectStateId[match(valuesToReplace$stateID,
                                                                          subjectStateTranslation$originalStateId)]
  
  valuesToReplace$stateVersion <- 0
  valuesToReplace$stateGroupIndex <- 1
  
  savedSubjectValues <- saveValuesFromLongFormat(valuesToReplace, stateGroupIndices=1, entityKind="subject", lsTransaction=lsTransaction)
  
  # Ignore old subject states
  query(paste0(
    "UPDATE subject_state
    SET ignored       = 1,
    version                   = version+1
    WHERE id          IN (", paste(stateIdsToIgnore, collapse=","), ")"))
  # Ignore old subject values
  query(paste0(
    "UPDATE subject_value
    SET ignored       = 1,
    version                   = version+1
    WHERE id          IN  (", paste(valueIdsToIgnore, collapse=","), ")"))
  
  
  ##### Treatment Groups ======================================================
  treatmentGroupStateGroups <- list(list(entityKind = "treatmentgroups",
                                         stateType = "data", 
                                         stateKind = "results",
                                         includesOthers = TRUE,
                                         includesCorpName = FALSE))
  
  treatmentGroupInput <- valuesToReplace[valuesToReplace$valueKind %in% c("Response", "flag"), 
                                         c('numericValue', 'stringValue', 'valueKind', 'valueOperator', 'valueType', 'valueUnit','oldSubjectStateID')]
  
  treatmentGroupIdTranslation <- subjectData[,c('subjectStateID', 'treatmentGroupID')]
  
  treatmentGroupInput$treatmentGroupID <- treatmentGroupIdTranslation$treatmentGroupID[match(treatmentGroupInput$oldSubjectStateID, 
                                                                                             treatmentGroupIdTranslation$subjectStateID)]
  
  createTreatmentGroupData <- function(inputData) {
    flaggedStateIDs <- inputData$oldSubjectStateID[inputData$valueKind == "flag" & ((inputData$stringValue != "") | is.na(inputData$stringValue))]
    dataToAverage <- inputData[!(inputData$oldSubjectStateID %in% flaggedStateIDs) & inputData$valueType=="numericValue", ]
    return(data.frame(
      valueKind = "Response", 
      valueType = "numericValue",
      # TODO: once there is more real data, add this back in and check
      #valueUnit = inputData$valueUnit[1],
      treatmentGroupID = inputData$treatmentGroupID[1],
      numberOfReplicates = nrow(dataToAverage),
      uncertainty = sd(dataToAverage$numericValue),
      uncertaintyType = "standard deviation",
      numericValue = if (is.numeric(dataToAverage$numericValue)) mean(dataToAverage$numericValue) else NA,
      stringValue = if (!is.numeric(dataToAverage$numericValue)) "flagged" else NA
    ))
  }
  treatmentGroupData <- ddply(treatmentGroupInput, .(treatmentGroupID), createTreatmentGroupData)
  
  treatmentGroupData$stateGroupIndex <- 1
  treatmentGroupData$publicData <- TRUE
  
  query(paste0(
    "UPDATE treatment_group_state
    SET ignored               = 1,
    version                   = version+1
    WHERE state_type_and_kind = 'data_results'
    AND ignored               = 0
    AND treatment_group_id    IN (", paste(treatmentGroupData$treatmentGroupID, collapse=","), ")"))
  
  query(paste0(
    "UPDATE treatment_group_value
    SET ignored               = 1,
    version                   = version+1
    WHERE treatment_state_id    IN
    (SELECT id
    FROM treatment_group_state
    WHERE state_type_and_kind = 'data_results'
    AND ignored               = 0
    AND treatment_group_id IN (", paste(treatmentGroupData$treatmentGroupID, collapse=","), "))"))
  
  treatmentGroupData$stateID <- saveStatesFromLongFormat(entityData = treatmentGroupData, entityKind = "treatmentgroup", 
                                                         stateGroups=treatmentGroupStateGroups, stateGroupIndices = 1, 
                                                         idColumn = "treatmentGroupID", recordedBy = "curveCuration", lsTransaction = lsTransaction)[['entityStateId']]
  treatmentGroupData$stateVersion <- 0
  savedTreatmentGroupValues <- saveValuesFromLongFormat(entityData = treatmentGroupData, entityKind = "treatmentgroup", 
                                                        stateGroups = treatmentGroupStateGroups, stateGroupIndices = 1,
                                                        lsTransaction = lsTransaction)
  
  #####  AnalysisGroups =======================================================
  
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
  return(TRUE)
}

replaceNullWithNA <- function(inputList) {
  inputList[sapply(inputList,is.null)] <- NA
  return(inputList)
}
addSubjectState <- function(subjectValue, subjectStateId) {
  print(i)
  subjectValue$stateID <- subjectStateId
  subjectValue$lsTransaction <- subjectValue$lsTransaction$id
  subjectValue <- replaceNullWithNA(subjectValue)
  return(as.data.frame(subjectValue, stringsAsFactors=FALSE))
  i <<- i+1
}
getValuesFromState <- function(subjectState) {
  require('plyr')
  return(ldply(subjectState$subjectValues, addSubjectState, subjectStateId=subjectState$id))
}
getValuesFromSubject <- function(subject) {
  require('plyr')
  return(ldply(subject$subjectStates, getValuesFromState))
}
