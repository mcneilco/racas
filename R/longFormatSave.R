#' Save states from a long format
#'
#' This function saves states to the database specified in \code{\link{applicationSettings}}
#' 
#'
#' @param entityData a data.frame, including one column named 'stateGroupIndex', one that matches idColumn, and one of the form 'entityID'
#' @param entityKind a string, the kind of the state, limited to "protocol", "experiment", "analysisgroup", "treatmentgroup", "subject", "container", "itxcontainercontainer"
#' @param stateGroups a list of lists, each of which includes details about how to save states (TODO link later)
#' @param stateGroupIndices an integer vector of indices to use from stateGroups
#' @param idColumn a string, the name of the column used to separate states (often stateID)
#' @param recordedBy a string, the name of the person recording the data
#' @param lsTransaction an integer, the id of the lsTransaction
#' @return A data.frame with columns "entityStateId" and "entityStateVersion", which are often added back to the original data.frame
#' @keywords save, format, stateGroups
#' @export


saveStatesFromLongFormat <- function(entityData, entityKind, stateGroups, idColumn, recordedBy, lsTransaction, stateGroupIndices = NULL) {
  
  require(plyr)

  if (is.null(stateGroupIndices)) {
    stateGroupIndices <- which(sapply(stateGroups, getElement, "entityKind") == entityKind)
    
    # This exists because labels were added, it removes indices for labels
    realStateGroups <- which(sapply(stateGroups, function(x) !is.null(x$stateType)))
    stateGroupIndices <- stateGroupIndices[stateGroupIndices %in% realStateGroups]
  }
  
  createRawOnlyLsState <- function(entityData, stateGroups, entityKind, recordedBy, lsTransaction) {
    
    lsType <- stateGroups[[entityData$stateGroupIndex[1]]]$stateType
    lsKind <- stateGroups[[entityData$stateGroupIndex[1]]]$stateKind
    lsState <- switch(
      entityKind,
      "analysisgroup" = {createAnalysisGroupState(analysisGroup = list(id=entityData$analysisGroupID[1], version=0),
                                                  lsType=lsType,
                                                  lsKind=lsKind,
                                                  recordedBy=recordedBy,
                                                  lsTransaction=lsTransaction)
      },
      "subject" = {createSubjectState(subject = list(id=entityData$subjectID[1], version=0),
                                      lsType=lsType,
                                      lsKind=lsKind,
                                      recordedBy=recordedBy,
                                      lsTransaction=lsTransaction)
      },
      "treatmentgroup" = {createTreatmentGroupState(treatmentGroup = list(id=entityData$treatmentGroupID[1], version=0),
                                                    lsType=lsType,
                                                    lsKind=lsKind,
                                                    recordedBy=recordedBy,
                                                    lsTransaction=lsTransaction)
      },
      "container" = {createContainerState(container = list(id=entityData$containerID[1], version=0),
                                          lsType=lsType,
                                          lsKind=lsKind,
                                          recordedBy=recordedBy,
                                          lsTransaction=lsTransaction)
      },
      interaction = {createInteractionState(interaction = list(id=entityData$interactionID[1], version = 0),
                                            lsType=lsType,
                                            lsKind=lsKind,
                                            recordedBy=recordedBy,
                                            lsTransaction=lsTransaction)
      },
      stop(paste("Configuration Error: Unrecognized entityKind:", entityKind)))
    
    return(lsState)
  }
  lsStates <- dlply(.data=entityData[entityData$stateGroupIndex %in% stateGroupIndices,], .variables=idColumn, .fun=createRawOnlyLsState, 
                        stateGroups=stateGroups, entityKind=entityKind, recordedBy=recordedBy, lsTransaction=lsTransaction)
  originalStateIds <- names(lsStates)
  names(lsStates) <- NULL
  savedLsStates <- saveAcasEntities(lsStates, paste0(entityKind, "states"))
  
  lsStateIds <- sapply(savedLsStates, getElement, "id")
  lsStateVersions <- sapply(savedLsStates, getElement, "version")
  entityStateTranslation <- data.frame(entityStateId = lsStateIds, 
                                       originalStateId = originalStateIds, 
                                       entityStateVersion = lsStateVersions)
  stateIdAndVersion <- entityStateTranslation[match(entityData[[idColumn]], 
                                                    entityStateTranslation$originalStateId),
                                              c("entityStateId", "entityStateVersion")]
  return(stateIdAndVersion)
}

#' Save labels from a long format
#'
#' This function saves labels to the database specified in \code{\link{applicationSettings}}
#' 
#'
#' @param entityData a data.frame, including one column named 'stateGroupIndex', one that matches idColumn, and one of the form 'entityID'
#' @param entityKind a string, the kind of the state, limited to "protocol", "experiment", "analysisgroup", "treatmentgroup", "subject", "container", "itxcontainercontainer"
#' @param stateGroups a list of lists, each of which includes details about how to save states (TODO link later)
#' @param stateGroupIndices an integer vector of indices to use from stateGroups
#' @param idColumn a string, the name of the column used to separate states (often stateID)
#' @param recordedBy a string, the name of the person recording the data
#' @param lsTransaction an integer, the id of the lsTransaction
#' @param labelPrefix a string, prefixed to all labels
#' @return NULL
#' @keywords save, format, stateGroups, label, labels
#' @export
saveLabelsFromLongFormat <- function(entityData, entityKind, stateGroups, idColumn, recordedBy, lsTransaction, stateGroupIndices = NULL, labelPrefix = NULL) {
  
  require(plyr)
  if (is.null(stateGroupIndices)) {
    stateGroupIndices <- which(sapply(stateGroups, getElement, "entityKind") == entityKind)
    
    labelStateGroups <- which(sapply(stateGroups, function(x) !is.null(x$labelType)))
    stateGroupIndices <- stateGroupIndices[stateGroupIndices %in% labelStateGroups]
  }
  
  labelData <- entityData[entityData$stateGroupIndex %in% stateGroupIndices, ]
  
  createRawOnlyLsLabel <- function(entityData, stateGroups, entityKind, recordedBy, lsTransaction, labelPrefix) {
    lsType <- stateGroups[[entityData$stateGroupIndex[1]]]$labelType
    lsKind <- stateGroups[[entityData$stateGroupIndex[1]]]$labelKind
    labelTextValueKind <- stateGroups[[entityData$stateGroupIndex[1]]]$labelText
    labelText <- if(is.na(entityData$stringValue[entityData$valueKind == labelTextValueKind][1])) {
      entityData$numericValue[entityData$valueKind == labelTextValueKind][1]
    } else {
      entityData$stringValue[entityData$valueKind == labelTextValueKind][1]
    }
    if(!is.null(labelPrefix)) {
      labelText <- paste0(labelPrefix, "_", labelText)
    }
    
    lsLabel <- switch(
      entityKind,
      "analysisgroup" = {createAnalysisGroupLabel(analysisGroup = list(id=entityData$analysisGroupID[1], version=0),
                                                  lsType=lsType,
                                                  lsKind=lsKind,
                                                  labelText = labelText,
                                                  recordedBy=recordedBy,
                                                  lsTransaction=lsTransaction)
      },
      "subject" = {createSubjectLabel(subject = list(id=entityData$subjectID[1], version=0),
                                      lsType=lsType,
                                      lsKind=lsKind,
                                      labelText = labelText,
                                      recordedBy=recordedBy,
                                      lsTransaction=lsTransaction)
      },
      "treatmentgroup" = {createTreatmentGroupLabel(treatmentGroup = list(id=entityData$treatmentGroupID[1], version=0),
                                                    lsType=lsType,
                                                    lsKind=lsKind,
                                                    labelText = labelText,
                                                    recordedBy=recordedBy,
                                                    lsTransaction=lsTransaction)
      },
      "container" = {createContainerLabel(container = list(id=entityData$containerID[1], version=0),
                                          lsType=lsType,
                                          lsKind=lsKind,
                                          labelText = labelText,
                                          recordedBy=recordedBy,
                                          lsTransaction=lsTransaction)
      },
      interaction = {createInteractionLabel(interaction = list(id=entityData$interactionID[1], version = 0),
                                            lsType=lsType,
                                            lsKind=lsKind,
                                            labelText = labelText,
                                            recordedBy=recordedBy,
                                            lsTransaction=lsTransaction)
      },
      stop(paste("Configuration Error: Unrecognized entityKind:", entityKind)))
    return(lsLabel)
  }
  lsLabels <- dlply(.data=labelData, .variables=idColumn, .fun=createRawOnlyLsLabel, 
                    stateGroups=stateGroups, entityKind=entityKind, recordedBy=recordedBy, lsTransaction=lsTransaction, labelPrefix=labelPrefix)
  names(lsLabels) <- NULL
  savedLsLabels <- saveAcasEntities(lsLabels, paste0(entityKind, "labels"))
  
  return(NULL)
}

#' Turns a batchCode column into rows in a long format
#' 
#' @param entityData a data frame with data
#' @param batchCodeStateIndices a numeric vector of indices in the stateGroupIndexColumn which should have batchCodes melted
#' @param replacedFakeBatchCode deprecated: a character vector of fake batch id's that were replaced, marking invalid batch codes
#' 
#' @details Does not work with data.table
#' 
#' @return A data frame with rows for all code values
#' 
meltBatchCodes <- function(entityData, batchCodeStateIndices, replacedFakeBatchCode = NULL) {
  require('plyr')
  
  # It will run once, mostly. So it is a for loop
  output <- data.frame()
  for (index in batchCodeStateIndices) {
#     if(is.null(replacedFakeBatchCode)) {
      batchCodeValues <- unique(entityData[entityData$stateGroupIndex==index, c("batchCode", "stateID", "stateVersion", "stateGroupIndex", "publicData")])
#       fakeBatchCodeValues <- data.frame()
#     } else {
      #batchCodeValues <- unique(entityData[entityData$stateGroupIndex==index, c("batchCode", "stateID", "stateVersion", "stateGroupIndex", "publicData", "originalBatchCode")])
      #fakeBatchCodeValues <- batchCodeValues[batchCodeValues$originalBatchCode %in% replacedFakeBatchCode, ]
      #batchCodeValues <- batchCodeValues[!(batchCodeValues$originalBatchCode %in% replacedFakeBatchCode), ]
#     }
    if (nrow(batchCodeValues) > 0) {
      names(batchCodeValues)[1] <- "codeValue"
      batchCodeValues$valueType <- "codeValue"
      batchCodeValues$valueKind <- "batch code"
      output <- rbind.fill(output, batchCodeValues)
    }
#     if (nrow(fakeBatchCodeValues) > 0) {
#       names(fakeBatchCodeValues)[names(fakeBatchCodeValues) == "originalBatchCode"] <- "codeValue"
#       fakeBatchCodeValues$valueType <- "codeValue"
#       fakeBatchCodeValues$valueKind <- "batch code"
#       fakeBatchCodeValues$batchCode <- NULL
#       output <- rbind.fill(output, fakeBatchCodeValues)
#     }
  }
  return(output)
}


#' Turns concentration columns into rows in a long format
#' 
#' @param entityData a data frame with data, must include rows "concentration" and "concentrationUnit"
meltConcentrations <- function(entityData) {
  require('plyr')
  
  createConcentrationRows <- function(entityData) {
    if(any(is.na(entityData$concentration))) {
      return(data.frame())
    } else {
      output <- data.frame(batchCode = entityData$batchCode[1], 
                           valueKind = "tested concentration", 
                           valueType = "numericValue",
                           numericValue = entityData$concentration[1],
                           valueUnit = entityData$concentrationUnit[1],
                           stateID = entityData$stateID[1],
                           stateGroupIndex = entityData$stateGroupIndex[1],
                           publicData = entityData$publicData[1],
                           resultTypeAndUnit = paste("INTERNAL---tested concentration", 
                                                     entityData$concentration[1], 
                                                     entityData$concentrationUnit[1], 
                                                     entityData$time[1], 
                                                     entityData$timeUnit[1]),
                           stringsAsFactors = FALSE)
      if(!is.null(entityData$treatmentGroupID) && !is.na(entityData$treatmentGroupID)) {
        output$treatmentGroupID <- entityData$treatmentGroupID[1]
      }
      return(output)
    }
  }
  output <- ddply(.data=entityData, .variables = c("stateID"), .fun = createConcentrationRows)
  return(output)
}

#' Turns time columns into rows in a long format
#' 
#' @param entityData a data frame with data, must include rows "time" and "timeUnit"
meltTimes <- function(entityData) {
  require('plyr')
  
  createTimeRows <- function(entityData) {
    if(any(is.na(entityData$time))) {
      return(data.frame())
    } else {
      output <- data.frame(batchCode = entityData$batchCode[1], 
                           valueKind = "time", 
                           valueType = "numericValue",
                           numericValue = entityData$time[1],
                           valueUnit = entityData$timeUnit[1],
                           stateID = entityData$stateID[1],
                           stateGroupIndex = entityData$stateGroupIndex[1],
                           publicData = entityData$publicData[1],
                           resultTypeAndUnit = paste("INTERNAL---time", 
                                                     entityData$concentration[1], 
                                                     entityData$concentrationUnit[1], 
                                                     entityData$time[1], 
                                                     entityData$timeUnit[1]),
                           stringsAsFactors = FALSE)
      if(!is.null(entityData$treatmentGroupID) && !is.na(entityData$treatmentGroupID)) {
        output$treatmentGroupID <- entityData$treatmentGroupID[1]
      }
      return(output)
    }
  }
  output <- ddply(.data=entityData, .variables = c("stateID"), .fun = createTimeRows)
  return(output)
}

#' saves "raw only" values
#' 
#' Saves values from a specific format
#' 
#' @param entityData A data frame that includes columns:
#'    \describe{
#'    \item{stateGroupIndex}{Integer vector marking the index of the state group for each row}
#'    \item{operatorType}{String: the type of the operator}
#'    \item{unitType}{String: the type of the unit}
#'     \item{stateID}{An integer that is the ID of the state for each value}
#'     \item{valueType}{A string of "stringValue", "dateValue", or "numericValue"}
#'     \item{valueKind}{A string value ofthe kind of value}
#'     \item{publicData}{Boolean: Marks if each value should be hidden}
#'     \item{stateVersion}{An integer that is the version of the state for each value}
#'     \item{stringValue}{String: a string value (optional)}
#'     \item{codeValue}{String: a code, such as a batch code (optional)}
#'     \item{dateValue}{A Date value (optional)}
#'     \item{valueOperator}{String: The operator for each value (optional)}
#'     \item{valueUnit}{String: The units for each value (optional)}
#'     \item{clobValue}{String: for very long strings (optional)}
#'     \item{numberOfReplicates}{Integer: The number of replicates (optional)}
#'     \item{uncertainty}{Numeric: the uncertainty (optional)}
#'     \item{uncertaintyType}{String: the type of uncertainty, such as standard deviation (optional)}
#'     }
#' @param  entityKind          String: the kind of the state, allowed values are: "protocol", "experiment", "analysisgroup", 
#' "subject", "treatmentgroup", "container", "itxcontainercontainer", "itxsubjectcontainer"
#' @param  stateGroups          A list of lists, each of which includes details about how to save states
#' @param  stateGroupIndices    An integer vector of the indices to use from stateGroups (others are removed)
#' @param  lsTransaction        An id of an lsTransaction
#' @param  testMode             A boolean marking if the function should return JSON instead of saving values
#' @param recordedBy String: the username recording the data
#' @return A list of value objects (lists)
saveValuesFromLongFormat <- function(entityData, entityKind, stateGroups = NULL, lsTransaction, recordedBy, stateGroupIndices = NULL, testMode=FALSE) {

  
  if (any(!(c("stateGroupIndex", "valueType", "valueKind", "publicData", "stateVersion") %in% names(entityData)))) {
    stop("Missing input columns in entityData")
  }
  if (any(is.na(entityData$stateID[entityData$stateGroupIndex %in% stateGroupIndices]))) {
    stop("Internal error: No stateID can be NA")
  }
  
  require(plyr)
  
  if (is.null(stateGroupIndices)) {
    stateGroupIndices <- which(sapply(stateGroups, getElement, "entityKind") == entityKind)
    
    # This exists because labels were added, it removes indices for labels
    realStateGroups <- which(sapply(stateGroups, function(x) !is.null(x$stateType)))
    stateGroupIndices <- stateGroupIndices[stateGroupIndices %in% realStateGroups]
  }
  
  entityData$rowID <- 1:(nrow(entityData))
  createLocalStateValue <- function(entityData, lsTransaction) {
    if (!is.null(entityData$dateValue)) {
      dateValue <- as.numeric(format(as.Date(entityData$dateValue,origin="1970-01-01"), "%s"))*1000
    } else {
      dateValue <- NA
    }
    stateValue <- createStateValue(
      lsState = list(id=entityData$stateID, version = entityData$stateVersion),
      lsType = if (entityData$valueType=="stringValue") {"stringValue"}  
      else if (entityData$valueType=="dateValue") {"dateValue"}
      else if (entityData$valueType == "codeValue") {"codeValue"}
      else {"numericValue"},
      lsKind = entityData$valueKind,
      stringValue = if (is.character(entityData$stringValue) && !is.na(entityData$stringValue)) {entityData$stringValue} else {NULL},
      dateValue = if(!is.na(dateValue)) {dateValue} else {NULL},
      clobValue = if(is.character(entityData$clobValue) && !is.na(entityData$clobValue)) {entityData$clobValue} else {NULL},
      codeValue = if(is.character(entityData$codeValue) && !is.na(entityData$codeValue)) {entityData$codeValue} else {NULL},
      valueOperator = if(is.character(entityData$valueOperator) && !is.na(entityData$valueOperator)) {entityData$valueOperator} else {NULL},
      operatorType = if(is.character(entityData$operatorType) && !is.na(entityData$operatorType)) {entityData$operatorType} else {NULL},
      numericValue = if(is.numeric(entityData$numericValue) && !is.na(entityData$numericValue)) {entityData$numericValue} else {NULL},
      valueUnit = if(is.character(entityData$valueUnit) && !is.na(entityData$valueUnit)) {entityData$valueUnit} else {NULL},
      unitType = if(is.character(entityData$unitType) && !is.na(entityData$unitType)) {entityData$unitType} else {NULL},
      publicData = entityData$publicData,
      lsTransaction = lsTransaction,
      numberOfReplicates = if(is.numeric(entityData$numberOfReplicates) && !is.na(entityData$numberOfReplicates)) {entityData$numberOfReplicates} else {NULL},
      uncertainty = if(is.numeric(entityData$uncertainty) && !is.na(entityData$uncertainty)) {entityData$uncertainty} else {NULL},
      uncertaintyType = if(is.character(entityData$uncertaintyType) && !is.na(entityData$uncertaintyType)) {entityData$uncertaintyType} else {NULL},
      recordedBy = recordedBy
    )
    return(stateValue)
  }
  entityValues <- dlply(.data = entityData[entityData$stateGroupIndex %in% stateGroupIndices, ], 
                        .variables = .(rowID), 
                        .fun = createLocalStateValue, 
                        lsTransaction = lsTransaction)
  
  names(entityValues) <- NULL

  if (testMode) {
    entityValues <- lapply(entityValues, function(x) {x$recordedDate <- 42; return (x)})
    return(toJSON(entityValues))
  } else {
    savedEntityValues <- saveAcasEntities(entityValues, paste0(entityKind, "values"))
    return(savedEntityValues)
  }
}
