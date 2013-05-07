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
#' @param lsTransaction a list, inclues an id and version
#' @return A data.frame with columns "entityStateId" and "entityStateVersion", which are often added back to the original data.frame
#' @keywords save, format, stateGroups
#' @export


saveStatesFromLongFormat <- function(entityData, entityKind, stateGroups, idColumn, recordedBy, lsTransaction, stateGroupIndices = NULL) {
  # saves "raw only" states
  # 
  # Args:
  #   entityData:           A data frame that includes columns:
  #     stateGroupIndex:      Integer vector marking the index of the state group for each row
  #     (idColumn):           Integer vector that separates rows into states (used for grouping, does not use numbers)
  #   entityKind:           A string of the kind of the state from list "subject", "treatmentgroup", "container"
  #   stateGroups:          A list of lists, each of which includes details about how to save states
  #   stateGroupIndices:    An integer vector of the indices to use from stateGroups (others are removed)
  #   idColumn:             The name of the column to use to separate groups
  #   recordedBy:           A string of the username
  #   lsTransaction:        A list that is an lsTransaction (must have an "id" element)
  #
  # Returns:
  #   An integer vector of the state id's
  
  require(plyr)
  
  if (is.null(stateGroupIndices)) {
    stateGroupIndices <- which(sapply(stateGroups, function(x) return (x$entityKind)) == entityKind)
  }
  
  createRawOnlyEntityState <- function(entityData, stateGroups, entityKind, recordedBy, lsTransaction) {
    
    
    stateType <- stateGroups[[entityData$stateGroupIndex[1]]]$stateType
    stateKind <- stateGroups[[entityData$stateGroupIndex[1]]]$stateKind
    entityState <- switch(
      entityKind,
      "analysisgroup" = {createAnalysisGroupState(analysisGroup = list(id=entityData$analysisGroupID[1], version=0),
                                                  stateType=stateType,
                                                  stateKind=stateKind,
                                                  recordedBy=recordedBy,
                                                  lsTransaction=lsTransaction)
      },
      "subject" = {createSubjectState(subject = list(id=entityData$subjectID[1], version=0),
                                      stateType=stateType,
                                      stateKind=stateKind,
                                      recordedBy=recordedBy,
                                      lsTransaction=lsTransaction)
      },
      "treatmentgroup" = {createTreatmentGroupState(treatmentGroup = list(id=entityData$treatmentGroupID[1], version=0),
                                                    stateType=stateType,
                                                    stateKind=stateKind,
                                                    recordedBy=recordedBy,
                                                    lsTransaction=lsTransaction)
      },
      "container" = {createContainerState(container = list(id=entityData$containerID[1], version=0),
                                          stateType=stateType,
                                          stateKind=stateKind,
                                          recordedBy=recordedBy,
                                          lsTransaction=lsTransaction)
      },
      stop(paste("Unrecognized entityKind:", entityKind)))
    return(entityState)
  }
  #TODO: the variables remains sketchy... fix once expanded
  entityStates <- dlply(.data=entityData[entityData$stateGroupIndex %in% stateGroupIndices,], .variables=idColumn, .fun=createRawOnlyEntityState, 
                        stateGroups=stateGroups, entityKind=entityKind, recordedBy=recordedBy, lsTransaction=lsTransaction)
  originalStateIds <- names(entityStates)
  names(entityStates) <- NULL
  savedEntityStates <- saveAcasEntities(entityStates, paste0(entityKind, "states"))
  
  entityStateIds <- sapply(savedEntityStates, function(x) x$id)
  entityStateVersions <- sapply(savedEntityStates, function(x) return(x$version))
  entityStateTranslation <- data.frame(entityStateId = entityStateIds, 
                                       originalStateId = originalStateIds, 
                                       entityStateVersion = entityStateVersions)
  
  stateIdAndVersion <- entityStateTranslation[match(entityData[[idColumn]], 
                                                    entityStateTranslation$originalStateId),
                                              c("entityStateId", "entityStateVersion")]
  return(stateIdAndVersion)
}


meltBatchCodes <- function(entityData, batchCodeStateIndices) {
  require('plyr')
  # Turns a batchCode column into rows in a long format
  
  # It will run once, mostly. So it is a for loop
  output <- data.frame()
  for (index in batchCodeStateIndices) {
    batchCodeValues  <- entityData[entityData$stateGroupIndex==index,c("batchCode", "stateID", "stateVersion", "stateGroupIndex", "publicData")]
    if (nrow(batchCodeValues) > 0) {
      names(batchCodeValues)[1] <- "codeValue"
      batchCodeValues$valueType <- "codeValue"
      batchCodeValues$valueKind <- "batch code"
      output <- rbind.fill(output, batchCodeValues)
    }
  }
  return(output)
}
saveValuesFromLongFormat <- function(entityData, entityKind, stateGroups = NULL, lsTransaction, stateGroupIndices = NULL, testMode=FALSE) {
  # saves "raw only" states
  # 
  # Args:
  #   entityData:           A data frame that includes columns:
  #     stateGroupIndex:      Integer vector marking the index of the state group for each row
  #     stateID:              An integer that is the ID of the state for each value
  #     valueType:            A string of "stringValue", "dateValue", or "numericValue"
  #     valueKind:            A string value ofthe kind of value
  #     publicData:           Boolean: Marks if each value should be hidden
  #     stateVersion:         An integer that is the version of the state for each value
  #     (Optional columns)
  #     stringValue:          String: a string value
  #     codeValue:            String: a code, such as a batch code
  #     dateValue:            A Date value
  #     valueOperator:        String: The operator for each value
  #     valueUnit:            String: The units for each value
  #     clobValue:            String: for very long strings
  #     numberOfReplicates:   Integer: The number of replicates
  #     uncertainty:          Numeric: the uncertainty
  #     uncertaintyType:      String: the type of uncertainty, such as standard deviation
  #   entityKind:           A string of the kind of the state from list "subject", "treatmentgroup", "container"
  #   stateGroups:          A list of lists, each of which includes details about how to save states
  #   stateGroupIndices:    An integer vector of the indices to use from stateGroups (others are removed)
  #   lsTransaction:        A list that is an lsTransaction (must have an "id" element)
  #   testMode:             A boolean marking if the function should return JSON instead of saving values
  #
  # Returns:
  #   NULL
  
  if (any(!(c("stateGroupIndex", "valueType", "valueKind", "publicData", "stateVersion") %in% names(entityData)))) {
    stop("Missing input columns in entityData")
  }
  if (any(is.na(entityData$stateID))) {
    stop("No stateID can be NA")
  }
  
  require(plyr)
  
  if (is.null(stateGroupIndices)) {
    stateGroupIndices <- which(sapply(stateGroups, function(x) return (x$entityKind)) == entityKind)
  }
  
  entityData$rowID <- 1:(nrow(entityData))
  createLocalStateValue <- function(entityData, lsTransaction) {
    if (!is.null(entityData$dateValue)) {
      dateValue <- as.numeric(format(as.Date(entityData$dateValue,origin="1970-01-01"), "%s"))*1000
    } else {
      dateValue <- NA
    }
    stateValue <- createStateValue(
      #The name "entityState" will be replaced with whatever other name is being used
      entityState = list(id=entityData$stateID, version = entityData$stateVersion),
      valueType = if (entityData$valueType=="stringValue") {"stringValue"}  
      else if (entityData$valueType=="dateValue") {"dateValue"}
      else if (entityData$valueType == "codeValue") {"codeValue"}
      else {"numericValue"},
      valueKind = entityData$valueKind,
      stringValue = if (is.character(entityData$stringValue) && !is.na(entityData$stringValue)) {entityData$stringValue} else {NULL},
      dateValue = if(!is.na(dateValue)) {dateValue} else {NULL},
      clobValue = if(is.character(entityData$clobValue) && !is.na(entityData$clobValue)) {entityData$clobValue} else {NULL},
      codeValue = if(is.character(entityData$codeValue) && !is.na(entityData$codeValue)) {entityData$codeValue} else {NULL},
      valueOperator = if(is.character(entityData$valueOperator) && !is.na(entityData$valueOperator)) {entityData$valueOperator} else {NULL},
      numericValue = if(is.numeric(entityData$numericValue) && !is.na(entityData$numericValue)) {entityData$numericValue} else {NULL},
      valueUnit = if(is.character(entityData$valueUnit) && !is.na(entityData$valueUnit)) {entityData$valueUnit} else {NULL},
      publicData = entityData$publicData,
      lsTransaction = lsTransaction,
      numberOfReplicates = if(is.numeric(entityData$numberOfReplicates) && !is.na(entityData$numberOfReplicates)) {entityData$numberOfReplicates} else {NULL},
      uncertainty = if(is.numeric(entityData$uncertainty) && !is.na(entityData$uncertainty)) {entityData$uncertainty} else {NULL},
      uncertaintyType = if(is.character(entityData$uncertaintyType) && !is.na(entityData$uncertaintyType)) {entityData$uncertaintyType} else {NULL}
    )
    return(stateValue)
  }
  entityValues <- dlply(.data = entityData[entityData$stateGroupIndex %in% stateGroupIndices,], 
                        .variables = .(rowID), 
                        .fun = createLocalStateValue, 
                        lsTransaction = lsTransaction)
  
  names(entityValues) <- NULL
  changeName <- function(entityValue, entityKind) {
    newEntity <- switch(entityKind,
                        protocol = "protocolState",
                        experiment = "experimentState",
                        analysisgroup = "analysisGroupState",
                        treatmentgroup = "treatmentGroupState",
                        subject = "subjectState",
                        container = "containerState",
                        stop(paste("Unrecognized entityKind:", entityKind)))
    entityValue[[newEntity]] <- entityValue$entityState
    return(entityValue)
  }
  entityValues <- lapply(entityValues, FUN = changeName, entityKind = entityKind)
  if (testMode) {
    entityValues <- lapply(entityValues, function(x) {x$recordedDate <- 42; return (x)})
    return(toJSON(entityValues))
  } else {
    savedEntityValues <- saveAcasEntities(entityValues, paste0(entityKind, "values"))
    return(savedEntityValues)
  }
}