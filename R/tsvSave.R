#' Save data frames using tsv loader
#' 
#' Saves data frames all in one transaction
#' 
#' @param analysisGroupData A data frame of analysis group values
#' @param treatmentGroupData A data frame of treatment group values
#' @param subjectData A data frame of subject values
#' @param appendCodeName A list with entries "analysisGroupData",
#'   "treatmentGroupData", "subjectData", each has a vector of valuekinds that
#'   should have the code name appended to the front
#
#' @details each of the data frames must have these columns: unitKind, 
#'   valueType, valueKind, numericValue, publicData, stateType, stateKind,
#'   tempStateId, tempId, lsType, lsKind. Other optional columns can be found in
#'   \code{\link{formatEntityAsTsvAndUpload}}. You can get faster saving using
#'   \code{\link{saveAllViaDirectDatabase}}, but it includes fewer checks.
#' 
#' @export
saveAllViaTsv <- function(analysisGroupData, treatmentGroupData, subjectData, appendCodeName = list()) {
  
  sendFiles <- list()
  
  if (!(is.null(appendCodeName$analysisGroup))) {
    analysisGroupData <- appendCodeNames(analysisGroupData, appendCodeName$analysisGroup, "analysis group")
  }
  if (!(is.null(appendCodeName$treatmentGroup))) {
    treatmentGroupData <- appendCodeNames(treatmentGroupData, appendCodeName$treatmentGroup, "treatment group")
  }
  if (!(is.null(appendCodeName$subject))) {
    subjectData <- appendCodeNames(subjectData, appendCodeName$subject, "subject")
  }
  
  # format and save as csv
  
  if (!is.null(analysisGroupData)) {
    analysisGroupDataCsv <- formatEntityAsTsvAndUpload(analysisGroupData)
    sendFiles$analysisGroupCsvFilePath <- analysisGroupDataCsv
  }
  if (!is.null(treatmentGroupData)) {
    treatmentGroupDataCsv <- formatEntityAsTsvAndUpload(treatmentGroupData)
    sendFiles$treatmentGroupCsvFilePath <- treatmentGroupDataCsv
  }
  if (!is.null(subjectData)) {
    subjectDataCsv <- formatEntityAsTsvAndUpload(subjectData)
    sendFiles$subjectCsvFilePath <- subjectDataCsv
  }
  
  response <- analysis_group_save_from_tsv(sendFiles$analysisGroupCsvFilePath, sendFiles$treatmentGroupCsvFilePath, sendFiles$subjectCsvFilePath)
  
  # Delete the files if they are on the same server. Warnings are suppressed
  # because it is expected to fail if they are on different servers (as happens
  # during development)
  if (!is.null(sendFiles$analysisGroupCsvFilePath)) {
    suppressWarnings(file.remove(sendFiles$analysisGroupCsvFilePath))
  }
  if (!is.null(sendFiles$treatmentGroupCsvFilePath)) {
    suppressWarnings(file.remove(sendFiles$treatmentGroupCsvFilePath))
  }
  if (!is.null(sendFiles$subjectCsvFilePath)) {
    suppressWarnings(file.remove(sendFiles$subjectCsvFilePath))
  }  
  return(response)
}
#' Save file paths using tsv loader
#' 
#' Saves tsv files all in one transaction
#' 
#' @param analysisGroupCsvFilePath A file path to analysis group values tsv file
#' @param optional treatmentGroupCsvFilePath A file path to treatment group values tsv file
#' @param optional subjectCsvFilePath A file path to subject group values tsv file
#' @details files must be a full path reachable to the acas roo services
#' 
#' @export
analysis_group_save_from_tsv <- function(analysisGroupCsvFilePath, treatmentGroupCsvFilePath = NULL, subjectCsvFilePath = NULL) {
  filePaths <- list(analysisGroupCsvFilePath = analysisGroupCsvFilePath)
  filePaths$treatmentGroupCsvFilePath <- treatmentGroupCsvFilePath
  filePaths$subjectCsvFilePath <- subjectCsvFilePath  

  response <- postURLcheckStatus(
    paste0(racas::applicationSettings$client.service.persistence.fullpath, 
           "experiments/analysisgroup/savefromtsv"),
    postfields=toJSON(filePaths),
    requireJSON = TRUE
  )
  return(response)
}

#' Appends code names to supplied valueKinds
#' 
#' @param entityData data.frame that will be changed
#' @param valueKinds vector of valueKinds to have stringValues changed
#' @param entitySpaced the name of the entity: "analysis group" or "treatment 
#'   group" or "subject"
#'   
#' @details not for export, used only by saveAllViaTsv. Supports data.table and
#'   data.frame. In tsvSave.R
appendCodeNames <- function(entityData, valueKinds, entitySpaced) {
  thingTypeAndKind <- paste0("document_", entitySpaced)
  entityCodeNameList <- unlist(getAutoLabels(thingTypeAndKind = thingTypeAndKind, 
                                             labelTypeAndKind = "id_codeName", 
                                             numberOfLabels = max(entityData$tempId)),
                               use.names = FALSE)
  
  entityData$codeName <- entityCodeNameList[entityData$tempId]
  
  # Adding codeNames to analysisGroupStrings listed in appendCodeNames (e.g. curve id)
  if (data.table::is.data.table(entityData)) {
    newStrings <- paste0(entityData[valueKind %in% valueKinds, stringValue], 
                         "_", entityData[valueKind == valueKinds, codeName])
  } else {
    newStrings <- paste0(entityData[entityData$valueKind %in% valueKinds, "stringValue"], 
                         "_", entityData[entityData$valueKind == valueKinds, "codeName"])
  }

  entityData$stringValue[entityData$valueKind %in% valueKinds] <- newStrings
  
  return(entityData)
}

#' Save data using tsv
#' 
#' Saves data frames to a tsv and uploads to persistence server
#' 
#' @param entityData data.frame with set of columns- see code for list
#
#' @details Assumes all data is new, not saving to existing entities
#' 
#' @return path to file on persistence server
#' 
#' @export
formatEntityAsTsvAndUpload <- function(entityData) {
  entityDataFormatted <- data.frame(
    tempValueId = NA,
    valueType = entityData$valueType,
    valueKind = entityData$valueKind,
    numericValue = entityData$numericValue,
    sigFigs = naIfNull(entityData$sigFigs),
    uncertainty = naIfNull(entityData$uncertainty),
    numberOfReplicates = naIfNull(entityData$numberOfReplicates),
    uncertaintyType = naIfNull(entityData$uncertaintyType),
    stringValue = naIfNull(entityData$stringValue),
    dateValue = naIfNull(entityData$dateValue),
    clobValue = naIfNull(entityData$clobValue),
    urlValue = naIfNull(entityData$urlValue),
    fileValue = naIfNull(entityData$fileValue),
    codeOrigin = naIfNull(entityData$codeOrigin),
    codeType = naIfNull(entityData$codeType),
    codeKind = naIfNull(entityData$codeKind),
    codeValue = naIfNull(entityData$codeValue),
    concentration = naIfNull(entityData$concentration),
    concUnit = naIfNull(entityData$concUnit),
    unitType = NA,
    unitKind = naIfNull(entityData$unitKind),
    operatorType = NA,
    operatorKind = naIfNull(entityData$operatorKind),
    publicData = entityData$publicData,
    comments = naIfNull(entityData$comments),
    stateType = entityData$stateType,
    stateKind = entityData$stateKind,
    tempStateId = entityData$tempStateId,
    stateId = NA,
    id = NA,
    tempId = entityData$tempId,
    parentId = naIfNull(entityData$parentId),
    tempParentId = naIfNull(entityData$tempParentId),
    lsTransaction = naIfNull(entityData$lsTransaction),
    recordedBy = naIfNull(entityData$recordedBy),
    codeName = naIfNull(entityData$codeName),
    lsType = entityData$lsType,
    lsKind = entityData$lsKind
  )
  
  # Create temporary file to send to persistence server
  csvFile <- tempfile(pattern = "csvUpload", tmpdir = "", fileext = ".tsv")
  csvLocalLocation <- paste0(tempdir(), csvFile)
  
  write.table(entityDataFormatted, file = csvLocalLocation, sep="\t", na = "", row.names = FALSE)
  
  tryCatch({
    response <- fromJSON(postForm(paste0(racas::applicationSettings$server.service.persistence.fileUrl), 
                      fileData = (fileUpload(csvLocalLocation, contentType = "text/plain"))))
  }, error = function (e) {
    stopUser("Internal: Could not send tsv file to server, make sure upload server is running.")
  })
  
  
  tsvServerLocation <- file.path(racas::applicationSettings$server.service.persistence.filePath, response$files[[1]]$name)
  return(tsvServerLocation)
}

#' Turn NULL into NA
#' 
#' Useful for changing objects to data frames
#' 
#' @param x input
#' @param naType NA value, e.g.\code{NA_character_}
#' 
#' @details will return the input for any value other than NULL
#' 
#' @export
naIfNull <- function(x, naType = NULL) {
  if (is.null(x)) {
    if (is.null(naType)) {
      return(NA)
    } else {
      return(naType)
    }
  } else {
    return(x)
  }
}
