#' Experiment status
#' 
#' Sets experiment status
#' 
#' @param status a string such as "running" or "completed"
#' @param experiment an experiment object
#' @param recordedBy username of current user (not used)
#' @param dryRun boolean if in dry run
#' @param lsTransaction the id of the transaction (not used)
#' @param lsServerURL the url for the roo server
#' @details sets the "status" value in state "analysis status" (or "dryrun
#'   status" if \code{dryRun == TRUE}). In updateExperimentMetadata.R
#' @export
setExperimentStatus <- function(status, experiment, recordedBy, dryRun = F, lsTransaction = NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  if (dryRun) {
    lsKind <- "dry run status"
  } else {
    lsKind <- "analysis status"
  }
  
  updateValueByTypeAndKind(status, "experiment", experiment$id, "metadata", 
                           "experiment metadata", "codeValue", lsKind, 
                           lsServerURL = lsServerURL)
}

#' Experiment html
#' 
#' Sets experiment html
#' 
#' @param htmlText html element to display for experiment
#' @param experiment an experiment object
#' @param recordedBy username of current user
#' @param dryRun boolean if in dry run
#' @param lsTransaction the id of the transaction
#' @param lsServerURL the url for the roo server
#' @details sets the "status" value in state "analysis status" (or "dryrun
#'   status" if \code{dryRun == TRUE}). In updateExperimentMetadata.R
#' @export
setExperimentHtml <- function(htmlText, experiment, recordedBy, dryRun = F, lsTransaction = NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  if (dryRun) {
    lsKind <- "dry run result html"
  } else {
    lsKind <- "analysis result html"
  }
  
  updateValueByTypeAndKind(htmlText, "experiment", experiment$id, "metadata", 
                           "experiment metadata", "clobValue", lsKind)
}

entityFileStorePaths <- c(experiment = "experiments", protocol = "protocols",  lsthing = "entities", cmpdregBulkLoad = "cmpdreg_bulkload")

#' save file in ACAS
#' 
#' Saves a file into a file service and saves a reference to it in the
#' experiment
#' 
#' @param fileStartLocation path to file, relative from privateUploads
#' @param experiment an experiment object, must have "id" and "codeName"
#' @param stateType the stateType of save location, e.g. "metadata"
#' @param stateKind the stateKind of save location, e.g. "experiment metadata"
#' @param valueKind the valueKind, or description of the file e.g. "source file"
#' @param recordedBy username of current user
#' @param lsTransaction the id of the transaction
#' @param valueType usually fileValue, could also be inlineFileValue
#' @param additionalPath if saving to a file system and not a service,
#'   additional file organization, i.e. "analysis/final"
#' @param fileServiceType "blueimp" for internal, "custom" for custom
#' @param fileService only required if fileServiceType is "custom", path to
#'   custom file service
#' @param deleteOldFile boolean, \code{TRUE} if current file should be deleted 
#'   after a copy is made
#' @param customSourceFileMove function for custom moving file to server
#' @param lsServerURL the url for the roo server
#' @details sets the "status" value in state "analysis status" (or "dryrun 
#'   status" if \code{dryRun == TRUE}). Get the file back with 
#'   \code{\link{getAcasFileLink}}. In updateExperimentMetadata.R
#' @export
saveAcasFileToExperiment <- function(
  fileStartLocation, experiment, stateType, stateKind, 
  valueKind, recordedBy, lsTransaction, valueType = "fileValue", additionalPath = "",
  fileServiceType = racas::applicationSettings$server.service.external.file.type, 
  fileService = racas::applicationSettings$server.service.external.file.service.url,
  deleteOldFile = TRUE, customSourceFileMove = NULL,
  lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  fileName <- basename(fileStartLocation)
  
  experimentCodeName <- experiment$codeName
  
  sourceLocation <- getUploadedFilePath(fileStartLocation)
  entityType <- "experiment"
  if (fileServiceType == "blueimp") {
    folderPath <- entityFileStorePaths[entityType]
    if (additionalPath == "") {
      folderLocation <- file.path(folderPath, experimentCodeName)
    } else {
      folderLocation <- file.path(folderPath, experimentCodeName, additionalPath)
    }
    
    dir.create(getUploadedFilePath(folderLocation), showWarnings = FALSE, recursive = TRUE)
    
    # Move the file
    serverFileLocation <- file.path(folderLocation, fileName)
    targetLocation <- getUploadedFilePath(serverFileLocation)
    if (deleteOldFile) {
      if (!file.rename(from = sourceLocation, to = targetLocation)) {
        warnUser(paste("could not rename file", fileName))
      }
    } else {
      if (sourceLocation != targetLocation) {
        if (!file.copy(from = sourceLocation, to = targetLocation, overwrite = TRUE)) {
          warnUser(paste("could not copy file", fileName))
        }
      }
    }
    
  } else if (fileServiceType %in% c("custom", "gcs")) {
    if(!exists('customSourceFileMove') || is.null(customSourceFileMove)) {
      stop(paste0("customSourceFileMove has not been defined in customFunctions.R"))
    }
    serverFileLocation <- customSourceFileMove(sourceLocation, recordedBy, fileName = fileName, entityType = entityType, entity = experiment, 
                                               deleteOldFile = deleteOldFile, additionalPath = additionalPath)
  } else {
    stopUser("Invalid file service type")
  }
  
  # Ignore old values
  existingValues <- fromJSON(getExperimentValuesByTypeAndKind(
    experiment$id, stateType, stateKind, valueType, valueKind))
  if (length(existingValues) > 0) {
    for (value in existingValues) {
      value$ignored <- TRUE
      updateAcasEntity(value, "experimentvalues")
    }
    existingStates <- lapply(existingValues, getElement, name="lsState")
  } else {
    existingStates <- fromJSON(getExperimentStatesByTypeAndKind(experiment$id, stateType, stateKind))
    if (length(existingStates) == 0) {
      stop("Must have an existing state for saving fileValue")
    }
  }
  stateId <- vapply(existingStates, getElement, 1, name="id")
  if (length(stateId) > 1) {
    stop(paste("Cannot have more than one stateId for fileValue:", paste(stateId, collapse = ", ")))
  }
  
  # Save new value with name in comments
  newValue <- createStateValue(
    lsType = valueType, lsKind = valueKind, fileValue = serverFileLocation, 
    comments = fileName, recordedBy = recordedBy, lsTransaction = lsTransaction, 
    lsState = existingStates[[1]])
  saveAcasEntity(newValue, "experimentvalues")
  
  return(serverFileLocation)
}

#' save file in ACAS
#' 
#' Saves a file into a file service and saves a reference to it in the provided
#' entity
#' 
#' @param fileStartLocation path to file, relative from privateUploads
#' @param entity an entity object, such as an experiment
#' @param entityKind the kind of entity, such as "experiment" or
#'   "analysisgroup", see \link{acasEntityHierarchy}
#' @param stateType the stateType of save location, e.g. "metadata"
#' @param stateKind the stateKind of save location, e.g. "experiment metadata"
#' @param valueKind the valueKind, or description of the file e.g. "source file"
#' @param recordedBy username of current user
#' @param lsTransaction the id of the transaction
#' @param valueType usually fileValue, could also be inlineFileValue
#' @param additionalPath if saving to a file system and not a service, 
#'   additional file organization, i.e. "analysis/final"
#' @param fileServiceType "blueimp" for internal, "gcs" for google cloud storage and "custom" for custom
#' @param fileService only required if fileServiceType is "custom", path to 
#'   custom file service
#' @param deleteOldFile boolean, \code{TRUE} if current file should be deleted 
#'   after a copy is made
#' @param customSourceFileMove function for custom moving file to server
#' @param lsServerURL the url for the roo server
#' @details sets the "status" value in state "analysis status" (or "dryrun 
#'   status" if \code{dryRun == TRUE}). Mostly used by
#'   \code{\link{saveAcasFileToExperiment}}. In updateExperimentMetadata.R
#' @export
saveAcasFile <- function(fileStartLocation, entity, entityKind, stateType, stateKind, valueKind, recordedBy, 
                         lsTransaction, valueType = "fileValue", additionalPath = "", experimentCodeName = NULL,
                         fileServiceType = racas::applicationSettings$server.service.external.file.type, 
                         fileService = racas::applicationSettings$server.service.external.file.service.url,
                         deleteOldFile = TRUE, customSourceFileMove = NULL,
                         lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  
  fileName <- basename(fileStartLocation)
  
  if (is.null(experimentCodeName)) {
    if (entityKind == "experiment") {
      experimentCodeName <- entity$codeName
    } else {
      stop("Must provide an experimentCodeName if entityKind != 'experiment'")
    }
  }
  
  sourceLocation <- getUploadedFilePath(fileStartLocation)
  if (fileServiceType == "blueimp") {
    folderPath <- entityFileStorePaths[entityKind]
    if (additionalPath == "") {
      folderLocation <- file.path(folderPath, experimentCodeName)
    } else {
      folderLocation <- file.path(folderPath, experimentCodeName, additionalPath)
    }

    dir.create(getUploadedFilePath(folderLocation), showWarnings = FALSE, recursive = TRUE)
    
    # Move the file
    serverFileLocation <- file.path(folderLocation, fileName)
    targetLocation <- getUploadedFilePath(serverFileLocation)
    if (deleteOldFile) {
      if (!file.rename(from = sourceLocation, to = targetLocation)) {
        warnUser(paste("could not rename file", fileName))
      }
    } else {
      if (!file.copy(from = sourceLocation, to = targetLocation)) {
        warnUser(paste("could not copy file", fileName))
      }
    }
    
  } else if (fileServiceType %in% c("custom", "gcs")) {
    if(!exists('customSourceFileMove') || is.null(customSourceFileMove)) {
      stop(paste0("customSourceFileMove has not been defined in customFunctions.R"))
    }
    serverFileLocation <- customSourceFileMove(sourceLocation, recordedBy, fileName = fileName, entityType = entityKind, entity = experiment, 
                                               deleteOldFile = deleteOldFile, additionalPath = additionalPath)
                          
  } else {
    stopUser("Invalid file service type")
  }
  
  updateValueByTypeAndKind(serverFileLocation, entityKind, entity$id, stateType, 
                           stateKind, valueType, valueKind)
  
  return(serverFileLocation)
}

#' Get url for ACAS file
#' 
#' Gets a url from an ACAS filecode. This deals with issues of whether the file
#' is stored internally or on an external file system, so you just get a link to
#' wherever the file is stored.
#' 
#' @param fileCode A file code of some custom type like \code{"FILE1234"} or a
#'   path link like \code{"experiment/EXPT-3/this.txt"}
#' @param login boolean to decide if login is required to reach link. Paths for 
#'   use by R should have this \code{FALSE}, but paths displayed to users should
#'   be \code{TRUE}.
#'   
#' @return a url
#' @details The getting equivalent of \code{\link{saveAcasFile}}. In
#'   updateExperimentMetadata.R
#' @export
getAcasFileLink <- function(fileCode, login = FALSE) {
  fileServiceType <- racas::applicationSettings$server.service.external.file.type
  if (fileServiceType %in% c("blueimp", "gcs")) {
    fileRootPath <- ifelse(login, 
                           paste0(getSSLString(), racas::applicationSettings$client.host, ":",
                                  racas::applicationSettings$client.port,
                                  racas::applicationSettings$client.path),
                           racas::applicationSettings$server.nodeapi.path)
    urlLocation <- paste0(fileRootPath, "/dataFiles/", URLencode(fileCode))
  } else if (fileServiceType == "custom") {
    urlLocation <- paste0(racas::applicationSettings$client.service.external.file.service.url, 
                          URLencode(fileCode))
  }
  return(urlLocation)
}
#' Save experiment parameters
#' 
#' Save the experiment parameters passed from the GUI to the data analysis parameters clobValue.
#' 
#' @param inputParameters string that contains JSON
#' @param experiment list, experiment object from JSON, only needs an id
#' @param lsTransaction unused
#' @param recordedBy unused
saveInputParameters <- function(inputParameters, experiment, lsTransaction, recordedBy) {
  # input: inputParameters a string that is JSON
  updateValueByTypeAndKind(inputParameters, "experiment", experiment$id, "metadata", 
                           "experiment metadata", "clobValue", "data analysis parameters")
  return(NULL)
}
