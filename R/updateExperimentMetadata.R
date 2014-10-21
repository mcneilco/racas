#' Experiment status
#' 
#' Sets experiment status
#' 
#' @param status a string such as "running" or "completed"
#' @param experiment an experiment object
#' @param recordedBy username of current user
#' @param dryRun boolean if in dry run
#' @param lsTransaction the id of the transaction
#' @param lsServerURL the url for the roo server
#' @details sets the "status" value in state "analysis status" (or "dryrun
#'   status" if \code{dryRun == TRUE}). In updateExperimentMetadata.R
#' @export
setExperimentStatus <- function(status, experiment, recordedBy, dryRun = F, lsTransaction = NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  if (dryRun) {
    lsKind <- "dryrun status"
  } else {
    lsKind <- "analysis status"
  }
  
  if (is.null(experiment$lsStates)) {
    experiment <- getExperimentById(experiment$id)
  }
  
  if (is.null(lsTransaction)) {
    lsTransaction <- createLsTransaction()$id
  }
  
  experimentState <- getOrCreateExperimentState(
    experiment, "metadata", "experiment metadata", recordedBy, lsTransaction
  )
  experimentValue <- updateOrCreateStateValue(
    "experiment", experimentState, "stringValue", lsKind, stringValue = status, 
    lsTransaction = lsTransaction, recordedBy = recordedBy
  )
}

#' save file in ACAS
#' 
#' Saves a file into a file service and saves a reference to it in the
#' experiment
#' 
#' @param fileStartLocation path to file
#' @param experiment an experiment object
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
#' @param lsServerURL the url for the roo server
#' @details sets the "status" value in state "analysis status" (or "dryrun
#'   status" if \code{dryRun == TRUE}). In updateExperimentMetadata.R
#' @export
saveAcasFileToExperiment <- function(
  fileStartLocation, experiment, stateType, stateKind, 
  valueKind, recordedBy, lsTransaction, valueType = "fileValue", additionalPath = "",
  fileServiceType = racas::applicationSettings$server.service.external.file.type, 
  fileService = racas::applicationSettings$server.service.external.file.service.url,
  deleteOldFile = TRUE,
  lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  return(saveAcasFile(
    fileStartLocation, experiment, 'experiment', stateType, stateKind, valueKind, recordedBy, 
    lsTransaction, valueType, additionalPath, fileServiceType = fileServiceType,
    fileService = fileService, deleteOldFile = deleteOldFile, lsServerURL = lsServerURL))
}

#' save file in ACAS
#' 
#' Saves a file into a file service and saves a reference to it in the provided
#' entity
#' 
#' @param fileStartLocation path to file
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
#' @param fileServiceType "blueimp" for internal, "custom" for custom
#' @param fileService only required if fileServiceType is "custom", path to
#'   custom file service
#' @param deleteOldFile boolean, \code{TRUE} if current file should be deleted
#'   after a copy is made
#' @param lsServerURL the url for the roo server
#' @details sets the "status" value in state "analysis status" (or "dryrun
#'   status" if \code{dryRun == TRUE}). In updateExperimentMetadata.R
#' @export
saveAcasFile <- function(fileStartLocation, entity, entityKind, stateType, stateKind, valueKind, recordedBy, 
                         lsTransaction, valueType = "fileValue", additionalPath = "", experimentCodeName = NULL,
                         fileServiceType = racas::applicationSettings$server.service.external.file.type, 
                         fileService = racas::applicationSettings$server.service.external.file.service.url,
                         deleteOldFile = TRUE, 
                         lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  
  fileName <- basename(fileStartLocation)
  
  if (is.null(experimentCodeName)) {
    if (entityKind == "experiment") {
      experimentCodeName <- entity$codeName
    } else {
      stop("Must provide an experimentCodeName if entityKind != 'experiment'")
    }
  }
  
  if (fileServiceType == "blueimp") {
    folderLocation <- file.path(dirname(fileStartLocation), "experiments", 
                                experimentCodeName, additionalPath)
    dir.create(folderLocation, showWarnings = FALSE, recursive = TRUE)
    
    # Move the file
    serverFileLocation <- file.path(folderLocation, fileName)
    if (deleteOldFile) {
      file.rename(from = fileStartLocation, to = serverFileLocation)
    } else {
      file.copy(from = fileStartLocation, to = serverFileLocation)
    }
    
    
  } else if (fileServiceType == "custom") {
    if(!exists('customSourceFileMove')) {
      stop(paste0("customSourceFileMove has not been defined in customFunctions.R"))
    }
    # TODO: figure out if need to update code for client to respect deleteOldFile
    serverFileLocation <- customSourceFileMove(fileStartLocation, fileName, fileService, experiment, recordedBy)
  } else {
    stopUser("Invalid file service type")
  }
  
  if (is.null(entity$lsStates)) {
    entity <- getEntityById(entity$id, paste0(entityKind, "s"))
  }
  lsState <- getOrCreateEntityState(entity, entityKind, stateType, stateKind, recordedBy, lsTransaction)
  
  updateOrCreateStateValue(entityKind, lsState, valueType, valueKind, 
                           fileValue = serverFileLocation, comments = fileName, 
                           lsTransaction = lsTransaction, recordedBy = recordedBy)
  
  return(serverFileLocation)
}
