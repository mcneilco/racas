#'Saves a file that will show up as a link in Seurat
#'
#'Saves an analysis group with states and values holding a file or url. Each batchCode has a separate state with the annotation.
#'
#'@param batchCodeList A list or vector of batch codes
#'@param recordedBy The username of the person recording the data
#'@param experiment A list that is an experiment object, needs to have values for id, verison, and codeName
#'@param lsTransaction A number that is a transaction id
#'@param reportFileSummary A description of the file or url annotation
#'@param reportFilePath The path to the report file (may be relative)
#'@param url A url (e.g. http://google.com)
#'@param fileType The type of the annotation, used by some custom services
#'@param testMode a boolean used for testing
#'
#'@details Must have either a reportFilePath or a url, but having both will result in the url being ignored.
#'
#'@return NULL
#'@export
addFileLink <- function(batchCodeList, recordedBy, experiment, lsTransaction, 
                        reportFileSummary = NULL, reportFilePath = NULL, fileType=NULL, url=NULL, testMode=FALSE) {
  
  if (!is.null(reportFilePath)) {
    fileName <- basename(reportFilePath)
    
    experimentCodeName <- experiment$codeName
    entityType <- "experiment"
    folderPath <- entityFileStorePaths["experiment"]
    
    if (racas::applicationSettings$server.service.external.file.type == "blueimp") {
      experimentFolderLocation <- file.path(dirname(reportFilePath),folderPath)
      if(!testMode) {
        dir.create(experimentFolderLocation, showWarnings = FALSE)
        
        fullFolderLocation <- file.path(experimentFolderLocation, experimentCodeName)
        dir.create(fullFolderLocation, showWarnings = FALSE)
        
        # Move the file
        file.rename(from=reportFilePath, to=file.path(fullFolderLocation, fileName))
      }
      
      serverFileLocation <- file.path(folderPath, experimentCodeName, fileName)
    } else if (racas::applicationSettings$server.service.external.file.type == "custom") {
      if(!exists('customSourceFileMove') || is.null(customSourceFileMove)) {
        stop(paste0("customSourceFileMove has not been defined in customFunctions.R"))
      }
      serverFileLocation <- customSourceFileMove(reportFilePath, recordedBy, fileName = fileName, entityType = entityType, entity = experiment, 
                                                deleteOldFile = TRUE, additionalPath = NA)
    } else {
      stopUser(paste0("A file service custom code for ", racas::applicationSettings$server.service.external.file.type, " should be added in the configuration file"))
    }
  }
  
  analysisGroupStates <- list()
  analysisGroups <- list()
  
  for (batchCode in batchCodeList) {
    analysisGroupStates <- list()
    analysisGroupValues <- list()
    # File or url value
    if (!is.null(reportFilePath)) {
      analysisGroupValues[[length(analysisGroupValues)+1]] <- createStateValue(
        lsType = "fileValue",
        lsKind = "report file",
        fileValue = serverFileLocation,
        comments = reportFileSummary,
        lsTransaction=lsTransaction,
        testMode=testMode
      )
    } else if (!is.null(url)) {
      analysisGroupValues[[length(analysisGroupValues)+1]] <- createStateValue(
        lsType = "urlValue",
        lsKind = "report url",
        urlValue = url,
        comments = reportFileSummary,
        lsTransaction=lsTransaction,
        testMode=testMode
      )
    } else {
      stopUser("Must supply either a reportFilePath or a url")
    }
    
    analysisGroupValues[[length(analysisGroupValues)+1]] <- createStateValue(
      lsType = "codeValue",
      lsKind = "batch code",
      codeValue = batchCode,
      lsTransaction = lsTransaction,
      testMode=testMode
    )    
    
    analysisGroupStates[[length(analysisGroupStates)+1]] <- createAnalysisGroupState( lsTransaction=lsTransaction, 
                                                                                      recordedBy=recordedBy,
                                                                                      lsType="metadata",
                                                                                      lsKind="report locations",
                                                                                      analysisGroupValues=analysisGroupValues,
                                                                                      testMode=testMode)
    
    analysisGroups[[length(analysisGroups)+1]] <- createAnalysisGroup(lsTransaction=lsTransaction,
                                                                      recordedBy=recordedBy,
                                                                      analysisGroupStates=analysisGroupStates,
                                                                      experiment=experiment,
                                                                      testMode=testMode)
  }
  
  if(testMode) {
    output <- list(analysisGroups = analysisGroups)
  } else {
    tryCatch({
      response <- saveAnalysisGroups(analysisGroups) 
    }, error = function(e) {
      stopUser(paste0("Could not save the report ", if (!is.null(reportFilePath)) "file" else "url"))
    })
  }
  
  existingStates <- fromJSON(getExperimentStatesByTypeAndKind(experiment$id, "metadata", "experiment metadata"))
  if (length(existingStates) == 0) {
    stop("Must have an existing state for saving fileValue")
  }
  locationState <- existingStates[[1]]
  
  if (racas::applicationSettings$server.service.external.file.type == "blueimp" || racas::applicationSettings$server.service.external.file.type == "custom") {
    if (!is.null(reportFilePath)) {
      locationValue <- createStateValue(recordedBy = recordedBy,
                                        lsType = "fileValue",
                                        lsKind = "annotation file",
                                        fileValue = serverFileLocation,
                                        lsState = locationState,
                                        lsTransaction = lsTransaction,
                                        comments = reportFileSummary,
                                        testMode=testMode)
    } else {
      locationValue <- createStateValue(recordedBy = recordedBy,
                                        lsType = "urlValue",
                                        lsKind = "annotation url",
                                        urlValue = url,
                                        lsState = locationState,
                                        lsTransaction = lsTransaction,
                                        comments = reportFileSummary,
                                        testMode=testMode)
    }
  } else {
    stopUser("Need to set up separate service R code for this annotation")
  }
  if(testMode) {
    output$locationValue <- locationValue
    return(output)
  } else {
    tryCatch({
      saveExperimentValues(list(locationValue))
    }, error = function(e) {
      stopUser("Could not save the annotation location")
    })
    return(NULL)
  }
}

#'Deletes a file that shows up as a link
#'
#'Deletes the file referenced as an annotation file in the experiment (in stateKind: "report locations").
#'
#'@param experiment A list that is an experiment object; needs to have values for id, version, and codeName
#'@param testMode boolean used for testing
#'
#'@details Will delete all annotation files associated with the experiment
#'@export
deleteLinkFile <- function(experiment, testMode=FALSE) {
  if (racas::applicationSettings$server.service.external.file.type == "blueimp") {
    locationState <- experiment$lsStates[lapply(experiment$lsStates, function(x) x$"lsKind")=="report locations"]
    
    # Get the location
    if (length(locationState) > 0) {
      locationState <- locationState[[1]]
      
      lsKinds <- lapply(locationState$lsValues, function(x) x$"lsKind")
      
      valuesToDelete <- locationState$lsValues[lsKinds %in% c("annotation file")]
      
      if (length(valuesToDelete) > 0) {
        filesToDelete <- sapply(valuesToDelete,getElement, "fileValue")
        if(testMode) {
          return(filesToDelete)
        } else {
          tryCatch({
            file.remove(paste0(racas::applicationSettings$server.file.server.path,"/", filesToDelete))
          }, error = function(e) {
            stopUser("There was an error deleting the old report file. Please contact your system adminstrator.")
          })
        }
      }
    }
  } else {
    stopUser("Need to set up custom link file deletion")
  }
}
