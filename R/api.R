#' Create acas protocol
#'
#' This function creates an acas protocol
#'
#' @param protocolName
#' @param shortDescription
#' @return recordedBy
#' @keywords register, create, protocol
#' @export
#' @examples
#' api_createProtocol(c("Brian Test Protocol2"), shortDescription = "Test Protocol", recordedBy = "bbolt")
api_createProtocol <- function(protocolName, shortDescription, recordedBy, stopOnAnyExist = TRUE) {
  if(is.null(protocolName)){
    stop("Protocol names cannot be null")
  }
  if(any(unlist(lapply(protocolName, function(x) x=="")))) {
    stop("Protocol names cannot equal \"\"")
  }
  if(any(duplicated(protocolName))) {
    stop("Protocol names must be unique")
  }
  if(is.null(recordedBy)){
    stop("Recorded by cannot be null")
  }
  if(recordedBy == ""){
    stop("Recorded by cannot equal \"\"")
  }
  
  protocolExistence <- checkExistence(protocolName, "protocolName")
  
  if(stopOnAnyExist) {
    if(any(unlist(protocolExistence))) {
      stop(paste0("Protocols already registered: '",paste(names(protocolExistence[unlist(protocolExistence)]),collapse = "', '")),"'")
    }
  }
  #Remove protocols that already exist
  protocolExistence[unlist(protocolExistence)] <- NULL
  protocolsToCreate <- names(protocolExistence)
  
  # Create the protocols
  
  createSimpleProtocol <- function(x) {
    options("scipen"=15)
    protocolLabels <- list()
    protocolLabels[[length(protocolLabels)+1]] <- createProtocolLabel(lsTransaction = lsTransaction, 
                                                                      recordedBy=recordedBy, 
                                                                      lsType="name", 
                                                                      lsKind="protocol name",
                                                                      labelText=x,
                                                                      preferred=TRUE)
    protocol <- createProtocol(lsTransaction = lsTransaction,
                               shortDescription=shortDescription,  
                               recordedBy=recordedBy, 
                               protocolLabels=protocolLabels
    )
    saveProtocol(protocol)
    return(protocol)
  }
  lsTransaction <- createLsTransaction()$id
  createdProtocols <- lapply(protocolsToCreate, createSimpleProtocol)
  return(createdProtocols)
}

#' Check for protocol or experiment existence
#'
#' This function checks to see if a given protocol or experiment entity exists
#'
#' @param x entity label or code names of the same type
#' @param type entity type of x (protocolName, protocolCodeName, experimentName or experimentCodeName)
#' @return boolean list with names that match x
#' @keywords check, existence, exists
#' @export
#' @examples
#' checkExistence("ADME_Human Liver Microsome Stability", "protocolName")
#' checkExistence(c("ADME_Human Liver Microsome Stability","some other protocol name"), "protocolName")
#' checkExistence("PROT-00000001", "protocolCodeName")
#' checkExistence("EXPT-00000001", "experimentCodeName")
checkExistence <- function(x, type = c("protocolName", "experimentName", "protocolCodeName", "experimentCodeName")) {
  entities <- getEntity(x, type)
  entityExists <- lapply(entities, function(x) length(x)>0)
  return(entityExists)
}
#' Retrieves a list of entities (experiments or prototocols) by type
#'
#' This function retrieves a list of protocols or experiments by code name or label
#'
#' @param x entity names or code names of the same type 
#' @param type entity type of x (protocolName, protocolCodeName, experimentName or experimentCodeName)
#' @return a list of the entities returned from the server
#' @keywords get, entity
#' @export
#' @examples
#' getEntity("ADME_Human Liver Microsome Stability", "protocolName")
#' getEntity(c("ADME_Human Liver Microsome Stability","some other protocol name"), "protocolName")
#' getEntity("PROT-00000001", "protocolCodeName")
#' getEntity("EXPT-00000001", "experimentCodeName")
getEntity <- function(x, type = c("protocolName", "experimentName", "protocolCodeName", "experimentCodeName")) {
  if(length(type) > 1) {
    stop("Must specify one type")
  }
  type <- match.arg(type)
  servicePath <- switch(type,
                        protocolName = "protocols?FindByProtocolName&protocolName=",
                        protocolCodeName = "protocols?FindByCodeName&codeName=",
                        experimentName = "experiments?FindByExperimentName&experimentName=",
                        experimentCodeName = "experiments?FindByCodeName&codeName="
  )
  urls <- paste0(applicationSettings$client.service.persistence.fullpath, servicePath, lapply(x, URLencode, reserved = TRUE))
  entitiesJSON <- lapply(urls, getURL)
  entitiesList <- lapply(entitiesJSON, fromJSON)
  names(entitiesList) <- x
  return(entitiesList)
}
#' Fits experiment dose response data
#'
#' This function retrieves the dose response data for a given experiment code, refits it using the simple settings and then saves the data back to the database
#'
#' @param simpleFitSettings list of fit settings (see details) 
#' @param recordedBy character user that curve data will be saved as
#' @param experimentCode character saved experiment code that contains dose response data
#' @param testMode logical unimplemented
#' @return a list of the entities returned from the server
#' @keywords dose, response, fit, model, experimet
#' @export
#' @examples
#' 
#' 
#' file <- system.file("docs", "example-ec50-simple-fitSettings.json", package = "racas" )
#' simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
#' simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
#' recordedBy <- "bbolt"
#' experimentCode <- "EXPT-00000441"
#' api_doseResponse.experiment(simpleFitSettings, recordedBy, experimentCode)
#' 
#' #Loading fake data first
#' # requires 1. that a protocol named "Target Y binding") be saved first (see \code{\link{api_createProtocol}})
#' #          2. have a valid recordedBy (or that acas is set to client.require.login=false) 
#'  
#' file <- system.file("docs", "example-ec50-simple-fitSettings.json", package = "racas" )
#' simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
#' simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
#' experimentCode <- loadDoseResponseTestData()
#' recordedBy <- "bbolt"
#' api_doseResponse.experiment(simpleFitSettings, recordedBy, experimentCode)
api_doseResponse.experiment <- function(simpleFitSettings, recordedBy, experimentCode, testMode = NULL) {
#     cat("Using fake data")
#     file <- "inst/docs/example-ec50-simple-fitSettings.json"
     file <- system.file("docs", "example-ec50-simple-fitSettings.json", package = "racas" )
     simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
     simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
#     recordedBy <- "bbolt"
  
  #experimentCode <- loadDoseResponseTestData()
  #experimentCode <- "EXPT-00000026"
  
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- FALSE
  myMessenger$logger <- logger(logName = "com.acas.fit.doseresponse.experiment")
  
  myMessenger$logger$debug("Converting simple fit settings to advanced settings")
  myMessenger$captureOutput("fitSettings <- simpleToAdvancedFitSettings(simpleFitSettings)", userError = "Fit settings error")
  
  myMessenger$logger$debug(paste0("Getting fit data for ",experimentCode))
  myMessenger$captureOutput("fitData <- getFitData.experimentCode(experimentCode)", userError = "Error when fetching the experiment curve data", continueOnError = FALSE)
  
  myMessenger$logger$debug("Fitting the data")
  myMessenger$captureOutput("fitData <- doseResponse.fitData(fitSettings, fitData)", userError = "Error when fitting the experiment curve data", continueOnError = FALSE)
  
  myMessenger$logger$debug("Saving the curve data")
  myMessenger$captureOutput("savedStates <- saveDoseResponseData(fitData, recordedBy, experimentCode = experimentCode)", userError = "Error saving the experiment curve data", continueOnError = FALSE)
  
  #Convert the fit data to a response for acas
  myMessenger$logger$debug("Responding to ACAS")
  if(length(myMessenger$userErrors) == 0 & length(myMessenger$errors) == 0 ) {
    response <- fitDataToResponse.acas(fitData, savedStates$lsTransaction, status = "complete", hasWarning = FALSE, errorMessages = myMessenger$userErrors)
  } else {
    myMessenger$logger$error(paste0("User Errors: ", myMessenger$userErrors, collapse = ","))
    myMessenger$logger$error(paste0("Errors: ", myMessenger$userErrors, collapse = ","))
    response <- fitDataToResponse.acas(fitData = NULL, -1, status = "error", hasWarning = FALSE, errorMessages = myMessenger$userErrors)
  }
  return(response)
}

api_doseResponse_stubs <- function(GET) {  
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- FALSE
  myMessenger$logger <- logger(logName = "com.acas.api.doseresponse.stubs")
  if(is.null(GET$experimentcode)) {
    msg <- "No 'experimentcode' provided"
    myMessenger$logger$error(msg)
    stop(msg)
  } else {
    experimentCode <- GET$experimentcode
  }
  #experimentCode <- "EXPT-00000026"
  myMessenger$logger$debug(paste0("Getting fit data for ",experimentCode))
  myMessenger$captureOutput("fitData <- getFitData.experimentCode(experimentCode, include = 'analysisgroupvalues')", userError = "Error when fetching the experiment curve data", continueOnError = FALSE)
  myMessenger$logger$debug(paste0("Getting modelHint saved parameter"))
  modelHint <- fitData[1]$modelHint
  myMessenger$logger$debug(paste0("Got modelHint '",modelHint,"'"))
  myMessenger$logger$debug(paste0("Getting sort options '",modelHint,"'"))
  if(fitData[1]$modelHint == "LL.4") {
    sortOptions <- list(list(code = "compoundCode", name = "Compound Code"),
                       list(code = "EC50", name = "EC50"),
                       list(code = "SST", name = "SST"),
                       list(code = "SSE", name = "SSE"),
                       list(code = "rsquare", name = "R^2"))
  } else {
    msg <- paste0("Model Hint '", modelHint, "' unimplemented for sort options")
    myMessenger$logger$error(msg)
    stop(msg)
  }
   myMessenger$logger$debug(paste0("Get curve attributes"))
  fitData[ , curves := list(list(list(curveid = curveid[[1]], 
                  algorithmApproved = TRUE,
                  userApproved = TRUE,
                  category = parameters[[1]][lsKind == "category", ]$stringValue,
                  curveAttributes = list(
                    EC50 = parameters[[1]][lsKind == "EC50"]$numericValue,
                    SST =  parameters[[1]][lsKind == "SST"]$numericValue,
                    SSE =  parameters[[1]][lsKind == "SSE"]$numericValue,
                    rsquare = parameters[[1]][lsKind == "rSquared"]$numericValue,
                    compoundCode = parameters[[1]][lsKind == "batch code"]$codeValue
                  )
                  ))), by = curveid]
    stubs <- list(sortOptions = sortOptions, curves = fitData$curves)
    myMessenger$logger$debug(paste0("Returning stubs"))
  
    #stop(toJSON(stubs))
    return(stubs)
}

api_doseResponse_detail <- function(GET) {  
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- FALSE
  myMessenger$logger <- logger(logName = "com.acas.api.doseresponse.detail")
  if(is.null(GET$id)) {
    msg <- "No GET parameter 'id' provided"
    myMessenger$logger$error(msg)
    stop(msg)
  } else {
    id <- GET$id
  }
  file <- system.file("docs", "example-ec50-simple-fitSettings.json", package = "racas")
  simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
  simpleBulkDoseResponseFitRequest <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
  fitSettings <- simpleToAdvancedFitSettings(simpleBulkDoseResponseFitRequest)

  fitResponse <- doseResponse(fitSettings, curveids = id)
  response <- fitDataToResponse.curation(fitData = fitResponse$fitData, sessionID = fitResponse$sessionID, fitSettings = simpleBulkDoseResponseFitRequest)
  return(response)
}
