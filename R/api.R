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
#      file <- system.file("docs", "example-ec50-simple-fitSettings.json", package = "racas" )
#      simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
#      simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
#     recordedBy <- "bbolt"
#   
  #experimentCode <- loadDoseResponseTestData()
  #experimentCode <- "EXPT-00000063"
  
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- FALSE
  myMessenger$logger <- logger(logName = "com.acas.doseresponse.fit.experiment")
  
  myMessenger$logger$debug("Converting simple fit settings to advanced settings")
  myMessenger$captureOutput("fitSettings <- simpleToAdvancedFitSettings(simpleFitSettings)", userError = "Fit settings error")
  
  myMessenger$logger$debug(paste0("Getting fit data for ",experimentCode))
  myMessenger$captureOutput("fitData <- getFitData(experimentCode)", userError = "Error when fetching the experiment curve data", continueOnError = FALSE)
  fitData[ , simpleFitSettings := toJSON(simpleFitSettings), by = curveid]
  myMessenger$logger$debug("Fitting the data")
  myMessenger$captureOutput("fitData <- doseResponse.fitData(fitSettings, fitData)", userError = "Error when fitting the experiment curve data", continueOnError = FALSE)
  
  myMessenger$logger$debug("Decorating fit data clob values")
  myMessenger$captureOutput("fitData <- doseResponse_add_clob_values(fitData)", continueOnError = FALSE)
  
  myMessenger$logger$debug("Saving the curve data")
  myMessenger$captureOutput("savedStates <- saveDoseResponseData(fitData, recordedBy)", userError = "Error saving the experiment curve data", continueOnError = FALSE)
  
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

api_doseResponse_get_curve_stubs <- function(GET) {  
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
  #experimentCode <- "EXPT-00000063"
  myMessenger$logger$debug(paste0("Getting fit data for ",experimentCode))
  myMessenger$captureOutput("fitData <- getFitData(experimentCode, include = 'analysisgroupvalues')", userError = "Error when fetching the experiment curve data", continueOnError = FALSE)
  setkey(fitData, codeName)
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

api_doseResponse_get_curve_detail <- function(GET, ...) {  
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
  
  fitData <- getFitData(id, type = "analysisGroupID", include = "fullobject")
  sessionID <- saveSession()
  
  response <- api_doseResponse_fitData_to_curveDetail(fitData, sessionID = sessionID)
  return(response)
}
#' fitData object to json response
#'
#' Converts a fitData object to a json response to return to the ACAS GUI
#' 
#' @param a fitData object
#' @param ... addition arguments to be passed to \code{\link{toJSON}}
#' @return A json object of the fitData and any other objects coerced to json by ... \code{\link{toJSON}}
#' @export
#' @examples
#' #Load and example fitData object
#' data("example-ec50-fitData-fitted")
#' #FitData object plus the "cars" data to a json string
#' api_doseResponse_fitData_to_curveDetail(fitData, cars)
api_doseResponse_fitData_to_curveDetail <- function(fitData, saved = TRUE,...) {
  if(saved) {
    reportedValues <- fitData[1]$parameters[[1]][lsKind == "reportedValuesClob"]$clobValue
    fitSummary <- fitData[1]$parameters[[1]][lsKind == "fitSummaryClob"]$clobValue
    parameterStdErrors <- fitData[1]$parameters[[1]][lsKind == "parameterStdErrorsClob"]$clobValue
    curveErrors <- fitData[1]$parameters[[1]][lsKind == "curveErrorsClob"]$clobValue
    fitSettings <- fromJSON(fitData[1]$parameters[[1]][lsKind == "fitSettings"]$clobValue)
    fittedParameters <- fitData[1]$parameters[[1]][grepl("Fitted ",lsKind), ][ , c("lsKind","numericValue"), with = FALSE]
    fittedParametersList <- list()
    fittedParametersList[1:nrow(fittedParameters)] <- fittedParameters$numericValue
    names(fittedParametersList) <- tolower(gsub('Fitted ', '', fittedParameters$lsKind))
    curveAttributes <- list(EC50 = fitData[1]$parameters[[1]][lsKind == "EC50"]$numericValue,
                            Operator = fitData[1]$parameters[[1]][lsKind == "EC50"]$valueOperator,
                            SST = fitData[1]$parameters[[1]][lsKind == "SST"]$numericValue,
                            SSE =  fitData[1]$parameters[[1]][lsKind == "SSE"]$numericValue,
                            rSquared =  fitData[1]$parameters[[1]][lsKind == "rSquared"]$numericValue,
                            compoundCode = fitData[1]$parameters[[1]][lsKind == "batch code"]$codeValue
    )
    category <- fitData[1]$parameters[[1]][lsKind == "category"]$stringValue
  
  } else {
    reportedValues <- fitData[1]$reportedValuesClob[[1]]
    fitSummary <- fitData[1]$fitSummaryClob[[1]]
    parameterStdErrors <- fitData[1]$parameterStdErrorsClob[[1]]
    curveErrors <- fitData[1]$curveErrorsClob[[1]]
    fitSettings = fromJSON(fitData[1]$simpleFitSettings)
    fittedParametersList <- fitData$fittedParameters[[1]]
    curveAttributes <- list(EC50 = fitData[1]$reportedParameters[[1]]$ec50$value,
                            Operator = fitData[1]$reportedParameters[[1]]$ec50$operator,
                            SST = fitData[1]$goodnessOfFit.model[[1]]$SST,
                            SSE =  fitData[1]$goodnessOfFit.model[[1]]$SSE,
                            rSquared =  fitData[1]$goodnessOfFit.model[[1]]$rSquared,
                            compoundCode = fitData[1]$parameters[[1]][lsKind == "batch code"]$codeValue
    )
    category <- fitData[1]$category[[1]]
    
  }
  curveid <- fitData[1]$curveid
  algorithmApproved = fitData[1]$approved[[1]]
  points <- fitData[1]$points[[1]][ , c("response_sv_id", "dose", "doseunits", "response", "responseunits", "flag", "flagchanged"), with = FALSE]
  #category <- nrow(points[!is.na(flag)])
  points <- split(points, points$response_sv_id)
  names(points) <- NULL
  plotData <- list(plotWindow = plotWindow(fitData[1]$points[[1]]),
                   points  = points,
                   curve = c(type = fitData[1]$modelHint,
                             fittedParametersList)
  )
  return(toJSON(list(curveid = curveid,
                     reportedValues = reportedValues,
                     fitSummary = fitSummary,
                     parameterStdErrors = parameterStdErrors,
                     curveErrors = curveErrors,
                     category = category,
                     algorithmApproved = algorithmApproved,
                     curveAttributes = curveAttributes,
                     plotData = plotData,
                     fitSettings = fitSettings,
                     ...
  )))
}
doseResponse_add_clob_values <- function(fitData) {
    fitData[ , c("reportedValuesClob", "fitSummaryClob", "parameterStdErrorsClob", "curveErrorsClob") := {
      if(fitConverged) {
        reportedValues <- flattenListToDataTable(reportedParameters[[1]])
        reportedValues <- reportedValues[ , value := {
          if(exists("operator")) {
            paste(ifelse(is.na(operator), "",operator), value)
          } else {
            value
          }}]
        reportedValuesClob <- objToHTMLTableString(reportedValues[ , c("name", "value"), with = FALSE], include.colnames = FALSE)
        fitSummaryClob <- captureOutput(summary(model[[1]]))
        goodnessOfFit.parameters <- flattenListToDataTable(goodnessOfFit.parameters[[1]])
        goodnessOfFit.parameters[ , c("name", "type") := {sp <- strsplit(name, "\\.")[[1]]
                                                            list(name = sp[[1]], type = sp[[2]])}, by = c("V1", "name")]
        goodnessOfFit.parameters <- dcast.data.table(goodnessOfFit.parameters, name ~ type, value.var = "V1")
        parameterStdErrors <- objToHTMLTableString(goodnessOfFit.parameters)
        parameterStdErrorsClob <- parameterStdErrors
        curveErrorsClob <- objToHTMLTableString(flattenListToDataTable(goodnessOfFit.model[[1]])[, c("name", "V1"), with = FALSE], include.colnames = FALSE)
        list(reportedValuesClob = list(reportedValuesClob), fitSummaryClob = list(fitSummaryClob), parameterStdErrorsClob = list(parameterStdErrorsClob), curveErrorsClob = list(curveErrorsClob))
      } else {
        list(reportedValuesClob = list(NULL), fitSummaryClob = list(NULL), parameterStdErrorsClob= list(NULL), curveErrorsClob = list(NULL))
      }
    }, by = curveid]
  return(fitData)
}

api_doseResponse_fit_curve <- function(postData) {  
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- TRUE
  myMessenger$logger <- logger(logName = "com.acas.api.doseresponse.fit.curve")
  
  myMessenger$logger$debug("parsing json from acas")
  myMessenger$logger$debug(paste0("got post data: ", capture.output(cat(postData))))
  
  POST <- jsonlite::fromJSON(postData)
  myMessenger$logger$debug(paste0("got session id: ", POST$sessionID))
  
  if(is.null(POST$persist)) POST$persist <- FALSE
  if(POST$persist == TRUE) {
    loadSession(POST$sessionID)
    myMessenger$logger$debug("adding clob values to fit data")    
    fitData <- doseResponse_add_clob_values(fitData)
    myMessenger$logger$debug("saving the curve data")
    myMessenger$logger$debug(exists("fitData"))
    myMessenger$captureOutput("savedStates <- saveDoseResponseData(fitData, POST$user)", userError = "Error saving the experiment curve data", continueOnError = FALSE)
    response <- TRUE
  } else {
    myMessenger$logger$debug("getting updated point flags sent from acas")
    points <- data.table(POST$plotData$points)
    myMessenger$logger$debug("converting simple fit settings to advanced settings")
    myMessenger$captureOutput("fitSettings <- simpleToAdvancedFitSettings(POST$fitSettings, points)", userError = "Fit settings error")
  
    myMessenger$logger$debug("fitting the dose response model")
    myMessenger$captureOutput("doseResponse <- doseResponse(fitSettings, sessionID = POST$sessionID, simpleFitSettings = POST$fitSettings)", userError = "Error when fitting the experiment curve data", continueOnError = FALSE)
    
    myMessenger$logger$debug("converting the fitted data to a response json object")
    fitData <- doseResponse_add_clob_values(doseResponse$fitData)
  
    myMessenger$captureOutput("response <- api_doseResponse_fitData_to_curveDetail(fitData, saved = FALSE, sessionID = doseResponse$sessionID)", userError = "Error converting Fit to a Response", continueOnError = FALSE)
  }
  myMessenger$logger$debug("returning response")
  return(response)
}
