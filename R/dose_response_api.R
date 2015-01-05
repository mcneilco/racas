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
#' file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas" )
#' simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
#' simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
#' recordedBy <- "bbolt"
#' experimentCode <- "EXPT-00000441"
#' api_doseResponse_experiment(simpleFitSettings, recordedBy, experimentCode)
#' 
#' #Loading fake data first
#' # requires 1. that a protocol named "Target Y binding") be saved first (see \code{\link{api_createProtocol}})
#' #          2. have a valid recordedBy (or that acas is set to client.require.login=false) 
#'  
#' file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas" )
#' simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
#' simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
#' experimentCode <- load_dose_response_test_data()
#' recordedBy <- "bbolt"
#' api_doseResponse_experiment(simpleFitSettings, recordedBy, experimentCode)
api_doseResponse_experiment <- function(simpleFitSettings, recordedBy, experimentCode, testMode = NULL) {
  #     cat("Using fake data")
#   file <- "inst/docs/example-simple-fitsettings-ll4.json"
#   file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas" )
#   simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
#   simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
#   recordedBy <- "bbolt"
#     
#   #system.time(experimentCode <- load_dose_response_test_data())
#   experimentCode <- "EXPT-00000427"
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- TRUE
  myMessenger$logger <- logger(logName = "com.racas.doseresponse.fit.experiment")
  
  myMessenger$logger$debug("updating experiment model fit status status value to running")
  experimentStatusValue <- update_experiment_model_fit_status(experimentCode, "running")
  
  myMessenger$logger$debug("converting simple fit settings to advanced settings")
  fitSettings <- simple_to_advanced_fit_settings(simpleFitSettings)
  
  myMessenger$logger$debug(paste0("getting fit data for ",experimentCode, collapse = ""))
  fitData <- get_fit_data_experiment_code(experimentCode, full_object = TRUE)
  fitData[ , simpleFitSettings := toJSON(simpleFitSettings), by = curveId]
  myMessenger$logger$debug("fitting the data")
  fitData <- dose_response(fitSettings, fitData)
  
  myMessenger$logger$debug("decorating fit data clob values")
  fitData <- add_clob_values_to_fit_data(fitData)
  
  myMessenger$logger$debug("saving the curve data")
  savedCurveIds <- save_dose_response_data(fitData, recordedBy)
  
  myMessenger$logger$debug("updating experiment model fit status value to complete")
  experimentStatusValue <- update_experiment_model_fit_status(experimentCode, "complete")
  
  #Convert the fit data to a response for acas
  myMessenger$logger$debug("getting acas response")
  if(length(myMessenger$userErrors) == 0 & length(myMessenger$errors) == 0 ) {
    response <- fit_data_to_acas_experiment_response(fitData, experimentCode, savedStates$lsTransaction, status = "complete", hasWarning = FALSE, errorMessages = myMessenger$userErrors)
  } else {
    myMessenger$logger$error(paste0("User Errors: ", myMessenger$userErrors, collapse = ","))
    myMessenger$logger$error(paste0("Errors: ", myMessenger$userErrors, collapse = ","))
    response <- fit_data_to_acas_experiment_response(fitData = NULL, -1, status = "error", hasWarning = FALSE, errorMessages = myMessenger$userErrors)
  }
  myMessenger$logger$debug("saving experiment value model fit result html")
  experimentDoseResponseAnalysisResultValue <- update_experiment_model_fit_html(experimentCode, html = response$result$htmlSummary)
  
  myMessenger$logger$debug("responding to acas")
  return(response)
}

api_doseResponse_get_curve_stubs <- function(GET) {  
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- FALSE
  myMessenger$logger <- logger(logName = "com.racas.api.doseresponse.stubs")
  if(is.null(GET$experimentcode) & is.null(GET$curveid)) {
    msg <- "No 'experimentcode' or 'curveid' provided"
    myMessenger$logger$error(msg)
    stop(msg)
  } else {
    if(!is.null(GET$experimentcode)) {
      entityID <- GET$experimentcode
      type <- "experimentCode"
    } else {
      entityID <- GET$curveid
      type <- "curveID"
    }
  }
  #entityID <- "EXPT-00000070"
  myMessenger$logger$debug(paste0("Getting fit data for ",entityID))
  fitData <- get_fit_data_experiment_code(entityID, full_object = FALSE)
  #TODO: 3.1.0 the next line work but not with 3.0.3, check again when data.table is above 1.9.2 (1.9.2 and devel 1.9.3 has lots of 3.1.0 issues)
  #setkey(fitData, codeName)
  myMessenger$logger$debug(paste0("Getting modelHint saved parameter"))
  modelHint <- fitData[1]$modelHint
  myMessenger$logger$debug(paste0("Got modelHint '",modelHint,"'"))
  myMessenger$logger$debug(paste0("Getting sort options '",modelHint,"'"))
  if(fitData[1]$modelHint == "LL.4") {
    sortOptions <- list(list(code = "compoundCode", name = "Compound Code"),
                        list(code = "EC50", name = "EC50"),
                        list(code = "SST", name = "SST"),
                        list(code = "SSE", name = "SSE"),
                        list(code = "rsquare", name = "R^2"),
                        list(code = "userFlagStatus", name = "User Flag Status"),
                        list(code = "userFlagStatus", name = "Algorithm Flag Status"))
  } else {
    msg <- paste0("Model Hint '", modelHint, "' unimplemented for sort options")
    myMessenger$logger$error(msg)
    stop(msg)
  }
  myMessenger$logger$debug(paste0("Get curve attributes"))
  fitData[ , curves := list(list(list(curveid = curveId[[1]], 
                                      algorithmFlagStatus = algorithmFlagStatus[[1]],
                                      userFlagStatus = userFlagStatus[[1]],
                                      category = Category[[1]],
                                      curveAttributes = list(
                                        EC50 = ec50[[1]],
                                        SST =  sst[[1]],
                                        SSE =  sse[[1]],
                                        rsquare = rSquared[[1]],
                                        compoundCode = batchCode[[1]],
                                        algorithmFlagStatus = algorithmFlagStatus[[1]],
                                        userFlagStatus = userFlagStatus[[1]]
                                      )
  ))), by = curveId]
  stubs <- list(sortOptions = sortOptions, curves = fitData$curves)
  myMessenger$logger$debug(paste0("Returning stubs"))
  
  #stop(toJSON(stubs))
  return(stubs)
}

api_doseResponse_update_flag <- function(POST) {
  fitData <- get_fit_data_curve_id(POST$curveid)
  simpleFitSettings <- fromJSON(fitData$ag_values[[1]][lsKind=='fitSettings']$clobValue)
  fitSettings <- simple_to_advanced_fit_settings(simpleFitSettings)
  doseResponse <- dose_response_session(fitSettings = fitSettings, fitData = fitData, flagUser = POST$flagUser, simpleFitSettings = simpleFitSettings)
  deleteSession(doseResponse$sessionID)
  fitData <- add_clob_values_to_fit_data(doseResponse$fitData)
  savedStates <- save_dose_response_data(fitData, recorded_by = POST$user)
  analysisgroupid <- rbindlist(lapply(savedStates$lsStates, function(x) list_to_data.table(x)))$analysisGroup[[1]]$id
  fitData <- get_fit_data_analysis_group_id(analysisgroupid, full_object = TRUE)
  
  fitData[ , curves := list(list(list(curveid = curveId[[1]], 
                                      algorithmFlagStatus = algorithmFlagStatus,
                                      userFlagStatus = userFlagStatus,
                                      category = ag_values[[1]][lsKind == "category", ]$stringValue,
                                      curveAttributes = list(
                                        EC50 = ag_values[[1]][lsKind == "EC50"]$numericValue,
                                        SST =  ag_values[[1]][lsKind == "SST"]$numericValue,
                                        SSE =  ag_values[[1]][lsKind == "SSE"]$numericValue,
                                        rsquare = ag_values[[1]][lsKind == "rSquared"]$numericValue,
                                        compoundCode = ag_values[[1]][lsKind == "batch code"]$codeValue,
                                        algorithmFlagStatus = algorithmFlagStatus,
                                        userFlagStatus = userFlagStatus
                                      )
  ))), by = curveid]
  return(toJSON(fitData$curves[[1]]))
}
api_doseResponse_get_curve_detail <- function(GET, ...) {  
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- FALSE
  myMessenger$logger <- logger(logName = "com.racas.api.doseresponse.detail")
  if(is.null(GET$id) & is.null(GET$analysisgroupid)) {
    msg <- "No GET parameter 'id' or 'analysisgroupid' provided"
    myMessenger$logger$error(msg)
    stop(msg)
  } else {
    if(!is.null(GET$id)) {
      fitData <- get_fit_data_curve_id(GET$id, full_object = TRUE)
    } else {
      fitData <- get_fit_data_analysis_group_id(GET$analysisgroupid, full_object = TRUE)
    }
  }
  
  sessionID <- saveSession()
  
  response <- api_doseResponse_fitData_to_curveDetail(fitData, saved = TRUE, sessionID = sessionID)
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
    reportedValues <- fitData[1]$reportedValuesClob[[1]]
    fitSummary <- fitData[1]$fitSummaryClob[[1]]
    parameterStdErrors <- fitData[1]$parameterStdErrorsClob[[1]]
    curveErrors <- fitData[1]$curveErrorsClob[[1]]
    fitSettings <- fromJSON(fitData[1]$fitSettings)
    fittedParametersList <- list(min = fitData[1]$fittedMin,  max = fitData[1]$fittedMax, ec50 = fitData[1]$fittedEC50, slope = fitData[1]$fittedSlope)
    curveAttributes <- list(EC50 = length0_or_na_to_null(fitData[1]$ec50),
                            Operator = length0_or_na_to_null(fitData[1]$ec50OperatorKind),
                            SST = length0_or_na_to_null(fitData[1]$sst),
                            SSE =  length0_or_na_to_null(fitData[1]$sse),
                            rSquared =  length0_or_na_to_null(fitData[1]$rSquared),
                            compoundCode = length0_or_na_to_null(fitData[1]$batchCode)
    )
    category <- fitData[1]$Category
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
                            compoundCode = fitData[1]$batchCode
    )
    category <- fitData[1]$category[[1]]
  }
  curveid <- fitData[1]$curveId
  algorithmFlagStatus = fitData[1]$algorithmFlagStatus
  userFlagStatus = fitData[1]$userFlagStatus
  points <- fitData[1]$points[[1]]
  #category <- nrow(points[!is.na(flag)])
  points <- split(points, points$responseSubjectValueId)
  names(points) <- NULL
  plotWindow <- get_plot_window(fitData[1]$points[[1]])
  plotWindow[c(1,3)] <- log10(plotWindow[c(1,3)])
  plotData <- list(plotWindow = plotWindow,
                   points  = points,
                   curve = c(type = fitData[1]$modelHint,
                             reported_ec50 = curveAttributes$EC50,
                             reported_operator = curveAttributes$Operator,
                             fittedParametersList)
  )
  if(length(fittedParametersList) == 0) {
    plotData$curve <- NULL
  }
  return(toJSON(list(id = curveid,
                     curveid = curveid,
                     reportedValues = reportedValues,
                     fitSummary = fitSummary,
                     parameterStdErrors = parameterStdErrors,
                     curveErrors = curveErrors,
                     category = category,
                     algorithmFlagStatus = algorithmFlagStatus,
                     userFlagStatus = userFlagStatus,
                     curveAttributes = curveAttributes,
                     plotData = plotData,
                     fitSettings = fitSettings,
                     dirty = !saved,
                     ...
  )))
}

api_doseResponse_save_session <- function(sessionID, user) {
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- TRUE
  myMessenger$logger <- logger(logName = "com.racas.api.doseresponse.save.session")
  myMessenger$logger$debug(paste0("got session id: ", sessionID))
  
  loadSession(sessionID)    
  if(is.null(fitData$model)) {
# If the model is null, then we have not refit the data since loading from the database, just return the object as is
    myMessenger$logger$debug("nothing has changed, returning object without saving")
    response <- api_doseResponse_fitData_to_curveDetail(fitData, sessionID = sessionID)
  } else {
# If the model does exist then we have fit, so save the data
    # delete the session because we don't need it anymore and will be returning a new one
    deleteSession(sessionID)
    myMessenger$logger$debug("adding clob values to fit data")
    fitData <- add_clob_values_to_fit_data(fitData)
    myMessenger$logger$debug("saving the curve data")
    savedCurveIds <- save_dose_response_data(fitData, user)
    GET <- list()
    GET$id <- savedCurveIds[[1]]
    response <- api_doseResponse_get_curve_detail(GET)
  }
  return(response)
}

api_doseResponse_refit <- function(POST) {
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- TRUE
  myMessenger$logger <- logger(logName = "com.racas.api.doseresponse.fit.curve")
  myMessenger$logger$debug("parsing json from acas")
  myMessenger$logger$debug(paste0("got session id: ", POST$sessionID))
  myMessenger$logger$debug("getting updated point flags sent from acas")
  points <- data.table(POST$plotData$points)
  myMessenger$logger$debug("converting simple fit settings to advanced settings")
  fitSettings <- simple_to_advanced_fit_settings(POST$fitSettings, points)
  
  myMessenger$logger$debug("fitting the dose response model")
  doseResponse <- dose_response_session(fitSettings = fitSettings, sessionID = POST$sessionID, simpleFitSettings = POST$fitSettings, flagUser = POST$userFlagStatus, user = POST$user)
  
  myMessenger$logger$debug("converting the fitted data to a response json object")
  fitData <- add_clob_values_to_fit_data(doseResponse$fitData)
  response <- api_doseResponse_fitData_to_curveDetail(fitData, saved = FALSE, sessionID = doseResponse$sessionID)
  
  if(myMessenger$hasErrors()) {
    return(myMessenger$toJSON())
  }
  myMessenger$logger$debug("returning response")
  return(response)
}

api_doseResponse_update_user_flag <- function(sessionID, flagUser, user) {
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- FALSE
  myMessenger$logger <- logger(logName = "com.racas.api.doseresponse.update.curve.user.flag")
  myMessenger$logger$debug(paste0("loading session ", sessionID))
  myMessenger$logger$debug(paste0("setting flag to ", flagUser))
  myMessenger$logger$debug(paste0("setting user to ", user))
  loadSession(sessionID)
  if(flagUser == "NA") {
    flagUser <- as.character(NA)
  }
  updated <- doseResponse_update_user_flag(fitData, flagUser, user)
  if(!myMessenger$hasErrors()) {
    GET <- list()
    GET$analysisgroupid <- fitData$analysisGroupId
    response <- api_doseResponse_get_curve_detail(GET)
  } else {
    response <- myMessenger$toJSON()
  }
  return(response)
}