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
#' modelFitType <- "4 parameter D-R"
#' modelFit <- racas::ll4
#' api_doseResponse_experiment(simpleFitSettings, modelFitType, recordedBy, experimentCode, modelFit)
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
api_doseResponse_experiment <- function(simpleFitSettings, modelFitType, recordedBy, experimentCode, testMode = NULL, modelFit) {
#   file <- system.file("docs", "example-simple-fitsettings-ki.json", package = "racas" )
#   simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
#   simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
#   recordedBy <- "bbolt"
#     
#   #system.time(experimentCode <- load_dose_response_test_data())
#   experimentCode <- "EXPT-00000427"
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- FALSE
  myMessenger$logger <- logger(logName = "com.racas.doseresponse.fit.experiment")
  on.exit({
    rolledback <- any(unlist(lapply(myMessenger$infos, function(x) x$message=="rolledback")))
    if(myMessenger$hasErrors()) {
      myMessenger$logger$error(paste0("User Errors: ", myMessenger$userErrors, collapse = ","))
      myMessenger$logger$error(paste0("Errors: ", myMessenger$userErrors, collapse = ","))
      response <- fit_data_to_acas_experiment_response(fitData = NULL, experimentCode, transactionId = -1, status = "error", hasWarning = FALSE, errorMessages = c(myMessenger$userErrors,lapply(myMessenger$errors, function(x) {x$message})))  
    } else {
      response <- fit_data_to_acas_experiment_response(fitData = NULL, experimentCode, transactionId = -1, status = "error", hasWarning = FALSE, errorMessages = "There was an error fitting curves")
    }
    
    if(rolledback) {
      update_experiment_model_fit_status(experimentCode, "error")
      myMessenger$logger$debug("saving experiment value model fit result html")
      experimentDoseResponseAnalysisResultValue <- update_experiment_model_fit_html(experimentCode, html = response$result$htmlSummary)
    } else {
      if(!is.null(experimentStatus)) {
        update_experiment_model_fit_status(experimentCode, experimentStatus)
      } else {
        update_experiment_model_fit_status(experimentCode, "error")
      }
    }
  
    return(response)
  })
  
  myMessenger$logger$debug("getting current experiment model fit status status to see if this experiment is already running")
  experimentStatus <- get_experiment_model_fit_status(experimentCode)
  if(!is.null(experimentStatus)) {
    if(experimentStatus == "running") {
      myMessenger$logger$warn("experiment model fit status is 'running', returning without fitting")
      response <- fit_data_to_acas_experiment_response(fitData = NULL, experimentCode, transactionId = -1, status = "running", hasWarning = FALSE, errorMessages = c("Model is already being fit, please wait for the current model fit to finish before refitting"))  
      on.exit()
      return(response)
    } else {
      myMessenger$logger$debug("experiment status is not 'running', this is a refit")
      refit <- TRUE
    }
  } else {
    refit <- FALSE
  }
  
  myMessenger$logger$debug("updating experiment model fit status status value to running")
  experimentStatusValue <- update_experiment_model_fit_status(experimentCode, "running")
  
  myMessenger$logger$debug(paste0("getting fit data for ",experimentCode, collapse = ""))
  myMessenger$capture_output(fitData <- get_fit_data_experiment_code(experimentCode, modelFitType, full_object = TRUE, modelFit = modelFit))
  if(myMessenger$hasErrors()) {
    return()
  }
  fitData[ , renderingHint := modelFitType]
  fitData[ , modelFit := list(list(modelFit))]
  fitData[ , simpleFitSettings := toJSON(simpleFitSettings), by = curveId]

  myMessenger$logger$debug("converting simple fit settings to advanced settings")
  fitSettings <- simple_to_advanced_fit_settings(modelFit$default_fit_settings, simpleFitSettings, modelFit$simple_to_advanced_fittings_function)
  
  #If refitting, then we want to set the algorithm and user flags back to a blank slate
  if(refit) {
    fitData[ , c("userFlagStatus", "algorithmFlagStatus") := list("", "")]
    fitData[ , points := list(list(remove_point_flags(points[[1]], flagKindsToRemove = c("algorithm", "user")))), by = curveId]
  }

  myMessenger$logger$debug("fitting the data")
  fitData <- dose_response(fitSettings, fitData)
  
  myMessenger$logger$debug("decorating fit data clob values")
  fitData <- add_clob_values_to_fit_data(fitData)
  
  myMessenger$logger$debug("saving the curve data")
  savedCurveIds <- save_dose_response_data(fitData, recordedBy)
  
  #Removing the on.exit function that updates status model fit status to error
  on.exit()
  
  myMessenger$logger$debug("updating experiment model fit status value to complete")
  experimentStatusValue <- update_experiment_model_fit_status(experimentCode, "complete")
  myMessenger$logger$debug(paste0("updating experiment model fit type value to ",modelFitType))  
  experimentStatusValue <- update_experiment_model_fit_type(experimentCode, modelFitType)
  myMessenger$logger$debug(paste0("updating experiment model fit parameters clob"))  
  experimentStatusValue <- update_experiment_model_fit_parameters(experimentCode, toJSON(simpleFitSettings))
  
  #Convert the fit data to a response for acas
  myMessenger$logger$debug("getting acas response")
  response <- fit_data_to_acas_experiment_response(fitData, experimentCode, -1, status = "complete", hasWarning = FALSE, errorMessages = myMessenger$userErrors)
  
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
  myMessenger$logger$debug(paste0("getting model fit type for ",entityID))
  modelFitType <- get_experiment_model_fit_type(entityID)
  modelFitStatus <- get_experiment_model_fit_status(entityID, fullValue = TRUE)
  if(is.null(modelFitStatus) && type == "experimentCode") {
    stop("experiment not fit")
  } else if(modelFitStatus$lsState$experiment$deleted || modelFitStatus$lsState$experiment$ignored) {
    stop("experiment has been deleted")
  }
  modelFit <- get_model_fit_from_type_code(modelFitType)
  
  myMessenger$logger$debug(paste0("getting fit data for ",entityID))
  fitData <- get_fit_data_experiment_code(entityID, modelFitType, full_object = FALSE, modelFit = modelFit)
  #TODO: 3.1.0 the next line work but not with 3.0.3, check again when data.table is above 1.9.2 (1.9.2 and devel 1.9.3 has lots of 3.1.0 issues)
  #setkey(fitData, codeName)
  myMessenger$logger$debug(paste0("Getting renderingHint saved parameter"))
  renderingHint <- fitData[1]$renderingHint
  myMessenger$logger$debug(paste0("Got renderingHint '",renderingHint,"'"))
  myMessenger$logger$debug(paste0("Getting sort options '",renderingHint,"'"))
  sortOptions <- modelFit$sortOptions
                          
  myMessenger$logger$debug(paste0("Get curve attributes"))
  fitData[ , curves := {
    list(list(list(curveid = curveId[[1]], 
                   algorithmFlagStatus = algorithmFlagStatus[[1]],
                   userFlagStatus = userFlagStatus[[1]],
                   category = category[[1]],
                   curveAttributes = modelFit$get_curve_attributes(.SD))))
  }, by = curveId]

  stubs <- list(sortOptions = sortOptions, curves = fitData$curves)
  myMessenger$logger$debug(paste0("Returning stubs"))
  
  #stop(toJSON(stubs))
  return(stubs)
}

api_doseResponse_update_flag <- function(POST, modelFit) {
  fitData <- get_fit_data_curve_id(POST$curveid)
  simpleFitSettings <- fromJSON(fitData$fitSettings)
  #fitSettings <- simple_to_advanced_fit_settings(simpleFitSettings, renderingHint = POST$curveAttributes$renderingHint)
  fitSettings <- simple_to_advanced_fit_settings(modelFit$default_fit_settings, simpleFitSettings, modelFit$simple_to_advanced_fittings_function)
  
  doseResponse <- dose_response_session(fitSettings = fitSettings, fitData = fitData, flagUser = POST$userFlagStatus, simpleFitSettings = simpleFitSettings, modelFit = modelFit)
  deleteSession(doseResponse$sessionID)
  fitData <- add_clob_values_to_fit_data(doseResponse$fitData)
  savedCurveID <- save_dose_response_data(fitData, recorded_by = POST$user)
  fitData <- get_fit_data_curve_id(savedCurveID, full_object = TRUE)
  
  fitData[ , curves := list(list(list(curveid = curveId[[1]], 
                                      algorithmFlagStatus = algorithmFlagStatus[[1]],
                                      userFlagStatus = userFlagStatus[[1]],
                                      category = category[[1]],
                                      curveAttributes = modelFit$get_curve_attributes(.SD)
  ))), by = curveId]
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
  fitData[, modelFit := list(list(get_model_fit_from_type_code(fitData[1]$renderingHint)))]
  
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
  if(fitData$category %in% c("inactive","potent")) {
    overRideMaxMin <- mean(fitData$points[[1]][userFlagStatus!="knocked out" & preprocessFlagStatus!="knocked out" & algorithmFlagStatus!="knocked out" & tempFlagStatus!="knocked out",]$response)    
  } else {
    overRideMaxMin <- NA
  }
  if(saved) {
    reportedValues <- fitData[1]$reportedValuesClob[[1]]
    fitSummary <- fitData[1]$fitSummaryClob[[1]]
    parameterStdErrors <- fitData[1]$parameterStdErrorsClob[[1]]
    curveErrors <- fitData[1]$curveErrorsClob[[1]]
    fitSettings <- fromJSON(fitData[1]$fitSettings)
    
    fittedParametersList <- fitData[1]$modelFit[[1]]$get_saved_fitted_parameters(fitData, overRideMaxMin)
    curveAttributes <- fitData[1]$modelFit[[1]]$get_curve_attributes(fitData)
    category <- fitData[1]$category
  } else {
    reportedValues <- fitData[1]$reportedValuesClob[[1]]
    fitSummary <- fitData[1]$fitSummaryClob[[1]]
    parameterStdErrors <- fitData[1]$parameterStdErrorsClob[[1]]
    curveErrors <- fitData[1]$curveErrorsClob[[1]]
    fitSettings = fromJSON(fitData[1]$simpleFitSettings)
    fittedParametersList <- fitData$fittedParameters[[1]]
    if(!is.null(fittedParametersList$max) && !is.na(overRideMaxMin)) {
      fittedParametersList$max <- overRideMaxMin
    }
    if(!is.null(fittedParametersList$min) && !is.na(overRideMaxMin)) {
      fittedParametersList$min <- overRideMaxMin
    }
    curveAttributes <- fitData[1]$modelFit[[1]]$get_curve_attributes(fitData, saved = FALSE)
    category <- fitData[1]$category[[1]]
  }
  curveid <- fitData[1]$curveId[[1]]
  algorithmFlagStatus = fitData[1]$algorithmFlagStatus[[1]]
  userFlagStatus = fitData[1]$userFlagStatus[[1]]
  points <- fitData[1]$points[[1]]
  renderingHint <- fitData[1]$renderingHint[[1]]
  points <- split(points, points$responseSubjectValueId)
  names(points) <- NULL
  protocol_display_values <- get_protocol_curve_display_min_and_max_by_curve_id(curveid)
  logDose <- TRUE
  if(fitData[1]$renderingHint %in% c("Michaelis-Menten", "Substrate Inhibition")) logDose <- FALSE
  plotWindow <- get_plot_window(fitData[1]$points[[1]], logDose = logDose)
  if(logDose) plotWindow[c(1,3)] <- log10(plotWindow[c(1,3)])
  plotWindow[c(2,4)] <- c(max(protocol_display_values$ymax,plotWindow[2], na.rm = TRUE),min(protocol_display_values$ymin,plotWindow[4], na.rm = TRUE))
  plotData <- list(plotWindow = plotWindow,
                                 points  = points,
                                 curve = c(type = fitData[1]$renderingHint,
                                           curveAttributes = list(curveAttributes),
                                           fittedParametersList)
                     )
                            
#   if(length(fittedParametersList) == 0) {
#     plotData$curve <- NULL
#   }
  return(toJSON(list(id = curveid,
                     compoundCode = fitData[1]$batchCode,
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
                     renderingHint = renderingHint,
                     ...
  )))
}

api_doseResponse_save_session <- function(sessionID, user) {
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- TRUE
  myMessenger$logger <- logger(logName = "com.racas.api.doseresponse.save.session")
  myMessenger$logger$debug(paste0("got session id: ", sessionID))
  
  loadSession(sessionID)    
  if(!"model" %in% names(fitData)) {
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

api_doseResponse_refit <- function(POST, modelFit) {
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- TRUE
  myMessenger$logger <- logger(logName = "com.racas.api.doseresponse.fit.curve")
  myMessenger$logger$debug("parsing json from acas")
  myMessenger$logger$debug(paste0("got session id: ", POST$sessionID))
  myMessenger$logger$debug("getting updated point flags sent from acas")
  points <- data.table(POST$plotData$points)
  
  myMessenger$logger$debug("converting simple fit settings to advanced settings")
  fitSettings <- simple_to_advanced_fit_settings(modelFit$default_fit_settings, POST$fitSettings, modelFit$simple_to_advanced_fittings_function, points)
  
  myMessenger$logger$debug("fitting the dose response model")
  doseResponse <- dose_response_session(fitSettings = fitSettings, sessionID = POST$sessionID, simpleFitSettings = POST$fitSettings, flagUser = POST$userFlagStatus, user = POST$user, modelFit = modelFit)
  
  myMessenger$logger$debug("converting the fitted data to a response json object")
  fitData <- add_clob_values_to_fit_data(doseResponse$fitData)
  response <- api_doseResponse_fitData_to_curveDetail(fitData, saved = FALSE, sessionID = doseResponse$sessionID)
  
  if(myMessenger$hasErrors()) {
    return(myMessenger$toJSON())
  }
  myMessenger$logger$debug("returning response")
  return(response)
}

sortOptions.LL4 <- list(
  list(code = "compoundCode", name = "Compound Code"),
  list(code = "EC50", name = "EC50"),
  list(code = "SST", name = "SST"),
  list(code = "SSE", name = "SSE"),
  list(code = "rsquare", name = "R^2"),
  list(code = "userFlagStatus", name = "User Flag Status"),
  list(code = "userFlagStatus", name = "Algorithm Flag Status")
)
sortOptions.LL4IC50 <- list(
  list(code = "compoundCode", name = "Compound Code"),
  list(code = "IC50", name = "IC50"),
  list(code = "SST", name = "SST"),
  list(code = "SSE", name = "SSE"),
  list(code = "rsquare", name = "R^2"),
  list(code = "userFlagStatus", name = "User Flag Status"),
  list(code = "userFlagStatus", name = "Algorithm Flag Status")
)
sortOptions.LL4IC50DMax <- list(
  list(code = "compoundCode", name = "Compound Code"),
  list(code = "IC50", name = "IC50"),
  list(code = "DMax", name = "DMax"),
  list(code = "SST", name = "SST"),
  list(code = "SSE", name = "SSE"),
  list(code = "rsquare", name = "R^2"),
  list(code = "userFlagStatus", name = "User Flag Status"),
  list(code = "userFlagStatus", name = "Algorithm Flag Status")
)
sortOptions.ki <- list(
  list(code = "compoundCode", name = "Compound Code"),
  list(code = "Ki", name = "Ki"),
  list(code = "SST", name = "SST"),
  list(code = "SSE", name = "SSE"),
  list(code = "rsquare", name = "R^2"),
  list(code = "userFlagStatus", name = "User Flag Status"),
  list(code = "userFlagStatus", name = "Algorithm Flag Status")
)
sortOptions.MM2 <- list(
  list(code = "compoundCode", name = "Compound Code"),
  list(code = "Km", name = "Km"),
  list(code = "SST", name = "SST"),
  list(code = "SSE", name = "SSE"),
  list(code = "rsquare", name = "R^2"),
  list(code = "userFlagStatus", name = "User Flag Status"),
  list(code = "userFlagStatus", name = "Algorithm Flag Status")
)
sortOptions.substrateInhibition <- list(
  list(code = "compoundCode", name = "Compound Code"),
  list(code = "Vmax", name = "Vmax"),
  list(code = "Km", name = "Km"),
  list(code = "Ki", name = "Ki"),
  list(code = "SST", name = "SST"),
  list(code = "SSE", name = "SSE"),
  list(code = "rsquare", name = "R^2"),
  list(code = "userFlagStatus", name = "User Flag Status"),
  list(code = "userFlagStatus", name = "Algorithm Flag Status")
)
get_curve_attributes.LL4 <- function(fitData, saved = TRUE) {
  if(saved) {
    return(list(
      EC50 = fitData$ec50[[1]],
      Operator = na_to_null(fitData$ec50OperatorKind),
      SST =  fitData$sst[[1]],
      SSE =  fitData$sse[[1]],
      rsquare = fitData$rSquared[[1]],
      compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
      algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
      userFlagStatus = fitData$userFlagStatus[[1]],
      renderingHint = fitData$renderingHint[[1]]
    ))
  } else {
    return(list(EC50 = fitData[1]$reportedParameters[[1]]$ec50$value,
                Operator = fitData[1]$reportedParameters[[1]]$ec50$operator,
                SST = fitData[1]$goodnessOfFit.model[[1]]$SST,
                SSE =  fitData[1]$goodnessOfFit.model[[1]]$SSE,
                rsquare =  fitData[1]$goodnessOfFit.model[[1]]$rSquared,
                compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
                algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
                userFlagStatus = fitData$userFlagStatus[[1]],
                renderingHint = fitData$renderingHint[[1]]
    ))
  }
}
get_curve_attributes.LL4IC50 <- function(fitData, saved = TRUE) {
  if(saved) {
    return(list(
      IC50 = fitData$ic50[[1]],
      Operator = na_to_null(fitData$ic50OperatorKind),
      SST =  fitData$sst[[1]],
      SSE =  fitData$sse[[1]],
      rsquare = fitData$rSquared[[1]],
      compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
      algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
      userFlagStatus = fitData$userFlagStatus[[1]],
      renderingHint = fitData$renderingHint[[1]]
    ))
  } else {
    return(list(IC50 = fitData[1]$reportedParameters[[1]]$ic50$value,
                Operator = fitData[1]$reportedParameters[[1]]$ic50$operator,
                SST = fitData[1]$goodnessOfFit.model[[1]]$SST,
                SSE =  fitData[1]$goodnessOfFit.model[[1]]$SSE,
                rsquare =  fitData[1]$goodnessOfFit.model[[1]]$rSquared,
                compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
                algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
                userFlagStatus = fitData$userFlagStatus[[1]],
                renderingHint = fitData$renderingHint[[1]]
    ))
  }
}
get_curve_attributes.LL4IC50DMax <- function(fitData, saved = TRUE) {
  if(saved) {
    return(list(
      IC50 = fitData$ic50[[1]],
      DMax = fitData$dmax[[1]],
      Operator = na_to_null(fitData$ic50OperatorKind),
      SST =  fitData$sst[[1]],
      SSE =  fitData$sse[[1]],
      rsquare = fitData$rSquared[[1]],
      compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
      algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
      userFlagStatus = fitData$userFlagStatus[[1]],
      renderingHint = fitData$renderingHint[[1]]
    ))
  } else {
    return(list(IC50 = fitData[1]$reportedParameters[[1]]$ic50$value,
                DMax = fitData[1]$reportedParameters[[1]]$dmax$value,
                Operator = fitData[1]$reportedParameters[[1]]$ic50$operator,
                SST = fitData[1]$goodnessOfFit.model[[1]]$SST,
                SSE =  fitData[1]$goodnessOfFit.model[[1]]$SSE,
                rsquare =  fitData[1]$goodnessOfFit.model[[1]]$rSquared,
                compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
                algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
                userFlagStatus = fitData$userFlagStatus[[1]],
                renderingHint = fitData$renderingHint[[1]]
    ))
  }
}
get_curve_attributes.ki <- function(fitData, saved = TRUE) {
  if(saved) {
    return(list(
      Ki = fitData$ki[[1]],
      Operator = na_to_null(fitData$kiOperatorKind),
      SST =  fitData$sst[[1]],
      SSE =  fitData$sse[[1]],
      rsquare = fitData$rSquared[[1]],
      compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
      algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
      userFlagStatus = fitData$userFlagStatus[[1]],
      renderingHint = fitData$renderingHint[[1]]
    ))
  } else {
    return(list(Ki = fitData[1]$reportedParameters[[1]]$ki$value,
                Operator = fitData[1]$reportedParameters[[1]]$ki$operator,
                SST = fitData[1]$goodnessOfFit.model[[1]]$SST,
                SSE =  fitData[1]$goodnessOfFit.model[[1]]$SSE,
                rsquare =  fitData[1]$goodnessOfFit.model[[1]]$rSquared,
                ligandConc = fitData[1]$ligandConc[[1]],
                kd = fitData[1]$kd[[1]],                                            
                compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
                algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
                userFlagStatus = fitData$userFlagStatus[[1]],
                renderingHint = fitData$renderingHint[[1]]
    ))
  }
}
get_curve_attributes.MM2 <- function(fitData, saved = TRUE) {
  if(saved) {
    return(list(
      Km = fitData$km[[1]],
      Operator = na_to_null(fitData$kmOperatorKind),
      SST =  fitData$sst[[1]],
      SSE =  fitData$sse[[1]],
      rsquare = fitData$rSquared[[1]],
      compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
      algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
      userFlagStatus = fitData$userFlagStatus[[1]],
      renderingHint = fitData$renderingHint[[1]]
    ))
  } else {
    return(list(Km = fitData[1]$reportedParameters[[1]]$km$value,
                Operator = fitData[1]$reportedParameters[[1]]$km$operator,
                SST = fitData[1]$goodnessOfFit.model[[1]]$SST,
                SSE =  fitData[1]$goodnessOfFit.model[[1]]$SSE,
                rsquare =  fitData[1]$goodnessOfFit.model[[1]]$rSquared,
                ligandConc = fitData[1]$ligandConc[[1]],
                compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
                algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
                userFlagStatus = fitData$userFlagStatus[[1]],
                renderingHint = fitData$renderingHint[[1]]
    ))
  }
}
get_curve_attributes.substrateInhibition <- function(fitData, saved = TRUE) {
  if(saved) {
    return(list(
      VMax = fitData$vmax[[1]],
      Km = fitData$km[[1]],
      Ki = fitData$ki[[1]],
      Operator = na_to_null(fitData$kmOperatorKind),
      SST =  fitData$sst[[1]],
      SSE =  fitData$sse[[1]],
      rsquare = fitData$rSquared[[1]],
      compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
      algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
      userFlagStatus = fitData$userFlagStatus[[1]],
      renderingHint = fitData$renderingHint[[1]]
    ))
  } else {
    return(list(VMax = fitData[1]$reportedParameters[[1]]$vmax$value,
                Km = fitData[1]$reportedParameters[[1]]$km$value,
                Ki = fitData[1]$reportedParameters[[1]]$ki$value,
                Operator = fitData[1]$reportedParameters[[1]]$km$operator,
                SST = fitData[1]$goodnessOfFit.model[[1]]$SST,
                SSE =  fitData[1]$goodnessOfFit.model[[1]]$SSE,
                rsquare =  fitData[1]$goodnessOfFit.model[[1]]$rSquared,
                compoundCode = paste0(fitData$batchCode[[1]],na_to_null(fitData$curveName)),
                algorithmFlagStatus = fitData$algorithmFlagStatus[[1]],
                userFlagStatus = fitData$userFlagStatus[[1]],
                renderingHint = fitData$renderingHint[[1]]
    ))
  }
}
get_model_fit_from_type_code <- function(modelFitTypeCode = NULL) {
  modelFitClasses <- get_model_fit_classes()
  if(is.null(modelFitTypeCode)) {
    source <- modelFitClasses[1]$RSource
  } else {
    source <- modelFitClasses[code==modelFitTypeCode]$RSource
  }
  if(!is.na(source)) {
    source(file.path(applicationSettings$appHome,source), local = TRUE) 
  } else {
    modelFit <- NA
  }
  return(modelFit)
}
get_model_fit_classes <- function() {
  modelFitClasses <- rbindlist(fromJSON(applicationSettings$client.curvefit.modelfitparameter.classes), fill = TRUE)
  return(modelFitClasses)
}
get_saved_fitted_parameters.LL4 <- function(fitData, overRideMaxMin = NA) {
  list(min = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMin, overRideMaxMin),  max = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMax, overRideMaxMin), ec50 = fitData[1]$fittedEC50, slope = fitData[1]$fittedSlope)
}
get_saved_fitted_parameters.LL4IC50 <- function(fitData, overRideMaxMin = NA) {
  list(min = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMin, overRideMaxMin),  max = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMax, overRideMaxMin), ic50 = fitData[1]$fittedIC50, slope = fitData[1]$fittedSlope)
}
get_saved_fitted_parameters.ki <- function(fitData, overRideMaxMin = NA) {
  list(min = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMin, overRideMaxMin), max = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMax, overRideMaxMin), ki = fitData[1]$fittedKi, ligandConc = fitData[1]$fittedLigandConc, kd = fitData[1]$fittedKd)
}
get_saved_fitted_parameters.MM2 <- function(fitData, overRideMaxMin = NA) {
  list(vmax = ifelse(is.na(overRideMaxMin), fitData[1]$fittedVMax, overRideMaxMin), km = fitData[1]$fittedKm)
}
get_saved_fitted_parameters.substrateInhibition <- function(fitData, overRideMaxMin = NA) {
  list(vmax = ifelse(is.na(overRideMaxMin), fitData[1]$fittedVMax, overRideMaxMin), km = fitData[1]$fittedKm, ki = fitData[1]$fittedKi)
}
get_plot_data_curve.LL4 <- function(fitData, overRideMaxMin = NA) {
  list(min = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMin, overRideMaxMin),  max = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMax, overRideMaxMin), ec50 = fitData[1]$fittedEC50, slope = fitData[1]$fittedSlope)
}
get_plot_data_curve.LL4IC50 <- function(fitData, overRideMaxMin = NA) {
  list(min = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMin, overRideMaxMin),  max = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMax, overRideMaxMin), ic50 = fitData[1]$fittedIC50, slope = fitData[1]$fittedSlope)
}
get_plot_data_curve.ki <- function(fitData, overRideMaxMin = NA) {
  list(min = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMin, overRideMaxMin), max = ifelse(is.na(overRideMaxMin), fitData[1]$fittedMax, overRideMaxMin), ki = fitData[1]$fittedKi, ligandConc = fitData[1]$fittedLigandConc, kd = fitData[1]$fittedKd)
}
get_plot_data_curve.MM2 <- function(fitData, overRideMaxMin = NA) {
  list(vmax = ifelse(is.na(overRideMaxMin), fitData[1]$fittedVMax, overRideMaxMin), km = fitData[1]$fittedKm)
}
get_plot_data_curve.substrateInhibition <- function(fitData, overRideMaxMin = NA) {
  list(vmax = ifelse(is.na(overRideMaxMin), fitData[1]$fittedVMax, overRideMaxMin), km = fitData[1]$fittedKm, km = fitData[1]$fittedKi)
}

