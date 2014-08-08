
LL4 <- 'min + (max - min)/(1 + exp(slope * (log(x/ec50))))'
OneSiteKi <- 'min + (max-min)/(1+10^(x-log10((10^Log10Ki)*(1+ligandConc/kd))))'
MM2 <- '(max*x)/(kd + x)'

#' Fit dose response data
#'
#' Converts a fitData object to a fitted fitData object (adds model, reported parameters...etc.)
#' 
#' @param fitSettingsJSON a fit settings json object (see examples)
#' @param fitData a fitData object to fit/refit
#' @return A fitted fitData object
#' @export
#' @examples
#' #Fit all the curveids in the database:
#' 
#' #get fitData
#' data("example-ec50-fitData")
#' 
#' #fitSettingsJSON
#' file <- system.file("conf", "default-ec50-fitSettings.json", package = "racas")
#' fitSettingsJSON <- readChar(file, file.info(file)$size)
#' fitSettings <- fromJSON(fitSettingsJSON)
#' #fit the data
#' fitData <- dose_response(fitSettings, fitData)
dose_response <- function(fitSettings, fitData) {
  #Need to copy fitData so we are working with our own copy (data.table does objects by reference)
  fitData <- copy(fitData)
  
  #Extract the fit variables from json
  myFixedParameters <- fitSettings$fixedParameters
  myParameterRules <- fitSettings$parameterRules
  myInactiveRule <- fitSettings$inactiveRule
  myInverseAgonistMode <- ifelse(is.null(fitSettings$inverseAgonistMode), TRUE, fitSettings$inverseAgonistMode)
  myBiphasicRule <- fitSettings$biphasicRule
  myUserFlag <- ifelse(is.null(fitSettings$user_flag), "NA", fitSettings$user_flag)
  fitData[ , fixedParameters := list(list(myFixedParameters))]
  fitData[ , parameterRules := list(list(myParameterRules))]
  fitData[ , inactiveRule := list(list(myInactiveRule))]
  fitData[ , inverseAgonistMode := myInverseAgonistMode]
  fitData[ , biphasicRule := list(list(myBiphasicRule))]
  
  #Update all of the flags to those that are in the fitSettings json
  updateFlags <- as.data.table(fitSettings$updateFlags)
  if(nrow(updateFlags) > 0 ) {
    #updateFlags[flag=="NA", flag := as.character(NA)]
    setkey(updateFlags,"response_sv_id" )
    #First fix issues with updateFlags (they may come in with "NA" instead of NA and logical istead of character)
    updateFlags[ flag_user == "NA", flag_user := as.character(NA)]
    updateFlags[ flag_on.load == "NA", flag_on.load := as.character(NA)]
    updateFlags[ , flag_algorithm := as.character(NA)]
    updateFlags[ , flag_user := as.character(flag_user)]
    updateFlags[ , flag_on.load := as.character(flag_on.load)]
    updateFlags[ , flag_algorithm := as.character(flag_algorithm)]
    
    update_point_flags <- function(pts, updateFlags) {  
      #save the column to return
      returnCols <- names(pts)
      setkey(pts, "response_sv_id")
      pts <- merge(pts,updateFlags, all.x = TRUE, by = "response_sv_id", suffixes = c("",".y"))
      pts[ , flagchanged :=  !identical(flag_user,flag_user.y) | !identical(flag_on.load,flag_on.load.y) | !identical(flag_algorithm,flag_algorithm.y) | flagchanged, by = "response_sv_id" ]
      pts[flagchanged==TRUE , c('flag_user', 'flag_on.load', 'flag_algorithm' ):= list(flag_user.y, flag_on.load.y, flag_algorithm.y)]
      return(pts[, returnCols, with = FALSE])
    }
    fitData[, points := list(list(update_point_flags(points[[1]], updateFlags))) , by = curveid]
  }
  
  #Fit the data
  fitData <- dose_response_fit(fitData)
  
  #Biphasic Detection
  fitData <- biphasic_detection(fitData)
  
  #Applying Limits
  fitData <- apply_limits(fitData, iterations = 20)
  
  #Categorize the fit data
  fitData[ , category := categorize_fit_data(modelHint, results.parameterRules[[1]], fitSettings[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]]), by = curveid]
  
  #Extract the reported Parameters
  fitData[ , reportedParameters := list(list(get_reported_parameters(modelHint, results.parameterRules[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]], fixedParameters[[1]], fittedParameters[[1]], pointStats[[1]], goodnessOfFit.parameters[[1]], goodnessOfFit.model[[1]], flag_algorithm, flag_user))), by = curveid]
  
  return(fitData)
}

biphasic_detection <- function(fitData) {
  returnCols <- copy(names(fitData))
  test_for_biphasic <- function(biphasicRule, points, pointStats, model.synced, goodnessOfFit.model, inactive, fitConverged, potent, insufficientRange, biphasicParameterPreviousValue, testConc, continueBiphasicDetection, firstRun) {    
    if(!continueBiphasicDetection) {
      testConc <- as.numeric(NA)
      biphasicParameterPreviousValue <- as.numeric(NA)
      return(list(points = list(points), model.synced = model.synced, biphasicParameterPreviousValue = biphasicParameterPreviousValue, testConc = testConc, continueBiphasicDetection = continueBiphasicDetection))
    }
    if(firstRun) {
      #If detect biphasic is on,
      # there are doses above the empirical max dose with respnoses below empirical max respnose
      # the curve is not inactive, non-converged, insufficient range or potent 
      continueBiphasicDetection <- (length(biphasicRule) > 0) & pointStats$count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax > 0 & (!inactive & fitConverged & !insufficientRange & !potent)
      if(!continueBiphasicDetection) {
        testConc <- as.numeric(NA)
        biphasicParameterPreviousValue <- as.numeric(NA)
        return(list(points = list(points), model.synced = model.synced, biphasicParameterPreviousValue = biphasicParameterPreviousValue, testConc = testConc, continueBiphasicDetection = continueBiphasicDetection))
      } else {
        if(biphasicRule$type == "percentage") {
          biphasicParameterPreviousValue <- as.numeric(goodnessOfFit.model[biphasicRule$parameter][[1]])
          ifelse(!is.finite(max(sort(pointStats$doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax, decreasing = TRUE))), , TRUE)
          testConc <- max(sort(pointStats$doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax, decreasing = TRUE))
          points[dose == testConc, flag_temp := "possible biphasic"]
          model.synced <- FALSE
          continueBiphasicDetection <- TRUE
        } else {
          stop(paste(biphasicRule$type, "not a valid biphasic rule type"))
        }
      }
    } else {
      stillASigmoid <- (!inactive & fitConverged & !insufficientRange & !potent)
      if(stillASigmoid) {
        canCompareAgainstLastFit <- is.finite(goodnessOfFit.model[biphasicRule$parameter][[1]])
      } else {
        canCompareAgainstLastFit <- FALSE
      }
      if(canCompareAgainstLastFit) {
        better <- eval(parse(text = paste('(biphasicParameterPreviousValue - goodnessOfFit.model[biphasicRule$parameter][[1]])/biphasicParameterPreviousValue',biphasicRule$operator,'biphasicRule$value')))
      } else {
        better <- FALSE
      } 
      if(better) {
        biphasicParameterPreviousValue <- as.numeric(goodnessOfFit.model[biphasicRule$parameter][[1]])
        points[dose == testConc, flag_algorithm := "biphasic"]
        points[dose == testConc, flag_temp := as.character(NA)]
        if(pointStats$count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax > 0) {
          testConc <- max(sort(pointStats$doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax, decreasing = TRUE))
          points[dose == testConc, flag_temp := "possible biphasic"]
          model.synced <- FALSE
          continueBiphasicDetection <- TRUE
        } else {
          model.synced <- FALSE
          continueBiphasicDetection <- FALSE
        }
      } else {
        points[dose == testConc, flag_temp := as.character(NA)]
        model.synced <- FALSE
        testConc <- as.numeric(NA)
        biphasicParameterPreviousValue <- as.numeric(NA)
        continueBiphasicDetection <- FALSE
        return(list(points = list(points), model.synced = model.synced, biphasicParameterPreviousValue = biphasicParameterPreviousValue, testConc = testConc, continueBiphasicDetection = continueBiphasicDetection))
      }
    }
    return(list(points = list(points), model.synced = model.synced, biphasicParameterPreviousValue = biphasicParameterPreviousValue, testConc = testConc, continueBiphasicDetection = continueBiphasicDetection))
    
  }
  fitData[ , continueBiphasicDetection := TRUE]
  fitData[ , firstRun := TRUE]
  fitData[ , biphasicParameterPreviousValue := as.numeric(NA)]
  fitData[ , c("points","model.synced","biphasicParameterPreviousValue", "testConc", "continueBiphasicDetection") := test_for_biphasic(biphasicRule[[1]], points[[1]], pointStats[[1]], model.synced, goodnessOfFit.model[[1]], inactive, fitConverged, potent, insufficientRange, biphasicParameterPreviousValue = biphasicParameterPreviousValue, continueBiphasicDetection = continueBiphasicDetection, firstRun = firstRun), by = curveid]
  fitData[ , firstRun := FALSE]
  while(any(!fitData$model.synced)) {
    fitData <- dose_response_fit(fitData)
    fitData[ , c("points","model.synced","biphasicParameterPreviousValue", "testConc", "continueBiphasicDetection") := test_for_biphasic(biphasicRule[[1]], 
                                                                                                                                       points[[1]], 
                                                                                                                                       pointStats[[1]], 
                                                                                                                                       model.synced,
                                                                                                                                       goodnessOfFit.model[[1]], 
                                                                                                                                       inactive, 
                                                                                                                                       fitConverged, 
                                                                                                                                       potent, 
                                                                                                                                       insufficientRange,
                                                                                                                                       biphasicParameterPreviousValue,
                                                                                                                                       testConc,
                                                                                                                                       continueBiphasicDetection,
                                                                                                                                       firstRun), by = curveid]
  }
  return(fitData[, returnCols, with = FALSE])
}

apply_limits <- function(fitData, iterations = 20) {
  #While refit is true, keep refitting using fixed parameters
  #The reason we check refit and continue is because we are dealing with limits, 
  #if max is above limit so then we limit it, does min then go below limit? ok, then limit that....etc.
  #Check if limits have been exceeded and refit if not inactive, non-converged or insufficient range
  check_refit <- function(fitData) {
    refit <- fitData[ , switch(modelHint,
                               "LL.4" = {  maxExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "maxThreshold" %in% results.parameterRules[[1]]$limits)
                                           minExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "minThreshold" %in% results.parameterRules[[1]]$limits)
                                           slopeExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "slopeThreshold" %in% results.parameterRules[[1]]$limits)
                                           exceededAThreshold <- (maxExceeded | minExceeded | slopeExceeded)
                                           refit <-  exceededAThreshold & (!inactive | !fitConverged | !insufficientRange | !potent)
                                           refit
                               },
                               "MM.2" = {  maxExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "maxThreshold" %in% results.parameterRules[[1]]$limits)
                                           exceededAThreshold <- (maxExceeded)
                                           refit <-  exceededAThreshold & (!inactive | !fitConverged | !insufficientRange | !potent)
                                           refit
                               },
{
  warnUser(paste0("Refit rule not implemented for ", modelHint))
  FALSE
}),
by = curveid]$V1
return(refit)
  }
refit <- check_refit(fitData)
i <- 1
while(any(refit) & i < iterations) {
  fitData[refit, model.synced := FALSE]
  fitData[refit, fixedParameters := switch(modelHint,
                                           "LL.4" = {
                                             if(ifelse(is.null(fixedParameters[[1]]$max), FALSE, !is.na(fixedParameters[[1]]$max))) {
                                               fixedMax <- fixedParameters[[1]]$max
                                             } else {
                                               if("maxThreshold" %in% results.parameterRules[[1]]$limits) {
                                                 if(parameterRules[[1]]$limits$maxThreshold$type == "threshold") {
                                                   fixedMax <- parameterRules[[1]]$limits$maxThreshold$value
                                                 } else {
                                                   fixedMax <- pointStats[[1]][parameterRules[[1]]$limits$maxThreshold$reference][[1]]
                                                 }
                                               } else {
                                                 fixedMax <- NA
                                               }
                                             }
                                             if(ifelse(is.null(fixedParameters[[1]]$min), FALSE, !is.na(fixedParameters[[1]]$min))) {
                                               fixedMin <- fixedParameters[[1]]$min
                                             } else {
                                               #fixedMax <- ifelse("Min threshold exceeded" %in% results.parameterRules[[1]]$limits, parameterRules[[1]]$limits$maxThreshold$value, NA)
                                               if("minThreshold" %in% results.parameterRules[[1]]$limits) {
                                                 if(parameterRules[[1]]$limits$minThreshold$type == "threshold") {
                                                   fixedMin <- parameterRules[[1]]$limits$minThreshold$value
                                                 } else {
                                                   fixedMin <- pointStats[[1]][parameterRules[[1]]$limits$minThreshold$reference][[1]]
                                                 }
                                               } else {
                                                 fixedMin <- NA
                                               }
                                             }
                                             if(ifelse(is.null(fixedParameters[[1]]$slope), FALSE, !is.na(fixedParameters[[1]]$slope))) {
                                               fixedSlope <- fixedParameters[[1]]$slope
                                             } else {
                                               fixedSlope <- ifelse("slopeThreshold" %in% results.parameterRules[[1]]$limits, parameterRules[[1]]$limits$slopeThreshold$value, NA)
                                             }
                                             list(list(myfixedParameters = list(max = fixedMax,min = fixedMin,slope = fixedSlope, ec50 = NA)))
                                           },
                                           "MM.2" = {
                                             if(ifelse(is.null(fixedParameters[[1]]$max), FALSE, !is.na(fixedParameters[[1]]$max))) {
                                               fixedMax <- fixedParameters[[1]]$max
                                             } else {
                                               if("maxThreshold" %in% results.parameterRules[[1]]$limits) {
                                                 if(parameterRules[[1]]$limits$maxThreshold$type == "threshold") {
                                                   fixedMax <- parameterRules[[1]]$limits$maxThreshold$value
                                                 } else {
                                                   fixedMax <- pointStats[[1]][parameterRules[[1]]$limits$maxThreshold$reference][[1]]
                                                 }
                                               } else {
                                                 fixedMax <- NA
                                               }
                                             }
                                             list(list(myfixedParameters = list(max = fixedMax, kd = NA)))
                                           }),
          by = curveid]
  fitData <- dose_response_fit(fitData)
  refit <- check_refit(fitData)
  i <- i + 1
}
return(fitData)
}

#' Convert a simple dose response request to an advanced dose response request
#'
#' Reads the default fit settings for the given model hint and updates it based on the simple request
#' 
#' @param simpleSettings a list object of simple fit settings
#' @param modelHint a character string to identify the dose response model to fit
#' @return an advanced fit settings list object
#' @export
#' @examples
#' file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas")
#' simpleSettingsJSON <- readChar(file, file.info(file)$size)
#' simpleSettings <- fromJSON(simpleSettingsJSON)
#' simple_to_advanced_fit_settings(simpleSettings)
simple_to_advanced_fit_settings <- function(simpleSettings, updateFlags = NULL, modelHint = "LL.4") {
  #simpleRequest <- simpleBulkDoseResponseFitRequest
  defaultSettings <- get_default_fit_settings(modelHint)
  updateFitSettings.LL4 <- function(fitSettings, simpleFitSettings) {
    update.fitSetting.parameter.LL4 <- function(fitSettings, name, simpleSettingsParameter) {   
      if(simpleSettingsParameter$limitType=="none") {
        fitSettings$parameterRules$limits[[paste0(name,"Threshold")]] <- NULL
        fitSettings$fixedParameters[[name]] <- NULL
      }
      if(simpleSettingsParameter$limitType=="pin") {
        fitSettings$parameterRules$limits[[paste0(name,"Threshold")]] <- NULL
        fitSettings$fixedParameters[[name]] <- ifelse(name=="slope",-simpleSettingsParameter$value,simpleSettingsParameter$value)
      }
      if(simpleSettingsParameter$limitType=="limit") {
        fitSettings$parameterRules$limits[[paste0(name,"Threshold")]] <- list(parameter = name,
                                                                              type = "threshold",
                                                                              operator = switch(name,
                                                                                                max = ">",
                                                                                                min = "<",
                                                                                                ec50 = ">",
                                                                                                slope = "<",
                                                                                                stop(paste0("Unknown parameter:",name))),
                                                                              value = ifelse(name=="slope",-simpleSettingsParameter$value,simpleSettingsParameter$value),
                                                                              displayName = paste0(name," threshold exceeded")
        )
        fitSettings$fixedParameters[[name]] <- NULL
      }
      return(fitSettings)
    }
    fitSettings <- update.fitSetting.parameter.LL4(fitSettings, name = "min", simpleSettingsParameter = simpleSettings$min)
    fitSettings <- update.fitSetting.parameter.LL4(fitSettings, name = "max", simpleSettingsParameter = simpleSettings$max)
    fitSettings <- update.fitSetting.parameter.LL4(fitSettings, name = "slope", simpleSettingsParameter = simpleSettings$slope) 
    return(fitSettings)
  }
  updateFitSettings.MM2 <- function(fitSettings, simpleFitSettings) {
    update.fitSetting.parameter.MM2 <- function(fitSettings, name, simpleSettingsParameter) {   
      if(simpleSettingsParameter$limitType=="none") {
        fitSettings$parameterRules$limits[[paste0(name,"Threshold")]] <- NULL
        fitSettings$fixedParameters[[name]] <- NULL
      }
      if(simpleSettingsParameter$limitType=="pin") {
        fitSettings$parameterRules$limits[[paste0(name,"Threshold")]] <- NULL
        fitSettings$fixedParameters[[name]] <- simpleSettingsParameter$value
      }
      if(simpleSettingsParameter$limitType=="limit") {
        fitSettings$parameterRules$limits[[paste0(name,"Threshold")]] <- list(parameter = name,
                                                                              type = "threshold",
                                                                              operator = switch(name,
                                                                                                max = ">",
                                                                                                kd = ">",
                                                                                                stop(paste0("Unknown parameter:",name))),
                                                                              value = ifelse(simpleSettingsParameter$value),
                                                                              displayName = paste0(name," threshold exceeded")
        )
        fitSettings$fixedParameters[[name]] <- NULL
      }
      return(fitSettings)
    }
    fitSettings <- update.fitSetting.parameter.MM2(fitSettings, name = "max", simpleSettingsParameter = simpleSettings$max)
    fitSettings <- update.fitSetting.parameter.MM2(fitSettings, name = "kd", simpleSettingsParameter = simpleSettings$kd) 
    return(fitSettings)
  }
  modifiedSettings <- defaultSettings
  modifiedSettings$inactiveRule$value <- simpleSettings$inactiveThreshold
  modifiedSettings$inverseAgonistMode <- simpleSettings$inverseAgonistMode
  if(!is.null(simpleSettings$biphasicRule)) {
    modifiedSettings$biphasicRule <- simpleSettings$biphasicRule
  }
  if(!is.null(simpleSettings$flag_user)) {
    modifiedSettings$flag_user <- simpleSettings$flag_user
  }
  
  modifiedSettings <- switch(modelHint,
                             "LL.4" = updateFitSettings.LL4(modifiedSettings,simpleSettings),
                             "MM.2" = updateFitSettings.MM2(modifiedSettings,simpleSettings),
                             warnUser(paste0("Simple to Advanced fit settings not implemented for modelHint: ",modelHint))
  )
  if(!is.null(updateFlags)) {
    modifiedSettings$updateFlags <- as.data.table(updateFlags)
  }
  return(modifiedSettings)
}

get_default_fit_settings <- function(modelHint) {
  file <- system.file("conf", switch(modelHint,
                                     "LL.4" = "default-ec50-fitSettings.json",
                                     "MM.2" = "default-kd-fitSettings.json",
                                     stop("modelHint \'", modelHint,"\' does not have a default fit settings json object")), package = "racas")
  defaultRequest <- readChar(file, file.info(file)$size)
  defaultRequest <- fromJSON(defaultRequest)
  return(defaultRequest)
}

#' Fit dose response data
#'
#' Converts a character vector of curveids, character sessionsID or data.table fitData object to a fitted fitData object, 
#' saves the fitData object to either a new session or back to the session provided and then returns 
#' a json representation of the fitted fitData object with the savedSessionID
#' 
#' @param fitSettings a fit settings list object (see examples)
#' @param curveids a character list of curveids
#' @param sessionID a path to a curve fit session
#' @param fitData a fidData object to refit
#' @param simpleFitSettings a list of simplefitsettings to attach to the fitData object
#' @return A list object with a fitted fitData object and sessionID
#' @export
#' @examples
#' #Fit all the curveids in the database:
#' 
#' #get curveids
#' curveids <- as.character(query("select curveid from api_curve_params")[[1]])
#' 
#' #fitSettings
#' file <- system.file("conf", "default-ec50-fitSettings.json", package = "racas")
#' fitSettingsJSON <- readChar(file, file.info(file)$size)
#' fitSettings <- fromJSON(fitSettingsJSON)
#' #fit the data
#' system.time(response <- get_fit_data_experiment_codefitSettings, curveids = curveids))
#' 
dose_response_session <- function(fitSettings, curveids = NA, sessionID = NA, fitData = NA, simpleFitSettings = NULL, ...) {
  if(all(is.na(c(curveids, sessionID, fitData)))) stop("Must provide curveids or sessionID or fitData, all are NA")
  if(class(curveids) == "character") {
    fitData <- get_fit_data_curve(curveids)
  }
  if(!is.na(sessionID)) {
    fitSettings_new <- fitSettings
    sessionID_new <- sessionID
    simpleFitSettings_new <- simpleFitSettings
    loadSession(sessionID)
    fitSettings <- fitSettings_new
    sessionID <- sessionID_new
    simpleFitSettings <- simpleFitSettings_new
    rm(fitSettings_new,sessionID_new, simpleFitSettings_new)
    if(exists("fitData")) {
      fitData[, model.synced := FALSE]      
    } else {
      stop("fitData object does not exist in session, are you sure this was a fit session?")
    }
  }
  if(any(class(fitData) == "data.table")) {
    if(!is.null(simpleFitSettings)) {
      fitData[ , simpleFitSettings := NULL]
      fitData[ , simpleFitSettings := toJSON(simpleFitSettings)]
    }
    fitData <- dose_response(fitSettings, fitData) 
  } else {
    stop("fitData not a data.table")
  }
  if(is.na(sessionID)) {
    sessionID <- saveSession()
  } else {
    sessionID <- saveSession(sessionID)
  }
  return(list(fitData = fitData, sessionID = sessionID))
}

predict_drm_points <- function(pts, drcObj) {
  if(nrow(pts)==0 || is.null(drcObj)) {
    return(NULL)
  }
  x <- unique(pts$dose)
  if(grepl("LOG",toupper(pts$doseunits[1]))) {
    valuesToPredict <- data.frame(x = exp( seq(log(min(x)), log(max(x)), length.out=12*length(x)) ))
  } else {
    valuesToPredict <- data.frame(x = seq(min(x), max(x), length.out=12*length(x)))
  }
  curveData <- data.frame(dose = valuesToPredict$x, response = predict(drcObj, newdata = valuesToPredict))
  return(curveData)
}


get_plot_window <- function(pts, logDose = TRUE, logResponse = FALSE, ymin = NA, ymax = NA, xmin = NA, xmax = NA){
  if(nrow(pts)==0) {
    return(NULL)
  } else {
    maxDose <- max(pts$dose)
    minDose <- min(pts$dose)
    maxResponse <- max(pts$response)
    minResponse <- min(pts$response)
    responseRange <- abs(maxResponse-minResponse)
    doseRange <- abs(maxDose-minDose)
    if(is.na(ymin)) {
      if(logResponse) {
        ymin <- floor(log10(maxResponse))
      } else {
        ymin <- (minResponse - 0.025*responseRange)
      }
      if(ymin > 0) {
        ymin  <- 0 - responseRange*0.05
      }
    }
    if(is.na(ymax)) {
      if(logResponse) {
        ymax <- ceiling(log10(maxResponse))
      } else {
        ymax <- (maxResponse + 0.025*responseRange)
      }
    }
    if(is.na(xmax)) {
      if(logDose) {
        xmax <- ceiling(log10(maxDose))
      } else {
        xmax <- maxDose + abs(0.01 * doseRange)
      }  
    }
    if(is.na(xmin)) {
      if(logDose) {
        xmin <- floor(log10(minDose))
      } else {
        xmin <- minDose - abs(0.01 * doseRange)
      }
    }
    return(c(xmin,ymax,xmax,ymin))
  }
}
capture_output <- function(obj, ...) {
  val <- capture.output({
    result <- withVisible(obj)
    if (result$visible)
      print(result$value)
  })
  nonEmpties <- which(val!="")
  val <- val[nonEmpties[1]:nonEmpties[length(nonEmpties)]]
  val <- gsub(" ", "&nbsp;", val)
  return(paste(val, ...))
}

data.table_to_html_table <- function(dataTable, ...) {
  htmlTableString <- ""
  if(is.null(dataTable)) {return(htmlTableString)}
  if(nrow(dataTable) == 0) {
    return(htmlTableString)
  }
  htmlTableString <- print(xtable(dataTable), 
                           type = "html", 
                           include.rownames = FALSE, 
                           comment = FALSE, 
                           timestamp = FALSE, 
                           rotate.rownames = TRUE, 
                           html.table.attributes = "",
                           print.results = FALSE, ...)
  return(htmlTableString)
}

get_reported_parameters <- function(modelHint, results, inactive, fitConverged, insufficientRange, potent, fixedParameters, fittedParameters, pointStats, goodnessOfFit.parameters, goodnessOfFit.model, flag_user, flag_algorithm) {
  if(!is.na(flag_algorithm) | !is.na(flag_user)) {
    return(list())
  }
  switch(modelHint,
         "LL.4" = {
           if(potent) {
             max <- list(value = pointStats$response.empiricalMax, operator = NULL, stdErr = NULL)
             min <- list(value = pointStats$response.empiricalMin, operator = NULL, stdErr = NULL)
             ec50 <- list(value = pointStats$dose.min, operator = "<", stdErr = NULL)
             reportedValues <- list(min = min, max = max, ec50 = ec50)
             return(reportedValues)
           }
           if(inactive | insufficientRange) {
             max <- list(value = pointStats$response.empiricalMax, operator = NULL, stdErr = NULL)
             min <- list(value = pointStats$response.empiricalMin, operator = NULL, stdErr = NULL)
             ec50 <- list(value = pointStats$dose.max, operator = ">", stdErr = NULL)
             reportedValues <- list(min = min, max = max, ec50 = ec50)
             return(reportedValues)
           }
           
           if(!fitConverged) {
             return(NULL)
           }
           if("maxUncertaintyRule" %in% results$goodnessOfFits) {
             max <- list(value = pointStats$response.empiricalMax, operator = NULL, stdErr = NULL)
           } else {
             if(is_null_or_na(fixedParameters$max)) {
               max <- list(value = fittedParameters$max, operator = NULL, stdErr = if(is.finite(goodnessOfFit.parameters$max.stdErr)) {goodnessOfFit.parameters$max.stdErr} else {NULL})
             } else {
               max <- list(value = fixedParameters$max, operator = NULL, stdErr = NULL)
             }
           }
           if("minUncertaintyRule" %in% results$goodnessOfFits) {
             min <- list(value = pointStats$response.empiricalMin, operator = NULL, stdErr = NULL)
           } else {
             if(is_null_or_na(fixedParameters$min)) {
               min <- list(value = fittedParameters$min, operator = NULL, stdErr = if(is.finite(goodnessOfFit.parameters$min.stdErr)) {goodnessOfFit.parameters$min.stdErr} else {NULL})
             } else {
               min <- list(value = fixedParameters$min, operator = NULL, stdErr = NULL)
             }
           }
           if(is_null_or_na(fixedParameters$slope)) {
             slope <- list(value = -fittedParameters$slope, operator = NULL, stdErr = if(is.finite(goodnessOfFit.parameters$slope.stdErr)) {goodnessOfFit.parameters$slope.stdErr} else {NULL})
           } else {
             slope <- list(value = -fixedParameters$slope, operator = NULL, stdErr = NULL)
           }
           if(("ec50ThresholdHigh" %in% results$limits | "maxUncertaintyRule" %in% results$goodnessOfFits) | ("ec50ThresholdLow" %in% results$limits)) {
             if(("ec50ThresholdHigh" %in% results$limits | "maxUncertaintyRule" %in% results$goodnessOfFits)) {
               ec50 <- list(value = pointStats$dose.max, operator = ">", stdErr = NULL)
             } else {
               ec50 <- list(value = pointStats$dose.min, operator = "<", stdErr = NULL)
             }
           } else {
             ec50 <- list(value = fittedParameters$ec50, operator = NULL, stdErr = if(is.finite(goodnessOfFit.parameters$ec50.stdErr)) {goodnessOfFit.parameters$ec50.stdErr} else {NULL})
           }
           reportedValues <- list(min = min, max = max, slope = slope, ec50 = ec50)
           return(reportedValues)
         },
         "MM.2" = {
           if(inactive | insufficientRange) {
             max <- list(value = pointStats$response.empiricalMax, operator = NULL, stdErr = NULL)
             kd <- list(value = pointStats$dose.max, operator = ">", stdErr = NULL)
             reportedValues <- list(max = max, kd = kd)
             return(reportedValues)
           }
           if(!fitConverged) {
             return(list())
           }
           if("maxUncertaintyRule" %in% results$goodnessOfFits) {
             max <- list(value = pointStats$response.empiricalMax, operator = NULL, stdErr = NULL)
           } else {
             if(is_null_or_na(fixedParameters$max)) {
               max <- list(value = fittedParameters$max, operator = NULL, stdErr = if(is.finite(goodnessOfFit.parameters$max.stdErr)) {goodnessOfFit.parameters$max.stdErr} else {NULL})
             } else {
               max <- list(value = fixedParameters$max, operator = NULL, stdErr = NULL)
             }
           }
           if("kdThreshold" %in% results$limits) {
             kd <- list(value = pointStats$dose.max, operator = NULL, stdErr = NULL)
           } else {
             kd <- list(value = fittedParameters$kd, operator = NULL, stdErr = if(is.finite(goodnessOfFit.parameters$kd.stdErr)) {goodnessOfFit.parameters$kd.stdErr} else {NULL})
           }
           reportedValues <- list(max = max, kd = kd)
           return(reportedValues)
         },
{warnUser(paste0("Not implemented for ", modelHint))
 return(list())
}
  )
}

get_fit_data_curve <- function(curveID, include = "fullobject") {
  myMessenger <- messenger()
  myMessenger$logger$debug("Doing query to convert curve id to analyis group id")
  analyisGroupIDOfCurveID <- query(paste0("select ags.analysis_group_id 
               from analysis_group_state ags 
               join analysis_group_value agv 
               on agv.analysis_state_id=ags.id where agv.string_value = ", sqliz(curveID)))[[1]]
  fitData <- get_fit_data_analysisgroupid(analyisGroupIDOfCurveID, include)
  return(fitData)
}

get_fit_data_analysisgroupid <- function (analysisGroupdID, include) {
  myMessenger <- messenger()
  myMessenger$logger$debug("Calling analysisgroup id service")
  serviceURL <- paste0(racas::applicationSettings$client.service.persistence.fullpath, "analysisgroups/", analysisGroupdID, "?with=", include)
  myMessenger$logger$debug(serviceURL)  
  analyisGroupJSON <- getURL(paste0(racas::applicationSettings$client.service.persistence.fullpath, "analysisgroups/", analysisGroupdID, "?with=", include))
  myMessenger$logger$debug("Parsing analyis group json")
  analysisGroup <- jsonlite::fromJSON(analyisGroupJSON[[1]])
  fitData <- list_to_data.table(analysisGroup)
  return(fitData)
}

get_fit_data_experiment_code <- function(experimentCode, include = "fullobject", ...) {
  myMessenger <- messenger()
  myMessenger$logger$debug("calling experiment code name service")
  serviceURL <- paste0(racas::applicationSettings$client.service.persistence.fullpath, "experiments/codename/", experimentCode, "?with=", include)
  myMessenger$logger$debug(serviceURL)
  experimentJSON <- getURL(serviceURL)
  myMessenger$logger$debug("parsing experiment json")
  experiment <- jsonlite::fromJSON(experimentJSON[[1]])
  fitData <- as.data.table(experiment$analysisGroups[[1]][!experiment$analysisGroups[[1]]$ignored,])
}

get_fit_data <- function(entityID, type = c("experimentCode","analysisGroupID", "curveID"), include = "fullobject", ...) {
  type <- match.arg(type)
  myMessenger <- messenger()
  fitData <- switch(type,
                    "experimentCode" = get_fit_data_experiment_code(entityID, include),
                    "curveID" = get_fit_data_curve(entityID, include),
                    "analysisGroupID" = get_fit_data_analysisgroupid(entityID, include)
  )
  
  myMessenger$logger$debug("extracting curve parameters")
  fitData[ , lsStates := list(list(
    rbindlist(lsStates)[ignored == FALSE]
  )), by = id]
  fitData <- fitData[fitData[ , 'data' %in% lsStates[[1]]$lsType, by = id][[2]]]
  fitData[ , parameters := list(list(
    rbindlist(lsStates[[1]][ , lsValues:= list(list(lsValues[[1]][ , order(names(lsValues[[1]]))])), by = id]$lsValues)[ , lsStates := list(list(lsStates[[1]]))]
  )), by = id]
  fitData[ , c('curveid','flag_algorithm','flag_user') := {
    list(parameters[[1]][grepl('.*curve id', lsKind)]$stringValue,
         ifelse(length(parameters[[1]][lsKind == "flag" & stringValue == "algorithm" & ignored == FALSE]$comments) == 0, as.character(NA), as.character(parameters[[1]][lsKind == "flag" & stringValue == "algorithm"]$comments)),
         ifelse(length(parameters[[1]][lsKind == "flag" & stringValue == "user" & ignored == FALSE]$comments) == 0, as.character(NA), as.character(parameters[[1]][lsKind == "flag" & stringValue == "user"]$comments)))
    
  }, by = id]
  myParameterRules <- list(goodnessOfFits = list(), limits = list())
  myInactiveRule <- list()
  myInverseAgonistMode <- TRUE
  myBiphasicRule <- list()
  myFixedParameters <- list()
  
  fitData[ , c("parameterRules", "inactiveRule", "fixedParameters", "inverseAgonistMode", "biphasicRule") := list(list(myParameterRules),
                                                                                                                  list(myInactiveRule),
                                                                                                                  list(myFixedParameters),
                                                                                                                  myInverseAgonistMode,
                                                                                                                  list(myBiphasicRule))]
  fitData[ , modelHint := unlist(lapply(rbindlist(parameters)[lsKind == "Rendering Hint"]$stringValue, 
                                        function(x) {
                                          ans <- switch(x,
                                                        "4 parameter D-R" = "LL.4",
                                                        "2 parameter Michaelis Menten" = "MM.2")
                                          return(ans)
                                        })),
          by = curveid]
  if(is.null(fitData$modelHint)) {
    myMessenger$addUserError(paste0("No Rendering Hint found for ", entityID))
    myMessenger$logger$error(paste0("Attempted to fit an expt code with no rendering hint stored in analysis group parameters"))
  }
  fitData[ , model.synced := FALSE]
  
  if (include == "fullobject") {
    myMessenger$logger$debug("extracting curve points")
    fitData[ ,  points:= list(list({
      treatmentGroups <- rbindlist(treatmentGroups)[ignored == FALSE]
      subjects <- treatmentGroups[ , {
        tg_id <- id
        rbindlist(subjects)[ , c("subj_id","tg_id") := list(subj_id = id, tg_id = tg_id)]
      }, by = id]
      subjectStates <- subjects[ , rbindlist(lsStates)[ , c("subj_id", "tg_id"):= list(subj_id = subj_id, tg_id = unique(tg_id))], by = subj_id]
      subjectStates[ lsKind=="results", c("response_ss_id","response_ss_version") := list(response_ss_id = id, response_ss_version = version)]
      points <- subjectStates[ , {
        lsValues <- Reduce(function(x,y) rbind(x,y,fill = TRUE), lapply(lsValues, as.data.table))
        list(list(lsValues[ , c("subj_id", "response_ss_id", "response_ss_version", "tg_id") :=list(subj_id = subj_id,
                                                                                                    response_ss_id=response_ss_id[!is.na(response_ss_id)], 
                                                                                                    response_ss_version=response_ss_version[!is.na(response_ss_version)],
                                                                                                    tg_id = unique(tg_id)
        )
        ]))
      }, by = subj_id]
      Reduce(function(x,y) rbind(x,y,fill = TRUE), points$V1)
    }
    )), by = id]
    myMessenger$logger$debug("pivoting the curve points")
    fitData[ , points := list(list({
      dr <- data.table::dcast.data.table(points[[1]][lsKind %in% c("Dose", "Response")], subj_id+response_ss_id+response_ss_version+tg_id ~ lsKind, value.var = "numericValue")
      dr <- merge(dr, points[[1]][ lsKind=="Response", c("subj_id", "id"), with =  FALSE], by = "subj_id")
      drUnits <- dcast.data.table(points[[1]][lsKind %in% c("Dose", "Response")], subj_id ~ lsKind, value.var = "unitKind")
      setnames(drUnits, "Dose", "doseUnits")
      setnames(drUnits, "Response", "responseUnits")
      dr <- dr[drUnits]
      setnames(dr, "id", "response_sv_id")
      if(nrow(points[[1]][lsKind=="flag" & ignored == FALSE]) > 0 & !is.null(points[[1]]$comments)) {
        fl <- dcast.data.table(points[[1]][lsKind=="flag" & ignored == FALSE], subj_id ~ lsKind+stringValue, value.var = "comments")
        setnames(fl, make.names(names(fl)))
        missingFlagColumns <- setdiff(c("flag_on.load","flag_user", "flag_algorithm", "flag_temp"), names(fl))
        if(length(missingFlagColumns) > 0) {
          fl[ , missingFlagColumns := as.character(NA), with = FALSE]
        }
        fl <- merge(fl,points[[1]][ lsKind=="flag" & ignored == FALSE, c("subj_id", "id"), with =  FALSE], by = "subj_id")
        setnames(fl, "id", "flag_sv_id")
      } else {
        fl <- data.table(subj_id = as.integer(),flag_sv_id = as.integer(), flag_on.load = as.character(), flag_user = as.character(), flag_algorithm = as.character(), flag_temp = as.character())
        setkey(fl, subj_id)
      }
      #bc <-  dcast.data.table(points[[1]][lsKind=="batch code"], subj_id ~ lsKind, value.var = "codeValue")
      setkey(fl, subj_id)
      #setkey(bc, subj_id)
      setkey(dr, subj_id)
      #pts <- fl[bc][dr]
      pts <- fl[dr]
      pts[ , flagchanged := FALSE]
      setnames(pts, names(pts), tolower(names(pts)))
      setcolorder(pts, order(names(pts)))
      pts
    })), by = id]
    myMessenger$logger$debug("filling out the rest of the fit data object")
  }
  myMessenger$logger$debug(paste0("returning from getting experiment curve data with ", nrow(fitData), " curves"))
  return(fitData)
}

dose_response_fit <- function(fitData, refit = FALSE, ...) {
  fitDataNames <- names(fitData)
  ###Fit
  fitData[model.synced == FALSE, model := list(model = list(switch(modelHint,
                                                                   "LL.4" = get_drc_model(points[[1]], drcFunction = LL.4, paramNames = c("slope", "min", "max", "ec50"), fixed = fixedParameters[[1]]),
                                                                   "MM.3" = get_drc_model(points[[1]], drcFunction = MM.3, paramNames = c("slope","max", "kd"), fixed = fixedParameters[[1]]),
                                                                   "MM.2" = get_drc_model(points[[1]], drcFunction = MM.2, paramNames = c("max", "kd"), fixed = fixedParameters[[1]])
  ))
  ), by = curveid]
  
  ###Collect Stats
  fitData[ model.synced == FALSE, fitConverged := ifelse(unlist(lapply(model, is.null)), FALSE, model[[1]]$fit$convergence), by = curveid]
  fitData[ model.synced == FALSE, c("pointStats","fittedParameters", "goodnessOfFit.model", "goodnessOfFit.parameters") := {
    list(pointStats = list(get_point_stats(points[[1]])),
         fittedParameters = list(get_parameters_drc_object(model[[1]])),
         goodnessOfFit.model = list(get_fit_stats_drc_object(model[[1]], points[[1]])),
         goodnessOfFit.parameters = list(get_goodness_of_fit_parameters_drc_object(model[[1]]))
    )}
    , by = curveid]
  #Fail Heuristics  
  fitData[ model.synced == FALSE, results.parameterRules := list(list(list(goodnessOfFits = apply_parameter_rules_goodness_of_fits(goodnessOfFit.parameters[[1]], parameterRules[[1]]$goodnessOfFits),
                                                                           limits = apply_parameter_rules_limits(fittedParameters[[1]],pointStats[[1]], parameterRules[[1]]$limits)
  ))), by = curveid]
  fitData[ model.synced == FALSE, c("inactive", "insufficientRange", "potent") := apply_inactive_rules(pointStats[[1]],points[[1]], inactiveRule[[1]], inverseAgonistMode), by = curveid]
  fitData[ model.synced == FALSE, "flag_algorithm" := ifelse(fitConverged | inactive | insufficientRange | potent, as.character(NA), "not converged"), by = curveid]
  returnCols <- unique(c(fitDataNames, "model", "fitConverged", "pointStats", "fittedParameters", "goodnessOfFit.model", "goodnessOfFit.parameters", "inactive", "insufficientRange", "potent"))
  
  fitData[ model.synced == FALSE, model.synced := TRUE]
  return(fitData[, returnCols, with = FALSE])
}

categorize_fit_data <- function(modelHint, results.parameterRules, fitSettings, inactive, converged, insufficientRange, potent) {
  #   -weak tested potency
  #   strong tested potency
  #   -inactive
  #   Low Quality Fit - failed PValue Test
  #   hockey stick
  #   sigmoid 
  #   -lack of fit - fit did not converge
  category <- "sigmoid"
  resultList <- unlist(results.parameterRules)
  category <- switch(modelHint,
                     "LL.4" = {
                       if("maxUncertaintyRule" %in% resultList | "ec50ThresholdHigh" %in% resultList) {
                         category <- "weak tested potency"
                       }
                       if(!converged) {
                         category <- "lack of fit - fit did not converge"
                       }
                       if(insufficientRange) {
                         category <- "insufficient range"
                       }
                       if("ec50ThresholdLow" %in% resultList | potent) {
                         category <- "strong tested potency"
                       }
                       if(inactive) {
                         category <- "inactive"
                       }
                       category
                     },
                     "MM.2" = {
                       if("maxUncertaintyRule" %in% resultList | "kdThresholdHigh" %in% resultList) {
                         category <- "weak tested potency"
                       }
                       if(!converged) {
                         category <- "lack of fit - fit did not converge"
                       }
                       if(insufficientRange) {
                         category <- "insufficient range"
                       }
                       if("kdThresholdLow" %in% resultList | potent) {
                         category <- "strong tested potency"
                       }
                       if(inactive) {
                         category <- "inactive"
                       }
                       category                       
                     },{
                       warnUser(paste0("Limit rules not implemented for ", modelHint))
                       FALSE
                     })
  return(category)
}
apply_parameter_rules_limits <- function(fittedParameters, pointStats, rules) {
  if(is.null(fittedParameters)) return(NULL)
  if(is.null(pointStats)) return(NULL)
  answer <- lapply(seq_along(rules), function(x) {
    rule <- rules[[x]]
    ruleName <- names(rules[x])
    reference <- rule$reference
    parameter <- rule$parameter
    type <- rule$type
    value <- rule$value
    operator <- rule$operator
    if(is.null(fittedParameters[parameter][[1]])) return(NULL)
    fail <- switch(type,
                   "logReference" = eval(parse( text = paste('fittedParameters[parameter]',operator,'10^(log10(pointStats[reference][[1]]) + value)'))),
                   "threshold" = eval(parse( text = paste('fittedParameters[parameter]',operator,'value'))),
                   "%" = eval(parse( text = paste('fittedParameters[parameter]',operator,'(pointStats[reference][[1]] + pointStats[reference][[1]]*(value/100))'))),
    )
    if(fail) {
      return(ruleName)
    }
  }  )
  return(unlist(answer))
}

apply_parameter_rules_goodness_of_fits <- function(goodnessOfFit.parameters, rules) {
  if(is.null(goodnessOfFit.parameters)) return(NULL)
  answer <- lapply(seq_along(rules), function(x) {
    rule <- rules[[x]]
    ruleName <- names(rules[x])
    parameter <- rule$parameter
    heuristic <- rule$type
    value <- rule$value
    operator <- rule$operator
    column <- paste0(parameter,".",heuristic)
    if(is.null(goodnessOfFit.parameters[column][[1]])) return(NULL)
    fitValue <- goodnessOfFit.parameters[column][[1]]
    if(!is.finite(fitValue)) {
      fail <- TRUE
    } else {
      fail <- eval(parse( text = paste('goodnessOfFit.parameters[paste0(parameter,".",heuristic)]',operator,'value')))
    }
    if(fail) {
      return(ruleName)
    }
  })
  return(unlist(answer))
}

apply_inactive_rules <- function(pointStats, points, rule, inverseAgonistMode) {
  if(is.null(pointStats)) return(NULL)
  if(is.null(points)) return(NULL)
  if(length(rule) > 0) {
    threshold <- rule$value
    mockControls <- ifelse(is.null(rule$mockControls), FALSE, rule$mockControls)
    if(mockControls) {
      response.empiricalMin <- pointStats$response.empiricalMin
      response.empiricalMax <- pointStats$response.empiricalMax
      threshold <- threshold * abs(min(response.empiricalMin) - max(response.empiricalMax))
    }
    means <- points[ is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp), list("dose" = dose, "mean.response" = mean(response)), by = dose]
    numDoses <- nrow(means)
    if(!inverseAgonistMode) {
      dosesAboveThreshold <- length(which(means$mean.response >= threshold))
    } else {
      dosesAboveThreshold <- length(which(abs(means$mean.response) >= threshold))
    }
    inactive <- dosesAboveThreshold < rule$activeDoses
    potent <- dosesAboveThreshold == numDoses
    
    insufficientRange <- abs(pointStats$response.empiricalMax - pointStats$response.empiricalMin) < threshold
  } else {
    inactive <- FALSE
    insufficientRange <- FALSE
    potent <- FALSE
  }
  return(list(inactive = inactive, insufficientRange = insufficientRange, potent = potent))  
}

get_drc_model <- function(dataSet, drcFunction = LL.4, subs = NA, paramNames = eval(formals(drcFunction)$names), fixed, robust = "mean") {
  fixedParams <- data.frame(matrix(NA,1,length(paramNames)))
  names(fixedParams) <- paramNames
  fixed[unlist(lapply(fixed, is.null))] <- NULL
  matches <- match(paramNames, names(fixed), nomatch= FALSE)
  fixedParams[which(matches != 0)] <- fixed[matches]
  fixed <- unlist(fixedParams)
  fct <- drcFunction(fixed=fixed, names=paramNames)
  drcObj <- NULL
  tryCatch({
    drcObj <- drm(formula = response ~ dose, data = dataSet, subset = is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp), robust=robust, fct = fct, control = drmc(errorm=TRUE))
  }, error = function(ex) {
    #Turned of printing of error message because shiny was printing to the browser because of a bug
    #print(ex$message)
  })
  return(drcObj)
}

get_point_stats <- function(pts) {
  pts <- copy(pts)
  pts[ is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp), meanByDose := mean(response), by = dose ]
  response.empiricalMax <- pts[ is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp), max(meanByDose)]
  response.empiricalMin <- pts[is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp), min(meanByDose)]
  dose.min <- min(pts[is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp), ]$dose)
  dose.max <- max(pts[is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp), ]$dose)
  dose.empiricalMaxResponse <- pts[(is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp)) &  meanByDose == response.empiricalMax, min(dose)]
  dose.empiricalMinResponse <- pts[(is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp)) &  meanByDose == response.empiricalMin, min(dose)]
  doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax <- pts[(is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp)) & dose > dose.empiricalMaxResponse & meanByDose < response.empiricalMax, unique(dose)]
  count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax <- length(doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax)
  doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin <- pts[(is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp)) & dose < dose.empiricalMinResponse & meanByDose > response.empiricalMin, unique(dose)]
  count.doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin <- length(doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin)
  return(list(response.empiricalMax = response.empiricalMax, 
              response.empiricalMin = response.empiricalMin, 
              dose.min = dose.min, 
              dose.max = dose.max, 
              doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax = doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax, 
              doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin = doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin,
              count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax = count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax,
              count.doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin = count.doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin))
}

ki_names <- c("Top", "Bottom", "logKi")
one_site_ki <- function(kd, ligandConc, fixed = c(NA, NA, NA), names = c("c", "d", "e")) {
  ki.fct3 <- param[,2] + (param[,1]-param[,2])/(1+10^(x-log10(10^param[,3]*(1+ligandConc/kd))))
  return(c(ki.fct, ki.ssft, ki.names))
}
on_site_ki_ssf <- function(data) {
  Top <- max(data[,2])
  Bottom <- min(data[,2])
  logKi <- -8.0
  return(c(Top, Bottom, logKi))
}

ki_ssf_free <- function(data) {
  Top <- max(data[,2])
  KiuM <- 0.1
  Bottom <- min(data[,2])
  return(c(Bottom, KiuM, Top))
}

get_parameters_drc_object <- function(drcObj = drcObject) {
  if(is.null(drcObj)) {
    return(NULL)
  }
  #Get calculated values (only non-fixed parameters)
  fittedParameters <- as.list(coefficients(drcObj))
  names(fittedParameters) <- gsub("\\:\\(Intercept\\)","", names(fittedParameters))
  
  fixedParameters <- as.list(drcObj$fct$fixed)
  fixedParameters[is.na(fixedParameters) | names(fixedParameters) == ""] <- NULL
  
  parameters <- c(fittedParameters, fixedParameters)
  return(parameters)
}
get_fit_stats_drc_object <- function(drcObject, points) {
  if(is.null(drcObject)) return(NULL)
  SSE <- suppressWarnings(sum((residuals(drcObject))^2))
  SST <- sum((points$response-mean(points[is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp),]$response))^2)
  rSquared <- 1-(SSE/SST)
  return(list(SSE = SSE, SST = SST, rSquared = rSquared))
}
get_goodness_of_fit_parameters_drc_object <- function(drcObj) {
  if(is.null(drcObj)) {
    return(NULL)
  }
  myMatrix <- suppressWarnings(coefficients(summary(drcObj)))
  rownames(myMatrix) <- gsub("\\:\\(Intercept\\)","", rownames(myMatrix))
  stdErrors <- as.list(myMatrix[,"Std. Error"])
  tValues <- as.list(myMatrix[,"t-value"])
  pValues <- as.list(myMatrix[,"p-value"])
  names(stdErrors) <- paste0(rownames(myMatrix),".stdErr")
  names(tValues) <- paste0(rownames(myMatrix),".tValue")
  names(pValues) <- paste0(rownames(myMatrix),".pValue")
  return(c(stdErrors,tValues, pValues))
}

#' fitData object to json response
#'
#' Converts a fitData object to a json response to return to the GUI
#' 
#' @param a fitData object
#' @param ... addition arguments to be passed to \code{\link{toJSON}}
#' @return A json object of the fitData and any other objects coerced to json by ... \code{\link{toJSON}}
#' @export
#' @examples
#' #Load and example fitData object
#' data("example-ec50-fitData-fitted")
#' #FitData object plus the "cars" data to a json string
#' fit_data_to_acas_experiment_response(fitData, cars)
fit_data_to_acas_experiment_response <- function(fitData, transactionId = -1, status, hasWarning, errorMessages = as.character(), ...) {

  hasError <- length(errorMessages) > 0
  if(!hasError) {
    rmd <- system.file("rmd", "fitDataToResponse_acas.rmd", package="racas")
    htmlSummary <- knit2html_bug_fix(input = rmd, 
                                    options = c("base64_images", "mathjax"),
                                    template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                    stylesheet = system.file("rmd", "racas_container.css", package="racas"))
  } else {
    rmd <- system.file("rmd", "fitDataToResponse_error.rmd", package="racas")
    htmlSummary <- knit2html_bug_fix(input = rmd, 
                                    options = c("base64_images", "mathjax"),
                                    template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                    stylesheet = system.file("rmd", "racas_container.css", package="racas"))
  }
  response <- list(
    transactionId = transactionId,
    results = list(htmlSummary = htmlSummary,
                   status = status),
    hasError = hasError,
    hasWarning = hasWarning,
    errorMessages = errorMessages
  )  
  return(response)
}

knit2html_bug_fix <- function (input, output = NULL, text = NULL, template = template, stylesheet = stylesheet, options = c()) {
  originalWD <- getwd()
  t <- tempfile(pattern = "folder")
  dir.create(t)
  setwd(t)
  tfile <- tempfile(tmpdir=t)
  out <- knitr::knit(input, output = tfile, text = NULL, envir = parent.frame(), encoding = getOption("encoding"), quiet = TRUE)
  output <- markdown::markdownToHTML(file = out, template = template, stylesheet = stylesheet, options = options, fragment.only = FALSE)
  setwd(originalWD)
  unlink(t, recursive = TRUE)
  return(output)
}

load_dose_response_test_data <- function(type = c("small.ll4","large.ll4", "explicit.ll4", "small.mm2")) {
  type <- match.arg(type)
  doseResponseSELFile <- switch(type,
                                "small.ll4" = system.file("docs", "Example-Dose-Response-SEL.xlsx", package="racas"),
                                "explicit.ll4" = system.file("docs", "example-dose-response-ec50-explicit.xlsx", package="racas"),
                                "small.mm2" = system.file("docs", "Example-Dose-Response-SEL-KD.xlsx", package="racas"),
                                "large.ll4" = system.file("docs", "Example-Dose-Response-SEL-Large.xlsx", package="racas")
  )
  originalWD <- getwd()
  on.exit(  setwd(originalWD))
  acasHome <- racas::applicationSettings$appHome
  selCode <- file.path(acasHome,"public","src","modules","GenericDataParser","src","server","generic_data_parser.R")
  setwd(acasHome)
  file.copy(doseResponseSELFile,"privateUploads/", overwrite = TRUE)
  source(selCode, local = TRUE)
  request <- list(fileToParse=basename(doseResponseSELFile), dryRunMode = "false", user="bbolt")
  response <- parseGenericData(request)
  if(response$hasError) {
    cat(response$errorMessages[[1]]$message)
  }
  if(grepl("explicit",type)) {
    wb <- XLConnect::loadWorkbook(doseResponseSELFile)
    genericDataFileDataFrame <- XLConnect::readWorksheet(wb, sheet=1, header = FALSE, dateTimeFormat="A_date_was_in_Excel_Date_format")
    metaData <- getSection(genericDataFileDataFrame, lookFor = "Experiment Meta Data", transpose = TRUE)
    experimentCode <- metaData$"Experiment Code Name"
  } else {
    experimentCode <- response$results$experimentCode
  }
  file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas")
  simpleSettingsJSON <- readChar(file, file.info(file)$size)
  simpleSettings <- fromJSON(simpleSettingsJSON)
  api_doseResponse.experiment(simpleSettings, recordedBy="bbolt", experimentCode=experimentCode)
  return(experimentCode)
}

flatten_list_to_data.table <- function(l) {
  dt <- Reduce(function(x,y) rbind(x,y,fill = TRUE), lapply(1:length(l), function(x) {
    dt <- as.data.table(l[[x]])
    dt[ , name := names(l)[[x]]]
    classes <- lapply(dt, class) 
    removeThese <- names(classes)[classes=="NULL"]
    if(length(removeThese) > 0) {
      dt[ , removeThese := NULL, with = FALSE]
    }
    return(dt)
  }
  )
  )
  return(dt)
}

list_to_data.table <- function(l) {
  dt <- data.table(1)
  invisible(lapply(1:length(l) , function(x) {
    if(is.null(l[[x]])) return()
    if(length(l[[x]]) == 1) {
      dt[ , names(l)[[x]] := l[[x]]]
    } else {
      if(class(l[[x]]) == "data.frame") {
        dt[ , names(l)[[x]] := list(list(as.data.table(l[[x]])))]
      } else {
        dt[ , names(l)[[x]] := list(list(l[[x]]))]
      }
    }
  }
  ))
  dt[ , V1 := NULL]
  return(dt)
}

create_analysis_group_values_from_fitData <- function(reportedParameters, fixedParameters, fittedParameters, goodnessOfFit.model, category, flag_algorithm, flag_user, tested_lot, recordedBy, lsTransaction, doseUnits, responseUnits, analysisGroupCode, renderingHint, reportedValuesClob, fitSummaryClob, parameterStdErrorsClob, curveErrorsClob, simpleFitSettings) {
  fitParameters <- c(fixedParameters,fittedParameters)
  names(fitParameters) <- paste0("fitted_",names(fitParameters))
  reportedParameters[unlist(lapply(reportedParameters, function(x) is_null_or_na(x$value)))] <- NULL
  publicAnalysisGroupValues <- c(reportedParameters, list(tested_lot = list(value = tested_lot, operator = NULL), curveid = list(value = paste0(analysisGroupCode,"_", lsTransaction), operator = NULL, stdErr = NULL)))
  privateAnalysisGroupValues <- c(fitParameters, goodnessOfFit.model, list('Rendering Hint' = renderingHint), c(list(category = category), list(algorithmFlag = "algorithm")[!is.na(flag_algorithm)],  list(userFlag = "user")[!is.na(flag_user)], list(reportedValuesClob = reportedValuesClob), list(fitSummaryClob = fitSummaryClob), list(parameterStdErrorsClob = parameterStdErrorsClob), list(curveErrorsClob = curveErrorsClob),  list(simpleFitSettings = simpleFitSettings)))
  privateAnalysisGroupValues[unlist(lapply(privateAnalysisGroupValues, is_null_or_na))] <- NULL
  privateAnalysisGroupValues <- lapply(privateAnalysisGroupValues, function(x) list(value = x, operator = NULL, stdErr = NULL))
  
  x <- c(publicAnalysisGroupValues,privateAnalysisGroupValues)   
  public <- c(rep(TRUE, length(publicAnalysisGroupValues)), rep(FALSE, length(privateAnalysisGroupValues)))
  lsTypes <- unlist(lapply(x, function(x) ifelse(class(x$value)=="numeric", "numericValue", "stringValue")))
  lsTypes[names(lsTypes) == "tested_lot"] <- "codeValue"
  lsTypes[names(lsTypes)  %in% c("algorithmFlag", "userFlag")] <- "comments"
  lsTypes[names(lsTypes) %in% c("reportedValuesClob", "fitSummaryClob", "parameterStdErrorsClob", "curveErrorsClob", "simpleFitSettings")] <- "clobValue"
  valueUnits <- rep(list(NULL),length(lsTypes))
  valueUnits[names(lsTypes) %in% c("min", "max","fitted_min", "fitted_max")] <- responseUnits
  valueUnits[names(lsTypes) %in% c("ec50", "fitted_ec50")] <- doseUnits
  kindMap <- list(min = "Min" ,
                  max = "Max" ,
                  slope = "Slope" ,
                  ec50 = "EC50" ,
                  tested_lot = "batch code" ,
                  curveid = "curve id" ,
                  fitted_slope = "Fitted Slope",
                  fitted_min = "Fitted Min" ,
                  fitted_max = "Fitted Max" ,
                  fitted_ec50 = "Fitted EC50" ,
                  SSE = "SSE" ,
                  SST = "SST" ,
                  rSquared = "rSquared" ,
                  category = "category" ,
                  algorithmFlag = "flag",
                  userFlag = "flag",
                  simpleFitSettings = "fitSettings")
  matches <- match(names(kindMap), names(x))
  names(x)[matches[!is.na(matches)]] <- kindMap[which(!is.na(matches))]
  lsKinds <- names(x)
  stringValues <- ifelse(lsTypes=="stringValue" | lsTypes == "comments", lapply(x, function(x) x$value), list(NULL))
  codeValues <- ifelse(lsTypes=="codeValue", lapply(x, function(x) x$value), list(NULL))
  numericValues <- ifelse(lsTypes=="numericValue", lapply(x, function(x) x$value), list(NULL))
  clobValues <- ifelse(lsTypes=="clobValue", lapply(x, function(x) x$value), list(NULL))
  comments <- ifelse(lsTypes=="comments", lapply(x, function(x) x$value), list(NULL))
  operatorValues <- lapply(x, function(x) x$operator)
  uncertanties <- lapply(x, function(x) x$stdErr)
  uncertantyTypes <- uncertanties
  uncertantyTypes[!unlist(lapply(uncertanties,is.null))] <- "Standard Error"
  agValues <- lapply(1:length(x), 
                     function(x) {
                       createStateValue(lsType = lsTypes[[x]],
                                        lsKind = lsKinds[[x]],
                                        valueUnit = valueUnits[[x]],
                                        stringValue = stringValues[[x]],
                                        comments = comments[[x]],
                                        numericValue = numericValues[[x]],
                                        valueOperator = operatorValues[[x]],
                                        uncertaintyType = uncertantyTypes[[x]],
                                        uncertainty = uncertanties[[x]],
                                        codeValue = codeValues[[x]],
                                        clobValue = clobValues[[x]],
                                        publicData = public[[x]],
                                        lsTransaction=lsTransaction,
                                        recordedBy = as.character(recordedBy))
                     }
  )
  return(list(analysisGroupCode = analysisGroupCode, analysisGroupValues = agValues))
}

save_dose_response_data <- function(fitData, recorded_by) {
  myMessenger <- messenger()
  myMessenger$logger <- logger(logName = "com.acas.api.doseresponse.save")
  
  myMessenger$logger$debug("getting transaction id")
  transactionID <- createLsTransaction()$id
  myMessenger$logger$debug("getting analysis group values from fit data")
  fitData[ , analysisGroupValues := list(list(create_analysis_group_values_from_fitData(reportedParameters[[1]],
                                                                     fixedParameters[[1]],
                                                                     fittedParameters[[1]],
                                                                     goodnessOfFit.model[[1]],
                                                                     category[[1]],
                                                                     flag_algorithm[[1]],
                                                                     flag_user[[1]],
                                                                     tested_lot = parameters[[1]][lsKind=="batch code",]$codeValue,
                                                                     recorded_by[[1]],
                                                                     transactionID,
                                                                     doseUnits = as.character(points[[1]][1]$doseunits), 
                                                                     responseUnits = as.character(points[[1]][1]$responseunits), 
                                                                     analysisGroupCode = codeName[[1]], 
                                                                     as.character(parameters[[1]][lsKind=="Rendering Hint"]$stringValue),
                                                                     reportedValuesClob = reportedValuesClob[[1]],
                                                                     fitSummaryClob = fitSummaryClob[[1]],
                                                                     parameterStdErrorsClob = parameterStdErrorsClob[[1]],
                                                                     curveErrorsClob = curveErrorsClob[[1]],
                                                                     simpleFitSettings = simpleFitSettings[[1]]
  )
  
  ))
  , by = curveid]
  myMessenger$logger$debug("saving dose response parameter data")
  savedStates <- save_fit_data(fitData, recorded_by, transactionID)
  myMessenger$logger$debug("saving dose response point data")
  savedPoints <- fitData[,  list(update_point_flags(points, recorded_by, transactionID))][[1]]  
  myMessenger$logger$debug("returning response")
  return(list(lsStates = savedStates, lsTransaction = transactionID))
  
}
update_experiment_status <- function(experimentCodeName, status) { 
  experiment <- getExperimentByCodeName(experimentCodeName)
  experimentMetaDataState <- which(unlist(lapply(experiment$lsStates, function(x) x$lsKind == "experiment metadata" & x$ignored == FALSE)))
  analysisStatusValue <- which(unlist(lapply(experiment$lsStates[[experimentMetaDataState]]$lsValues,function(x) x$lsKind == "analysis status" & x$ignored == FALSE)))
  experiment$lsStates[[experimentMetaDataState]]$lsValues[[analysisStatusValue]]$stringValue <- status
  value <- experiment$lsStates[[experimentMetaDataState]]$lsValues[[analysisStatusValue]]
  value <- updateAcasEntity(value, "experimentvalues")
}

save_fit_data <- function(fitData, recordedBy, lsTransaction) {
  ignoredAnalysisGroupStates <- lapply(fitData$lsStates, function(x) {
    x <- as.list(x)
    x$lsValues <- NULL
    x$ignored <- TRUE
    updateAcasEntity(x, "analysisgroupstates")
  })
  fitData[ , newStates := list(list(createAnalysisGroupState(
    analysisGroup = list(id =  id[[1]], version = version),
    analysisGroupValues = analysisGroupValues[[1]]$analysisGroupValues,
    recordedBy = recordedBy,
    lsType = "data",
    lsKind = "Dose Response",
    lsTransaction = lsTransaction    
  ))), by = curveid]
  savedAnalysisGroupStates <- saveAcasEntities(fitData$newStates, "analysisgroupstates")
  return(fitData$newStates)
}

doseResponse_update_user_flag <- function(fitData, flagUser, recordedBy) {
  savedUserFlag <- fitData[1]$parameters[[1]][lsKind == "flag" & stringValue == "user" & ignored == FALSE]
  
  # If there are already saved user flags (meaning the curve was not approved by the user)
  if(nrow(savedUserFlag) == 0) {
    # If 
    if(is.na(flagUser)) {
      idsToIgnore <- fitData[1]$parameters[[1]][(publicData == TRUE | lsKind %in% c("reportedValuesClob") | lsKind == "flag" & stringValue == "user") & !lsKind %in% c("batch code","curve id") ]$id
      valuesToIgnore <- lapply(idsToIgnore, get_entity_by_id, "analysisgroupvalues")
      ignoredAnalysisGroupValues <- lapply(valuesToIgnore, function(x) {
        x$ignored <- TRUE
        updateAcasEntity(x, "analysisgroupvalues")
      })
      return(TRUE)
    } else {
      lsTransactionID <- createLsTransaction()$id
      flagUserStateValue <- createStateValue(lsState=get_entity_by_id(fitData[1]$parameters[[1]]$lsStates[[1]]$id,acasCategory="analysisgroupstates"),
                                             lsType = 'comments',
                                             lsKind = 'flag',
                                             stringValue = flagUser,
                                             comments = 'user',
                                             publicData = FALSE,
                                             lsTransaction=lsTransactionID,
                                             recordedBy = as.character(recordedBy))
      saved <- saveAcasEntities(list(flagUserStateValue), "analysisgroupvalues")
      
      otherStateValuesIds <- fitData[1]$parameters[[1]][ignored == FALSE & (publicData == TRUE | lsKind %in% c("reportedValuesClob")) & !lsKind %in% c("batch code","curve id") ]$id
      otherStateValues <- lapply(otherStateValuesIds, get_entity_by_id, "analysisgroupvalues")
      otherStateValues <- lapply(otherStateValues, function(x) {
        x$ignored <- TRUE
        updateAcasEntity(x, "analysisgroupvalues")
      })
      return(TRUE)
    }
  } else {
    if(is.na(flagUser)) {
      savedUserFlag <- get_entity_by_id(savedUserFlag$id, "analysisgroupvalues")
      savedUserFlag$ignored <- TRUE
      updateAcasEntity(savedUserFlag, "analysisgroupvalues")
      
      otherStateValuesIds <- fitData[1]$parameters[[1]][ignored == TRUE & (publicData == TRUE | lsKind %in% c("reportedValuesClob")) & !lsKind %in% c("batch code","curve id") ]$id
      otherStateValues <- lapply(otherStateValuesIds, get_entity_by_id, "analysisgroupvalues")
      otherStateValues <- lapply(otherStateValues, function(x) {
        x$ignored <- FALSE
        updateAcasEntity(x, "analysisgroupvalues")
      })
      return(TRUE)
      
    } else {
      otherStateValuesIds <- fitData[1]$parameters[[1]][ignored == FALSE & (publicData == TRUE | lsKind %in% c("reportedValuesClob")) & !lsKind %in% c("batch code","curve id") ]$id
      otherStateValues <- lapply(otherStateValuesIds, get_entity_by_id, "analysisgroupvalues")
      otherStateValues <- lapply(otherStateValues, function(x) {
        x$ignored <- TRUE
        updateAcasEntity(x, "analysisgroupvalues")
      })
      return(TRUE)
    }
  }
}

get_ls_state_from_entity <- function(entities, ...) {
  unlistEntities <- unlist(entities, recursive = FALSE)
  lsStatesList <- unlistEntities[names(unlistEntities) == "lsStates"]
  lsStates <- do.call("c", lsStatesList)
  match_list_criteria <- function(lsState, listCriteria) {
    #lsState <- lsStates[[1]]
    unlistedLSState <- unlist(lsState)
    match_criteria <- function(unlistedLSState, criteria) {
      any(names(unlistedLSState) == names(criteria) & unlistedLSState == criteria[[1]])
    }
    return(all(unlist(lapply(1:length(listCriteria), function(x) match_criteria(unlistedLSState,listCriteria[x])))))
  }
  matches <- unlist(lapply(lsStates, match_list_criteria, listCriteria = list(...)))
  lsStates[!matches] <- NULL
  return(lsStates)
}

update_point_flags <- function(points, recordedBy, lsTransaction) {  
  saveSession("~/Desktop/flags")
  pointData <- Reduce(function(x,y) rbind(x,y,fill = TRUE), points)
  pointData <- pointData[flagchanged == TRUE, ]
  addTheseFlags <- pointData[ !is.na(flag_user) | !is.na(flag_on.load) | !is.na(flag_algorithm)]
  ignoreTheseFlags <- pointData[!is.na(flag_sv_id), list(flag_sv_id, response_ss_id, response_ss_version, tg_id, flag_user, flag_on.load, flag_algorithm)]
  flagsToIgnore <- lapply(ignoreTheseFlags$flag_sv_id, get_entity_by_id, "subjectvalues")
  ignoredFlags <- lapply(flagsToIgnore, function(x) {
    x$ignored <- TRUE
    updateAcasEntity(x, "subjectvalues")
  })
  if(nrow(addTheseFlags) > 0) {
    addTheseFlags <- reshape2::melt(addTheseFlags,  measure.vars = c("flag_user", "flag_on.load", "flag_algorithm"), value.name = "comments", variable.name = "stringValue")[!is.na(comments),]
    addTheseFlags[ , stringValue := gsub("flag_","" ,stringValue)]
    addTheseFlags[ , stringValue := gsub("on.load","on load" ,stringValue)]
    newFlags <- addTheseFlags[, list(list(createStateValue(lsType = "stringValue", lsKind = "flag", stringValue = stringValue, comments = comments, lsTransaction=lsTransaction,recordedBy=recordedBy, lsState=list(id=response_ss_id[[1]], version=response_ss_version[[1]])))), by = response_sv_id]$V1
    saveAcasEntities(newFlags, "subjectvalues")
  }
  #   #Treatment group value updates
  #   update_tg_id <- unique(pointData$tg_id)
  #   updateTheseValues <- pointData[pointData$tg_id %in% update_tg_id]
  #   treatmentGroups <- lapply(update_tg_id, get_entity_by_id, "treatmentgroups")
  #   treatmentGroupDF <- ldply(treatmentGroups, flattenEntity, acasCategory= "treatmentGroup", includeFromState = c("id", "lsType", "lsKind", "version"))
  #   valuesToIgnoreDF <- treatmentGroupDF[treatmentGroupDF$lsKind == "Response", ]
  #   valuesToIgnore <- lapply(valuesToIgnoreDF$id, get_entity_by_id, "treatmentgroupvalues")
  #   ignoredValues <- lapply(valuesToIgnore, function(x) {
  #     x$ignored <- T
  #     updateAcasEntity(x, "treatmentgroupvalues")
  #   })
  #   updateTheseValues$tgs_id <- treatmentGroupDF$stateId[match(updateTheseValues$tg_id, treatmentGroupDF$treatmentGroupId)]
  #   updateTheseValues$tgs_version <- treatmentGroupDF$stateVersion[match(updateTheseValues$tg_id, treatmentGroupDF$treatmentGroupId)]
  #   #updateTheseValues$tgv_id <- treatmentGroupDF$id[match(updateTheseValues$tg_id, valuesToIgnoreDF$TreatmentGroupId)]
  #   
  #   newValues <- updateTheseValues[, list(list(createStateValue(lsType = "numericValue",
  #                                                               lsKind = "Response", 
  #                                                               numericValue = na_to_null(suppressWarnings(mean(response))), 
  #                                                               numberOfReplicates=length(response), 
  #                                                               uncertaintyType="standard deviation", 
  #                                                               uncertainty = na_to_null(sd(response)), 
  #                                                               lsTransaction=lsTransaction,
  #                                                               recordedBy=recordedBy, 
  #                                                               lsState=list(id=unique(tgs_id), 
  #                                                                            version=unique(tgs_version))))), 
  #                                  by = tg_id]$V1
  #   saveAcasEntities(newValues, "treatmentgroupvalues")
  
}

get_entity_by_id <- function(id, acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(paste0(racas::applicationSettings$client.service.persistence.fullpath, acasCategory, "/", id))
  if (grepl("^<", response)) {
    myLogger <- createLogger(logName = "com.acas.sel", logFileName = "racas.log")
    myLogger$error(response)
    stop(paste0("The loader was unable to find ", acasCategory, 
                " ", id, ". Check the logs at ", Sys.time()))
  }
  response <- fromJSON(response)
  return(response)
}
is_null_or_na <- function(value) {
  if(is.null(value)) return(TRUE)
  return(is.na(value))
}
na_to_null <- function(x) {
  if(is_null_or_na(x)) return(NULL)
  return(x)
}

add_clob_values_to_fit_data <- function(fitData) {
  fitData[ , c("reportedValuesClob", "fitSummaryClob", "parameterStdErrorsClob", "curveErrorsClob") := {
    if(model.synced) {
      if(fitConverged) {
        if(length(reportedParameters[[1]]) == 0) {
          reportedValuesClob <- list(NULL)
        } else {
          reportedValues <- flatten_list_to_data.table(reportedParameters[[1]])
          reportedValues <- reportedValues[ , value := {
            if(exists("operator")) {
              paste(ifelse(is.na(operator), "",operator), value)
            } else {
              value
            }}]
          reportedValuesClob <- data.table_to_html_table(reportedValues[ , c("name", "value"), with = FALSE], include.colnames = FALSE) 
        }
        fitSummaryClob <- capture_output(suppressWarnings(summary(model[[1]])), collapse = "<br>")
        goodnessOfFit.parameters <- flatten_list_to_data.table(goodnessOfFit.parameters[[1]])
        goodnessOfFit.parameters[ , c("name", "type") := {sp <- strsplit(name, "\\.")[[1]]
                                                          list(name = sp[[1]], type = sp[[2]])}, by = c("V1", "name")]
        goodnessOfFit.parameters <- dcast.data.table(goodnessOfFit.parameters, name ~ type, value.var = "V1")
        parameterStdErrors <- data.table_to_html_table(goodnessOfFit.parameters)
        parameterStdErrorsClob <- parameterStdErrors
        curveErrorsClob <- data.table_to_html_table(flatten_list_to_data.table(goodnessOfFit.model[[1]])[, c("name", "V1"), with = FALSE], include.colnames = FALSE)
        list(reportedValuesClob = list(reportedValuesClob), fitSummaryClob = list(fitSummaryClob), parameterStdErrorsClob = list(parameterStdErrorsClob), curveErrorsClob = list(curveErrorsClob))
      } else {
        list(reportedValuesClob = list(NULL), fitSummaryClob = list(NULL), parameterStdErrorsClob= list(NULL), curveErrorsClob = list(NULL))
      }
    } else {
      fitData[1]$parameters[[]]
    }
  }, by = curveid]
  return(fitData)
}