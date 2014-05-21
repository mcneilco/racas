
LL4 <- 'min + (max - min)/(1 + exp(slope * (log(x/ec50))))'
OneSiteKi <- 'min + (max-min)/(1+10^(x-log10((10^Log10Ki)*(1+ligandConc/kd))))'
MM2 <- '(max*x)/(kd + x)'

#' Fit dose response data
#'
#' Converts a fitData object to a fitted fitData object
#' 
#' 
#' @param fitSettingsJSON a fit settings json object (see examples)
#' @param curveids a character list of curveids
#' @param sessionID a path to a curve fit session
#' @param fitData a fidData object to refit
#' @return A json object with the sessionid among a number of html divs to display back to the user
#' @export
#' @examples
#' #Fit all the curveids in the database:
#' 
#' #get fitData
#' data("example-ec50-fitData")
#' 
#' #fitSettingsJSON
#' file <- system.file("docs", "default-ec50-fitSettings.json", package = "racas")
#' fitSettingsJSON <- readChar(file, file.info(file)$size)
#' 
#' #fit the data
#' fitData <- doseResponse.fitData(fitSettings, fitData)
doseResponse.fitData <- function(fitSettings, fitData) {
  #Need to copy fitData so we are working with our own copy (data.table does objects by reference)
  fitData <- copy(fitData)
  
  #Extract the fit variables from json
  myFixedParameters <- fitSettings$fixedParameters
  myParameterRules <- fitSettings$parameterRules
  myInactiveRule <- fitSettings$inactiveRule
  myInverseAgonistMode <- fitSettings$inverseAgonistMode
  myBiphasicRule <- fitSettings$biphasicRule
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
    updateFlags[ flag_algorithm == "NA", flag_algorithm := as.character(NA)]
    updateFlags[ flag_temp == "NA", flag_temp := as.character(NA)]
    updateFlags[ , flag_user := as.character(flag_user)]
    updateFlags[ , flag_on.load := as.character(flag_on.load)]
    updateFlags[ , flag_temp := as.character(flag_temp)]
    
    #pts <- fitData[1]$points[[1]]
    fitData[, points := list(list(update_point_flags(points[[1]]))) , by = curveid]
  }
  
  #Fit the data
  fitData <- doseResponseFit(fitData)
  
  #Biphasic Detection
  if(!is.null(fitSettings$biphasicRule)) {
    #fitData <- doseResponse.biphasicDetection(fitData)
  }
  
  #Applying Limits
  fitData <- doseResponse.applyLimits(fitData, iterations = 20)
  
  #Categorize the fit data
  fitData[ , category := categorizeFitData(results.parameterRules[[1]], fitSettings[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]]), by = curveid]
  #Extract the reported Parameters
  fitData[ , reportedParameters := list(list(getReportedParameters(modelHint, results.parameterRules[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]], fixedParameters[[1]], fittedParameters[[1]], pointStats[[1]], goodnessOfFit.parameters[[1]], goodnessOfFit.model[[1]]))), by = curveid]
  return(fitData)
}

update_point_flags <- function(pts) {  
  #save the column to return
  returnCols <- names(pts)
  setkey(pts, "response_sv_id")
  pts <- merge(pts,updateFlags, all.x = TRUE, by = "response_sv_id", suffixes = c("",".y"))
  pts[ , flagchanged :=  !identical(flag_user,flag_user.y) | !identical(flag_on.load,flag_on.load.y) | !identical(flag_algorithm,flag_algorithm.y) | !identical(flag_temp,flag_temp.y) | flagchanged, by = "response_sv_id" ]
  pts[flagchanged==TRUE , c('flag_user', 'flag_on.load', 'flag_algorithm', 'flag_temp' ):= list(flag_user.y, flag_on.load.y, flag_algorithm.y, flag_temp.y)]
  return(pts[, returnCols, with = FALSE])
}

doseResponse.biphasicDetection <- function(fitData) {
  s <- "~/Desktop/biphasic"
  saveSession(s)

  testForPossibleBiphasic <- function(biphasicRule, points, pointStats, model.synced, goodnessOfFit.model, inactive, fitConverged, potent, insufficientRange, biphasicParameterPreviousValue, testConc, continueBiphasicDetection) {
    #If detect biphasic is on,
    # there are doses above the empirical max dose with respnoses below empirical max respnose
    # the curve is not inactive, non-converged, insufficient range or potent
#     saveSession("~/Desktop/blah")
    
    if(continueBiphasicDetection & is.NULLorNA(biphasicParameterPreviousValue)) {
      continueBiphasicDetection <- biphasicRule$detectBiphasic & pointStats$count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax > 0 & (!inactive | !fitConverged | !insufficientRange | !potent)
    }
    
    if(continueBiphasicDetection) {
      if(biphasicRule$type == "percentage") {
        if(is.NULLorNA(biphasicParameterPreviousValue)) {
          biphasicParameterPreviousValue <- as.numeric(goodnessOfFit.model[biphasicRule$parameter][[1]])
          testConc <- pointStats$doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax[order(pointStats$doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax)]
          points[dose == testConc, flag_temp := "biphasic"]
          model.synced <- FALSE
          continueBiphasicDetection <- TRUE
        } else {
          cat("old-", biphasicParameterPreviousValue,"# new", goodnessOfFit.model[biphasicRule$parameter][[1]])
          better <- (biphasicParameterPreviousValue - goodnessOfFit.model[biphasicRule$parameter][[1]])/biphasicParameterPreviousValue > biphasicRule$value          
          if (better) {
            biphasicParameterPreviousValue <- as.numeric(goodnessOfFit.model[biphasicRule$parameter][[1]])
            points[dose == testConc, flag_temp := "biphasic"]
            testConc <- pointStats$doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax[order(pointStats$doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax)]
            model.synced <- FALSE
            continueBiphasicDetection <- TRUE
          } else {
            biphasicParameterPreviousValue <- as.numeric(goodnessOfFit.model[biphasicRule$parameter][[1]])
            testConc <- as.numeric(NA)
            points[dose == testConc, flag_temp := as.character(NA)]
            model.synced <- FALSE
            continueBiphasicDetection <- FALSE
          }
        }
       } else {
        stop(paste(biphasicRule$type, "not a valid biphasic rule"))
      }
    } else {
      continueBiphasicDetection <- FALSE
      testConc <- as.numeric(NA)
      biphasicParameterPreviousValue <- as.numeric(NA)
    }
    return(list(points = list(points), model.synced = model.synced, biphasicParameterPreviousValue = biphasicParameterPreviousValue, testConc = testConc, continueBiphasicDetection = continueBiphasicDetection))
  }
#   loadSession(s)
#   biphasicRule$value <- 0.90
#   fitData[ ,biphasicRule := NULL]
#   fitData[ ,biphasicRule := list(list(biphasicRule))]
  fitData[ , continueBiphasicDetection := TRUE]
  fitData[ , c("points","model.synced","biphasicParameterPreviousValue", "testConc", "continueBiphasicDetection") := testForPossibleBiphasic(biphasicRule[[1]], points[[1]], pointStats[[1]], model.synced, goodnessOfFit.model[[1]], inactive, fitConverged, potent, insufficientRange, continueBiphasicDetection = continueBiphasicDetection), by = curveid]
  while(any(!fitData$model.synced)) {
    fitData <- doseResponseFit(fitData)
    fitData[ , c("points","model.synced","biphasicParameterPreviousValue", "testConc", "continueBiphasicDetection") := testForPossibleBiphasic(biphasicRule[[1]], 
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
                                                                                     continueBiphasicDetection), by = curveid]
  }
  
  
}




doseResponse.applyLimits <- function(fitData, iterations = 20) {
  #While refit is true, keep refitting using fixed parameters
  #The reason we check refit and continue is because we are dealing with limits, 
  #if max is above limit so then we limit it, does min then go below limit? ok, then limit that....etc.
  #Check if limits have been exceeded and refit if not inactive, non-converged or insufficient range
  checkRefit <- function(fitData) {
    refit <- fitData[ , switch(modelHint,
                               "LL.4" = {  maxExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "maxThreshold" %in% results.parameterRules[[1]]$limits)
                                           minExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "minThreshold" %in% results.parameterRules[[1]]$limits)
                                           slopeExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "slopeThreshold" %in% results.parameterRules[[1]]$limits)
                                           exceededAThreshold <- (maxExceeded | minExceeded | slopeExceeded)
                                           refit <-  exceededAThreshold & (!inactive | !fitConverged | !insufficientRange | !potent)
                                           refit
                               },{
                                 warning(paste0("Refit rule not implemented for ", modelHint))
                                 FALSE
                               }),
                     by = curveid]$V1
    return(refit)
  }
  refit <- checkRefit(fitData)
  i <- 1
  while(any(refit) & i < iterations) {
    fitData[refit, model.synced := FALSE]
    fitData[refit, fixedParameters := switch(modelHint,
                                             "LL.4" = {
                                               if(ifelse(is.null(fixedParameters[[1]]$max), FALSE, !is.na(fixedParameters[[1]]$max))) {
                                                 fixedMax <- fixedParameters[[1]]$max
                                               } else {
                                                 #fixedMax <- ifelse("maxThreshold" %in% results.parameterRules[[1]]$limits, parameterRules[[1]]$limits$maxThreshold$value, NA)
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
                                               list(list(myfixedParameters = list(kd = NA, max = NA)))
                                             }),
            by = curveid]
    fitData <- doseResponseFit(fitData)
    refit <- checkRefit(fitData)
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
#' file <- system.file("docs", "example-ec50-simple-fitSettings.json", package = "racas")
#' simpleSettingsJSON <- readChar(file, file.info(file)$size)
#' simpleSettings <- fromJSON(simpleSettingsJSON)
#' simpleToAdvancedFitSettings(simpleSettings)
simpleToAdvancedFitSettings <- function(simpleSettings, updateFlags = NULL, modelHint = "LL.4") {
  #simpleRequest <- simpleBulkDoseResponseFitRequest
  defaultSettings <- getDefaultFitSettings(modelHint)
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
  modifiedSettings <- defaultSettings
  modifiedSettings$inactiveRule$value <- simpleSettings$inactiveThreshold
  modifiedSettings$inverseAgonistMode <- simpleSettings$inverseAgonistMode
  if(!is.null(simpleSettings$biphasicRule)) {
    modifiedSettings$biphasicRule <- simpleSettings$biphasicRule
  }
  modifiedSettings <- switch(modelHint,
                             "LL.4" = updateFitSettings.LL4(modifiedSettings,simpleSettings),
                             warning(paste0("Simple to Advanced fit settings not implemented for modelHint: ",modelHint))
  )
  if(!is.null(updateFlags)) {
    modifiedSettings$updateFlags <- as.data.table(updateFlags)
  }
  return(modifiedSettings)
}

getDefaultFitSettings <- function(modelHint) {
  file <- system.file("docs", switch(modelHint,
                                     "LL.4" = "default-ec50-fitSettings.json",
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
#' file <- system.file("docs", "default-ec50-fitSettings.json", package = "racas")
#' fitSettingsJSON <- readChar(file, file.info(file)$size)
#' fitSettings <- fromJSON(fitSettingsJSON)
#' #fit the data
#' system.time(response <- doseResponse(fitSettings, curveids = curveids))
#' 
doseResponse <- function(fitSettings, curveids = NA, sessionID = NA, fitData = NA, simpleFitSettings = NULL, ...) {
  if(all(is.na(c(curveids, sessionID, fitData)))) stop("Must provide curveids or sessionID or fitData, all are NA")
  if(class(curveids) == "character") {
    fitData <- getFitData.curveID(curveids)
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
    fitData <- doseResponse.fitData(fitSettings, fitData) 
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

predictPoints <- function(pts, drcObj) {
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


plotWindow <- function(pts, logDose = TRUE, logResponse = FALSE, ymin = NA, ymax = NA, xmin = NA, xmax = NA){
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
captureOutput <- function(obj, ...) {
  val <- capture.output({
    result <- withVisible(obj)
    if (result$visible)
      print(result$value)
  })
  
  return(paste(val, ...))
}

objToHTMLTableString <- function(dataTable, ...) {
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

getReportedParameters <- function(modelHint, results, inactive, fitConverged, insufficientRange, potent, fixedParameters, fittedParameters, pointStats, goodnessOfFit.parameters, goodnessOfFit.model) {
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
             if(is.NULLorNA(fixedParameters$max)) {
               max <- list(value = fittedParameters$max, operator = NULL, stdErr = goodnessOfFit.parameters$max.stdErr)
             } else {
               max <- list(value = fixedParameters$max, operator = NULL, stdErr = NULL)
             }
           }
           if("minUncertaintyRule" %in% results$goodnessOfFits) {
             min <- list(value = pointStats$response.empiricalMin, operator = NULL, stdErr = NULL)
           } else {
             if(is.NULLorNA(fixedParameters$min)) {
               min <- list(value = fittedParameters$min, operator = NULL, stdErr = goodnessOfFit.parameters$min.stdErr)
             } else {
               min <- list(value = fixedParameters$min, operator = NULL, stdErr = NULL)
             }
           }
           if(is.NULLorNA(fixedParameters$slope)) {
             slope <- list(value = -fittedParameters$slope, operator = NULL, stdErr = goodnessOfFit.parameters$slope.stdErr)
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
             ec50 <- list(value = fittedParameters$ec50, operator = NULL, stdErr = goodnessOfFit.parameters$ec50.stdErr)
           }
           reportedValues <- list(min = min, max = max, slope = slope, ec50 = ec50)
           return(reportedValues)
         },
         "MM.2" = {
           if(inactive | insufficientRange) {
             max <- list(value = pointStats$response.empiricalMax, operator = NULL, stdErr = NULL)
             kd <- list(value = pointStats$dose.max, operator = NULL, stdErr = NULL)
             kdOperator <- ">"
             reportedValues <- list(max = max, kd = kd)
             return(reportedValues)
           }
           if(!fitConverged) {
             return(list())
           }
           if("maxUncertaintyRule" %in% results$goodnessOfFits) {
             max <- list(value = pointStats$response.empiricalMax, operator = NULL, stdErr = NULL)
           } else {
             if(is.NULLorNA(fixedParameters$max)) {
               max <- list(value = fittedParameters$max, operator = NULL, stdErr = goodnessOfFit.parameters$max.stdErr)
             } else {
               max <- list(value = fixedParameters$max, operator = NULL, stdErr = NULL)
             }
           }
           if("kdThreshold" %in% results$limits) {
             kd <- list(value = pointStats$dose.max, operator = NULL, stdErr = NULL)
             kdOperator <- ">"
           } else {
             kd <- list(value = fittedParameters$kd, operator = NULL, stdErr = goodnessOfFit.parameters$kd.stdErr)
             kdOperator <- ""
           }
           reportedValues <- list(max = max, kd = kd)
           return(reportedValues)
         },
{warning("Not implemented for ", modelHint)
 return(list())
}
  )
}

getFitData.curveID <- function(curveID, include = "fullobject") {
  myMessenger <- messenger()
  myMessenger$logger$debug("Doing query to convert curve id to analyis group id")
  analyisGroupIDOfCurveID <- query(paste0("select ags.analysis_group_id 
               from analysis_group_state ags 
               join analysis_group_value agv 
               on agv.analysis_state_id=ags.id where agv.string_value = ", sqliz(curveID)))[[1]]
  fitData <- getFitData.analysisGroupID(analyisGroupIDOfCurveID, include)
  return(fitData)
}

getFitData.analysisGroupID <- function (analysisGroupdID, include) {
  myMessenger <- messenger()
  myMessenger$logger$debug("Calling analysisgroup id service")
  serviceURL <- paste0(racas::applicationSettings$client.service.persistence.fullpath, "analysisgroups/", analysisGroupdID, "?with=", include)
  myMessenger$logger$debug(serviceURL)  
  analyisGroupJSON <- getURL(paste0(racas::applicationSettings$client.service.persistence.fullpath, "analysisgroups/", analysisGroupdID, "?with=", include))
  myMessenger$logger$debug("Parsing analyis group json")
  analysisGroup <- jsonlite::fromJSON(analyisGroupJSON[[1]])
  fitData <- listToDataTable(analysisGroup)
  return(fitData)
}

getFitData.experimentCode <- function(experimentCode, include = "fullobject", ...) {
  myMessenger <- messenger()
  myMessenger$logger$debug("calling experiment code name service")
  serviceURL <- paste0(racas::applicationSettings$client.service.persistence.fullpath, "experiments/codename/", experimentCode, "?with=", include)
  myMessenger$logger$debug(serviceURL)
  experimentJSON <- getURL(serviceURL)
  myMessenger$logger$debug("parsing experiment json")
  experiment <- jsonlite::fromJSON(experimentJSON[[1]])
  fitData <- as.data.table(experiment$analysisGroups[[1]][!experiment$analysisGroups[[1]]$ignored,])
}

getFitData <- function(entityID, type = c("experimentCode","analysisGroupID", "curveID"), include = "fullobject", ...) {
  type <- match.arg(type)
  myMessenger <- messenger()
  fitData <- switch(type,
                    "experimentCode" = getFitData.experimentCode(entityID, include),
                    "curveID" = getFitData.curveID(entityID, include),
                    "analysisGroupID" = getFitData.analysisGroupID(entityID, include)
  )
  
  myMessenger$logger$debug("Extracting curve parameters")
  fitData[ , lsStates := list(list(
    rbindlist(lsStates)[ignored == FALSE & lsKind=="Dose Response"]
  )), by = id]
  fitData[ , parameters := list(list(
    rbindlist(lsStates[[1]][ , lsValues:= list(list(lsValues[[1]][ , order(names(lsValues[[1]]))])), by = id]$lsValues)
  )), by = id]
  fitData[ , curveid := parameters[[1]][grepl('.*curve id', lsKind)]$stringValue , by = id]
  myParameterRules <- list(goodnessOfFits = list(), limits = list())
  myInactiveRule <- list()
  myInverseAgonistMode <- TRUE
  myBiphasicRule <- list()
  myFixedParameters <- list()
  fitData[ , c("parameterRules", "inactiveRule", "fixedParameters", "inverseAgonistMode", "biphasicRule") := list(list(myParameterRules),
                                                                            list(myInactiveRule),
                                                                            myInverseAgonistMode,
                                                                            list(myBiphasicRule),
                                                                            list(myFixedParameters))]
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
    myMessenger$logger$debug("Extracting curve points")
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
    myMessenger$logger$debug("Pivoting the curve points")
    fitData[ , points := list(list({
      dr <- data.table::dcast.data.table(points[[1]][lsKind %in% c("Dose", "Response")], subj_id+response_ss_id+response_ss_version+tg_id ~ lsKind, value.var = "numericValue")
      dr <- merge(dr, points[[1]][ lsKind=="Response", c("subj_id", "id"), with =  FALSE], by = "subj_id")
      drUnits <- dcast.data.table(points[[1]][lsKind %in% c("Dose", "Response")], subj_id ~ lsKind, value.var = "unitKind")
      setnames(drUnits, "Dose", "doseUnits")
      setnames(drUnits, "Response", "responseUnits")
      dr <- dr[drUnits]
      setnames(dr, "id", "response_sv_id")
      if(nrow(points[[1]][lsKind=="flag" & ignored == FALSE]) > 0) {
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
      bc <-  dcast.data.table(points[[1]][lsKind=="batch code"], subj_id ~ lsKind, value.var = "codeValue")
      setkey(fl, subj_id)
      setkey(bc, subj_id)
      setkey(dr, subj_id)
      pts <- fl[bc][dr]
      pts[ , flagchanged := FALSE]
      setnames(pts, names(pts), tolower(names(pts)))
      setcolorder(pts, order(names(pts)))
      pts
    })), by = id]
    myMessenger$logger$debug("Filling out the rest of the fit data object")
  }
  myMessenger$logger$debug(paste0("returning from getting experiment curve data with ", nrow(fitData), " curves"))
  return(fitData)
}

doseResponseFit <- function(fitData, refit = FALSE, ...) {
  fitDataNames <- names(fitData)
  ###Fit
  fitData[model.synced == FALSE, model := list(model = list(switch(modelHint,
                                                                   "LL.4" = getDRCModel(points[[1]], drcFunction = LL.4, paramNames = c("slope", "min", "max", "ec50"), fixed = fixedParameters[[1]]),
                                                                   "MM.3" = getDRCModel(points[[1]], drcFunction = MM.3, paramNames = c("slope","max", "kd"), fixed = fixedParameters[[1]]),
                                                                   "MM.2" = getDRCModel(points[[1]], drcFunction = MM.2, paramNames = c("max", "kd"), fixed = fixedParameters[[1]])
  ))
  ), by = curveid]
  
  ###Collect Stats
  fitData[ model.synced == FALSE, fitConverged := ifelse(unlist(lapply(model, is.null)), FALSE, model[[1]]$fit$convergence), by = curveid]
  fitData[ model.synced == FALSE, c("pointStats","fittedParameters", "goodnessOfFit.model", "goodnessOfFit.parameters") := list(pointStats = list(getPointStats(points[[1]])), 
                                                                                                                                fittedParameters = list(drcObject.getParameters(model[[1]])),
                                                                                                                                goodnessOfFit.model = list(drcObject.getDRCFitStats(model[[1]], points[[1]])),
                                                                                                                                goodnessOfFit.parameters = list(drcObject.getGoodnessOfFitParameters(model[[1]]))
  ), by = curveid]
  #Fail Heuristics  
  fitData[ model.synced == FALSE, results.parameterRules := list(list(list(goodnessOfFits = applyParameterRules.goodnessOfFits(goodnessOfFit.parameters[[1]], parameterRules[[1]]$goodnessOfFits),
                                                                           limits = applyParameterRules.limits(fittedParameters[[1]],pointStats[[1]], parameterRules[[1]]$limits)
  ))), by = curveid]
  
  fitData[ model.synced == FALSE, c("inactive", "insufficientRange", "potent") := applyInactiveRule(pointStats[[1]],points[[1]], inactiveRule[[1]], inverseAgonistMode), by = curveid]
  fitData[ model.synced == FALSE, "approved" := fitConverged | inactive | insufficientRange | potent, by = curveid]
  returnCols <- unique(c(fitDataNames, "model", "fitConverged", "pointStats", "fittedParameters", "goodnessOfFit.model", "goodnessOfFit.parameters", "inactive", "insufficientRange", "potent"))
  
  fitData[ model.synced == FALSE, model.synced := TRUE]
  return(fitData[, returnCols, with = FALSE])
}

categorizeFitData <- function(results.parameterRules, fitSettings, inactive, converged, insufficientRange, potent) {
  #   -weak tested potency
  #   strong tested potency
  #   -inactive
  #   Low Quality Fit - failed PValue Test
  #   hockey stick
  #   sigmoid 
  #   -lack of fit - fit did not converge
  category <- "sigmoid"
  resultList <- unlist(results.parameterRules)
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
  return(category)
}
applyParameterRules.limits <- function(fittedParameters, pointStats, rules) {
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

applyParameterRules.goodnessOfFits <- function(goodnessOfFit.parameters, rules) {
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

applyInactiveRule <- function(pointStats, points, rule, inverseAgonistMode) {
  saveSession("~/Desktop/apply")
  if(is.null(pointStats)) return(NULL)
  if(is.null(points)) return(NULL)
  threshold <- rule$value
  if(length(rule) > 0) {
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

getDRCModel <- function(dataSet, drcFunction = LL.4, subs = NA, paramNames = eval(formals(drcFunction)$names), fixed, robust = "mean") {
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

getPointStats <- function(pts) {
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

kiNames <- c("Top", "Bottom", "logKi")
oneSiteKi <- function(kd, ligandConc, fixed = c(NA, NA, NA), names = c("c", "d", "e")) {
  ki.fct3 <- param[,2] + (param[,1]-param[,2])/(1+10^(x-log10(10^param[,3]*(1+ligandConc/kd))))
  return(c(ki.fct, ki.ssft, ki.names))
}
oneSiteKi.ssf <- function(data) {
  Top <- max(data[,2])
  Bottom <- min(data[,2])
  logKi <- -8.0
  return(c(Top, Bottom, logKi))
}

kissfctFree <- function(data) {
  Top <- max(data[,2])
  KiuM <- 0.1
  Bottom <- min(data[,2])
  return(c(Bottom, KiuM, Top))
}

drcObject.getParameters <- function(drcObj = drcObject) {
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
drcObject.getDRCFitStats <- function(drcObject, points) {
  if(is.null(drcObject)) return(NULL)
  SSE <- sum((residuals(drcObject))^2)
  SST <- sum((points$response-mean(points[is.na(flag_user) & is.na(flag_on.load) & is.na(flag_algorithm) & is.na(flag_temp),]$response))^2)
  rSquared <- 1-(SSE/SST)
  return(list(SSE = SSE, SST = SST, rSquared = rSquared))
}
drcObject.getGoodnessOfFitParameters <- function(drcObj) {
  if(is.null(drcObj)) {
    return(NULL)
  }
  myMatrix <- coefficients(summary(drcObj))
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
#' fitDataToResponse.acas(fitData, cars)
fitDataToResponse.acas <- function(fitData, transactionId = -1, status, hasWarning, errorMessages = as.character(), ...) {
  #   response <- list(
  #     transactionId= transactionId,
  #     results= list(
  #       htmlSummary= capture.output(blah()),
  #       status= status
  #     ),
  #     hasError= hasError,
  #     hasWarning = FALSE,
  #     errorMessages= errorMessages
  #   )
  #rmd <- "inst/rmd/fitDataToResponse_acas.rmd"
  hasError <- length(errorMessages) > 0
  if(!hasError) {
    rmd <- system.file("rmd", "fitDataToResponse_acas.rmd", package="racas")
    htmlSummary <- knit2html.bugFix(input = rmd, 
                                    options = c("base64_images", "mathjax"),
                                    template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                    stylesheet = system.file("rmd", "racas_container.css", package="racas"))
  } else {
    rmd <- system.file("rmd", "fitDataToResponse_error.rmd", package="racas")
    htmlSummary <- knit2html.bugFix(input = rmd, 
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

knit2html.bugFix <- function (input, output = NULL, text = NULL, template = template, stylesheet = stylesheet, options = c()) {
  originalWD <- getwd()
  t <- tempdir()
  dir.create(t)
  setwd(t)
  tfile <- tempfile(tmpdir=t)
  out <- knitr::knit(input, output = tfile, text = NULL, envir = parent.frame(), encoding = getOption("encoding"), quiet = TRUE)
  output <- markdown::markdownToHTML(out, template = template, stylesheet = stylesheet, options = options, fragment.only = FALSE)
  setwd(originalWD)
  unlink(t, recursive = TRUE)
  return(output)
}

loadDoseResponseTestData <- function(size = c("small","large", "explicit")) {
  size <- match.arg(size)
  doseResponseSELFile <- switch(size,
                                "small" = system.file("docs", "Example-Dose-Response-SEL.xlsx", package="racas"),
                                "explicit" = system.file("docs", "example-dose-response-ec50-explicit.xlsx", package="racas"),
                                "large" = system.file("docs", "Example-Dose-Response-SEL-Large.xlsx", package="racas")
  )
  originalWD <- getwd()
  on.exit(  setwd(originalWD))
  acasHome <- normalizePath(file.path(path.package("racas"),"..",".."))
  selCode <- file.path(acasHome,"public","src","modules","GenericDataParser","src","server","generic_data_parser.R")
  setwd(acasHome)
  file.copy(doseResponseSELFile,"privateUploads", overwrite = TRUE)
  source(selCode, local = TRUE)
  request <- list(fileToParse=basename(doseResponseSELFile), dryRunMode = "false", user="bbolt")
  response <- parseGenericData(request)
  if(response$hasError) {
    cat(response$errorMessages[[1]]$message)
  }
  return(response$results$experimentCode)
}

flattenListToDataTable <- function(l) {
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

listToDataTable <- function(l) {
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