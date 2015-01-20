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
  if(nrow(updateFlags) > 0) {
    #updateFlags[flag=="NA", flag := as.character(NA)]
    setkey(updateFlags,"responseSubjectValueId" )
    update_point_flags <- function(pts, updateFlags) {  
      #save the column to return
      pts <- copy(pts)
      returnCols <- names(pts)
      setkey(pts, "responseSubjectValueId")
      pts <- merge(pts,updateFlags, all.x = TRUE, by = "responseSubjectValueId", suffixes = c("",".y"))
      pts[ , flagchanged :=  
            !identical(userFlagStatus,userFlagStatus.y) |
            !identical(userFlagObservation,userFlagObservation.y) | 
            !identical(userFlagReason,userFlagReason.y) | 
            !identical(userFlagComment,userFlagComment.y) | 
            !identical(algorithmFlagStatus,algorithmFlagStatus.y) |
            !identical(algorithmFlagObservation,algorithmFlagObservation.y) | 
            !identical(algorithmFlagReason,algorithmFlagReason.y) | 
            !identical(algorithmFlagComment,algorithmFlagComment.y) |          
            !identical(preprocessFlagStatus,preprocessFlagStatus.y) |
            !identical(preprocessFlagObservation,preprocessFlagObservation.y) | 
            !identical(preprocessFlagReason,preprocessFlagReason.y) | 
            !identical(preprocessFlagComment,preprocessFlagComment.y) |      
            flagchanged, by = "responseSubjectValueId" ]
      pts[flagchanged==TRUE , c('userFlagStatus', 'userFlagObservation', 'userFlagReason', 'userFlagComment', 'algorithmFlagStatus', 'algorithmFlagObservation', 'algorithmFlagReason', 'algorithmFlagComment', 'preprocessFlagStatus', 'preprocessFlagObservation', 'preprocessFlagReason', 'preprocessFlagComment' ):= list(userFlagStatus.y, userFlagObservation.y, userFlagReason.y, userFlagComment.y, algorithmFlagStatus.y, algorithmFlagObservation.y, algorithmFlagReason.y, algorithmFlagComment.y, preprocessFlagStatus.y, preprocessFlagObservation.y, preprocessFlagReason.y, preprocessFlagComment.y)]
      return(pts[, returnCols, with = FALSE])
    }
    fitData[, points := list(list(update_point_flags(points[[1]], updateFlags))) , by = curveId]
  }
  
  #Fit the data
  fitData <- dose_response_fit(fitData)
  
  #Biphasic Detection
  fitData <- biphasic_detection(fitData)
  
  #Applying Limits
  fitData <- apply_limits(fitData, iterations = 20)
  
  #Categorize the fit data
  fitData[ , category := categorize_fit_data(renderingHint, results.parameterRules[[1]], fitSettings[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]], pointStats[[1]]), by = curveId]
  
  #Extract the reported Parameters
  fitData[ , reportedParameters := {
    list(list(get_reported_parameters(renderingHint, results.parameterRules[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]], fixedParameters[[1]], fittedParameters[[1]], pointStats[[1]], goodnessOfFit.parameters[[1]], goodnessOfFit.model[[1]], algorithmFlagStatus[[1]], userFlagStatus[[1]])))
    }
    , by = curveId]
  
  return(fitData)
}

biphasic_detection <- function(fitData) {
  returnCols <- copy(names(fitData))
  test_for_biphasic <- function(biphasicRule, points, pointStats, model.synced, goodnessOfFit.model, category, biphasicParameterPreviousValue, testConc, continueBiphasicDetection, firstRun) {    
    points <- copy(points)
    if(!continueBiphasicDetection) {
      testConc <- as.numeric(NA)
      biphasicParameterPreviousValue <- as.numeric(NA)
      return(list(points = list(points), model.synced = model.synced, biphasicParameterPreviousValue = biphasicParameterPreviousValue, testConc = testConc, continueBiphasicDetection = continueBiphasicDetection))
    }
    if(firstRun) {
      #If detect biphasic is on and the following
      # there are doses above the empirical max dose with respnoses below empirical max respnose
      # the curve is not inactive, non-converged, insufficient range or potent 
      continueBiphasicDetection <- (length(biphasicRule) > 0) & pointStats$count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax > 0 & (category == "sigmoid")
      if(!continueBiphasicDetection) {
        testConc <- as.numeric(NA)
        biphasicParameterPreviousValue <- as.numeric(NA)
        return(list(points = list(points), model.synced = model.synced, biphasicParameterPreviousValue = biphasicParameterPreviousValue, testConc = testConc, continueBiphasicDetection = continueBiphasicDetection))
      } else {
        if(biphasicRule$type == "percentage") {
          biphasicParameterPreviousValue <- as.numeric(goodnessOfFit.model[biphasicRule$parameter][[1]])
          #           ifelse(!is.finite(max(sort(pointStats$doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax, decreasing = TRUE))), , TRUE)
          testConc <- max(sort(pointStats$doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax, decreasing = TRUE))
          points[dose == testConc, tempFlagStatus := "possible biphasic"]
          model.synced <- FALSE
          continueBiphasicDetection <- TRUE
        } else {
          stop(paste(biphasicRule$type, "not a valid biphasic rule type"))
        }
      }
    } else {      
      stillASigmoid <- category == "sigmoid"
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
        points[dose == testConc & flagchanged == FALSE, flagchanged := TRUE]
        points[dose == testConc, algorithmFlagStatus := "knocked out"]
        points[dose == testConc, algorithmFlagObservation := "biphasic"]
        points[dose == testConc, algorithmFlagReason := "biphasic"]
        
        points[dose == testConc, tempFlagStatus := ""]
        if(pointStats$count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax > 0) {
          testConc <- max(sort(pointStats$doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax, decreasing = TRUE))
          points[dose == testConc, tempFlagStatus := "possible biphasic"]
          model.synced <- FALSE
          continueBiphasicDetection <- TRUE
        } else {
          model.synced <- FALSE
          continueBiphasicDetection <- FALSE
        }
      } else {
        points[dose == testConc, tempFlagStatus := ""]
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
  fitData[ ,  tempCategory := categorize_fit_data(renderingHint, results.parameterRules[[1]], fitSettings[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]], pointStats[[1]]), by = curveId]
  fitData[ , c("points","model.synced","biphasicParameterPreviousValue", "testConc", "continueBiphasicDetection") := test_for_biphasic(biphasicRule[[1]], points[[1]], pointStats[[1]], model.synced, goodnessOfFit.model[[1]], tempCategory, biphasicParameterPreviousValue = biphasicParameterPreviousValue, continueBiphasicDetection = continueBiphasicDetection, firstRun = firstRun), by = curveId]
  fitData[ , firstRun := FALSE]
  while(any(!fitData$model.synced)) {
    fitData <- dose_response_fit(fitData)  
    fitData[ ,  tempCategory := categorize_fit_data(renderingHint, results.parameterRules[[1]], fitSettings[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]], pointStats[[1]]), by = curveId]
    fitData[ , c("points","model.synced","biphasicParameterPreviousValue", "testConc", "continueBiphasicDetection") := test_for_biphasic(biphasicRule[[1]], 
                                                                                                                                         points[[1]], 
                                                                                                                                         pointStats[[1]], 
                                                                                                                                         model.synced,
                                                                                                                                         goodnessOfFit.model[[1]], 
                                                                                                                                         tempCategory,
                                                                                                                                         biphasicParameterPreviousValue,
                                                                                                                                         testConc,
                                                                                                                                         continueBiphasicDetection,
                                                                                                                                         firstRun), by = curveId]
  }
  return(fitData[, returnCols, with = FALSE])
}

apply_limits <- function(fitData, iterations = 20) {
  #While refit is true, keep refitting using fixed parameters
  #The reason we check refit and continue is because we are dealing with limits, 
  #if max is above limit so then we limit it, does min then go below limit? ok, then limit that....etc.
  #Check if limits have been exceeded and refit if not inactive, non-converged or insufficient range
  check_refit <- function(fitData) {
    refit <- fitData[ , switch(renderingHint,
                               "4 parameter D-R" = {  maxExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "maxThreshold" %in% results.parameterRules[[1]]$limits)
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
                               "Ki Fit" = {  maxExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "maxThreshold" %in% results.parameterRules[[1]]$limits)
                                         minExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "minThreshold" %in% results.parameterRules[[1]]$limits)
                                         exceededAThreshold <- (maxExceeded | minExceeded)
                                         refit <-  exceededAThreshold & (!inactive | !fitConverged | !insufficientRange | !potent)
                                         refit
                               },{
                                 warnUser(paste0("Limit Refit rule not implemented for ", renderingHint))
                                 FALSE
                               }),
                     by = curveId]$V1
    return(refit)
  }
  refit <- check_refit(fitData)
  i <- 1
  while(any(refit) & i < iterations) {
    fitData[refit, model.synced := FALSE]
    fitData[refit, fixedParameters := switch(renderingHint,
                                             "4 parameter D-R" = {
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
                                             },
                                             "Ki Fit" = {
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
                                               list(list(myfixedParameters = list(max = fixedMax,min = fixedMin, ki = NA)))
                                             }
    ),
    by = curveId]
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
#' @param renderingHint a character string to identify the dose response model to fit
#' @return an advanced fit settings list object
#' @export
#' @examples
#' file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas")
#' simpleSettingsJSON <- readChar(file, file.info(file)$size)
#' simpleSettings <- fromJSON(simpleSettingsJSON)
#' simple_to_advanced_fit_settings(simpleSettings)
simple_to_advanced_fit_settings <- function(simpleSettings, updateFlags = NULL, renderingHint = "4 parameter D-R") {
  #simpleRequest <- simpleBulkDoseResponseFitRequest
  defaultSettings <- get_default_fit_settings(renderingHint)
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
  updateFitSettings.Ki <- function(fitSettings, simpleFitSettings) {
    update.fitSetting.parameter.Ki <- function(fitSettings, name, simpleSettingsParameter) {   
      if(is.null(simpleSettingsParameter$limitType) || simpleSettingsParameter$limitType=="pin") {
        fitSettings$parameterRules$limits[[paste0(name,"Threshold")]] <- NULL
        fitSettings$fixedParameters[[name]] <- ifelse(name=="slope",-simpleSettingsParameter$value,simpleSettingsParameter$value)
        return(fitSettings)
      }
      if(simpleSettingsParameter$limitType=="none") {
        fitSettings$parameterRules$limits[[paste0(name,"Threshold")]] <- NULL
        fitSettings$fixedParameters[[name]] <- NULL
        return(fitSettings)
      }
      if(simpleSettingsParameter$limitType=="limit") {
        fitSettings$parameterRules$limits[[paste0(name,"Threshold")]] <- list(parameter = name,
                                                                              type = "threshold",
                                                                              operator = switch(name,
                                                                                                max = ">",
                                                                                                min = "<",
                                                                                                ki = ">",
                                                                                                stop(paste0("Unknown parameter:",name))),
                                                                              value = ifelse(name=="slope",-simpleSettingsParameter$value,simpleSettingsParameter$value),
                                                                              displayName = paste0(name," threshold exceeded")
        )
        fitSettings$fixedParameters[[name]] <- NULL
        return(fitSettings)
      }
    }
    fitSettings <- update.fitSetting.parameter.Ki(fitSettings, name = "min", simpleSettingsParameter = simpleSettings$min)
    fitSettings <- update.fitSetting.parameter.Ki(fitSettings, name = "max", simpleSettingsParameter = simpleSettings$max)
    fitSettings <- update.fitSetting.parameter.Ki(fitSettings, name = "kd", simpleSettingsParameter = simpleSettings$kd)
    fitSettings <- update.fitSetting.parameter.Ki(fitSettings, name = "ligandConc", simpleSettingsParameter = simpleSettings$ligandConc)
    
    return(fitSettings)
  }
  modifiedSettings <- defaultSettings
  if(!is.null(simpleSettings$smartMode) && simpleSettings$smartMode) {
    if(simpleSettings$inactiveThresholdMode) {
      modifiedSettings$inactiveRule$value <- simpleSettings$inactiveThreshold    
    } else {
      modifiedSettings$inactiveRule <- list()    
    }
    modifiedSettings$inverseAgonistMode <- simpleSettings$inverseAgonistMode
    if(!is.null(simpleSettings$biphasicRule)) {
      modifiedSettings$biphasicRule <- simpleSettings$biphasicRule
    }    
  } else {
    modifiedSettings$biphasicRule <- list()
    modifiedSettings$inactiveRule <- list()
    modifiedSettings$parameterRules$goodnessOfFits <- list()
    modifiedSettings$parameterRules$limits <- list()
  }
  if(!is.null(simpleSettings$userFlagStatus)) {
    modifiedSettings$userFlagStatus <- simpleSettings$userFlagStatus
  }
  
  modifiedSettings <- switch(renderingHint,
                             "4 parameter D-R" = updateFitSettings.LL4(modifiedSettings,simpleSettings),
                             "MM.2" = updateFitSettings.MM2(modifiedSettings,simpleSettings),
                             "Ki Fit" = updateFitSettings.Ki(modifiedSettings,simpleSettings),
                             warnUser(paste0("Simple to Advanced fit settings not implemented for renderingHint: ",renderingHint))
  )
  if(!is.null(updateFlags)) {
    modifiedSettings$updateFlags <- as.data.table(updateFlags)
  }
  return(modifiedSettings)
}

get_default_fit_settings <- function(renderingHint) {
  file <- system.file("conf", switch(renderingHint,
                                     "4 parameter D-R" = "default-ec50-fitSettings.json",
                                     "MM.2" = "default-kd-fitSettings.json",
                                     "Ki Fit" = "default-ki-fitSettings.json",
                                     stop("renderingHint \'", renderingHint,"\' does not have a default fit settings json object")), package = "racas")
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
#' system.time(response <- get_fit_data_experiment_code(fitSettings, curveids = curveids))
#' 
dose_response_session <- function(fitSettings, curveids = NA, sessionID = NA, fitData = NA, simpleFitSettings = NULL, flagUser = NULL, user = NULL, ...) {
  if(all(is.na(c(curveids, sessionID, fitData)))) stop("Must provide curveids or sessionID or fitData, all are NA")
  if(class(curveids) == "character") {
    fitData <- get_fit_data_curve(curveids)
  }
  if(!is.na(sessionID)) {
    fitSettings_new <- fitSettings
    sessionID_new <- sessionID
    simpleFitSettings_new <- simpleFitSettings
    flagUser_new <- flagUser
    loadSession(sessionID)
    fitSettings <- fitSettings_new
    sessionID <- sessionID_new
    simpleFitSettings <- simpleFitSettings_new
    flagUser <- flagUser_new    
    
    rm(fitSettings_new,sessionID_new, simpleFitSettings_new, flagUser_new)
    if(exists("fitData")) {
      fitData[, model.synced := FALSE]      
    } else {
      stop("fitData object does not exist in session, are you sure this was a fit session?")
    }
  }
  if(any(class(fitData) == "data.table")) {
    if(!is.null(fitData$simpleFitSettings)) {
      fitData[ , simpleFitSettings := NULL]
    }
    fitData[ , simpleFitSettings := toJSON(simpleFitSettings)]
    if(!is.null(flagUser)) {
      fitData[ , userFlagStatus := flagUser]
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
    if(logDose) {
      maxDose <- max(pts$dose[pts$dose > 0])
      minDose <- min(pts$dose[pts$dose > 0])
    } else {
      maxDose <- max(pts$dose)
      minDose <- min(pts$dose)
    }
    if(logResponse) {
      maxResponse <- max(pts$response[pts$response > 0])
      minResponse <- min(pts$response[pts$response > 0])
    } else {
      maxResponse <- max(pts$response)
      minResponse <- min(pts$response)
    }
    responseRange <- abs(maxResponse-minResponse)
    doseRange <- abs(maxDose-minDose)
    if(is.na(ymin)) {
      if(logResponse) {
        ymin <- 10^(log10(minResponse) - 0.5)        
      } else {
        if(responseRange != 0) {
          ymin <- (minResponse - 0.10*responseRange)
        } else {
          ymin <- floor(maxResponse)
        }
      }
    }
    if(is.na(ymax)) {
      if(logResponse) {
        ymax <- 10^(log10(maxResponse) + 0.5)        
      } else {
        if(responseRange != 0) {
          ymax <- (maxResponse + 0.10*responseRange)
        } else {
          ymax <- ceiling(maxResponse)
        }
      }
    }
    if(is.na(xmax)) {
      if(logDose) {
        xmax <- 10^(log10(maxDose) + 0.5)        
      } else {
        xmax <- maxDose + abs(0.1 * doseRange)
      }  
    }
    if(is.na(xmin)) {
      if(logDose) {
        xmin <- 10^(log10(minDose) - 0.5)        
      } else {
        xmin <- minDose - abs(0.1 * doseRange)
      }
    }
    return(c(xmin,ymax,xmax,ymin))
  }
}
get_reported_parameters <- function(renderingHint, results, inactive, fitConverged, insufficientRange, potent, fixedParameters, fittedParameters, pointStats, goodnessOfFit.parameters, goodnessOfFit.model, algorithmFlagStatus, userFlagStatus) {
  switch(renderingHint,
         "4 parameter D-R" = {
           if(algorithmFlagStatus != "" | identical(userFlagStatus, "rejected")) {
             max <- list(value = ifelse(identical(userFlagStatus, "rejected"), userFlagStatus, algorithmFlagStatus), operator = NULL, stdErr = NULL)
             min <- list(value = ifelse(identical(userFlagStatus, "rejected"), userFlagStatus, algorithmFlagStatus), operator = NULL, stdErr = NULL)
             ec50 <- list(value = ifelse(identical(userFlagStatus, "rejected"), userFlagStatus, algorithmFlagStatus), operator = NULL, stdErr = NULL)
             slope <- list(value = ifelse(identical(userFlagStatus, "rejected"), userFlagStatus, algorithmFlagStatus), operator = NULL, stdErr = NULL)
             reportedValues <- list(min = min, max = max, ec50 = ec50, slope = slope)
             return(reportedValues)
           }
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
         "Ki Fit" = {
           if(algorithmFlagStatus != "" | identical(userFlagStatus, "rejected")) {
             max <- list(value = ifelse(identical(userFlagStatus, "rejected"), userFlagStatus, algorithmFlagStatus), operator = NULL, stdErr = NULL)
             min <- list(value = ifelse(identical(userFlagStatus, "rejected"), userFlagStatus, algorithmFlagStatus), operator = NULL, stdErr = NULL)
             ki <- list(value = ifelse(identical(userFlagStatus, "rejected"), userFlagStatus, algorithmFlagStatus), operator = NULL, stdErr = NULL)
             reportedValues <- list(min = min, max = max, ki = ki)
             return(reportedValues)
           }
           if(potent) {
             max <- list(value = pointStats$response.empiricalMax, operator = NULL, stdErr = NULL)
             min <- list(value = pointStats$response.empiricalMin, operator = NULL, stdErr = NULL)
             ki <- list(value = pointStats$dose.min, operator = "<", stdErr = NULL)
             reportedValues <- list(min = min, max = max, ki = ki)
             return(reportedValues)
           }
           if(inactive | insufficientRange) {
             max <- list(value = pointStats$response.empiricalMax, operator = NULL, stdErr = NULL)
             min <- list(value = pointStats$response.empiricalMin, operator = NULL, stdErr = NULL)
             ki <- list(value = pointStats$dose.max, operator = ">", stdErr = NULL)
             reportedValues <- list(min = min, max = max, ki = ki)
             return(reportedValues)
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
           if(("kiThresholdHigh" %in% results$limits | "maxUncertaintyRule" %in% results$goodnessOfFits) | ("kiThresholdLow" %in% results$limits)) {
             if(("kiThresholdHigh" %in% results$limits | "kiUncertaintyRule" %in% results$goodnessOfFits)) {
               ki <- list(value = pointStats$dose.max, operator = ">", stdErr = NULL)
             } else {
               ki <- list(value = pointStats$dose.min, operator = "<", stdErr = NULL)
             }
           } else {
             ki <- list(value = fittedParameters$ki, operator = NULL, stdErr = if(is.finite(goodnessOfFit.parameters$ki.stdErr)) {goodnessOfFit.parameters$ki.stdErr} else {NULL})
           }
           reportedValues <- list(min = min, max = max, ki = ki)
           return(reportedValues)
         },{
           warnUser(paste0("Reported parameter rules implemented for ", renderingHint))
           return(list())
         }
  )
}
#' tsv service url to data.table
#'
#' Calls a tsv service and returns a data.table of results
#' 
#' @param url a url encoded string that calls a service that returns a tsv
#' @param type simple (uses data.table's fread which is fast but can't handle embedded html or \t values) or complex (uses read.csv which is slower but can handle embeded tables .etc.)
#' @return a data.table result
#' @export
#' @examples
#' # AG values for dose response have complex results
#' url <- "http://host4.labsynch.com:8080/acas/api/v1/experiments/EXPT-00000408/agvalues/bystate/data/Dose%20Response/tsv"
#' tsv_url_to_data_table(url, "complex")
#' 
#' Subject values fo rdose response have simple results
#' url <- "http://host4.labsynch.com:8080/acas/api/v1/experiments/EXPT-00000408/subjectvalues/bystate/data/results/tsv"
#' tsv_url_to_data_table(url, "simple")
#' 
tsv_url_to_data_table <- function(url, type = c("simple", "complex"), ...) {
  myMessenger <- messenger()
  myMessenger$logger$debug(url)
  type <- match.arg(type)
  response <- getURL(url)
  if(type == "simple") {
    tsv_data_table <- suppressWarnings(fread(response, sep = "\t", stringsAsFactors=FALSE))
  }
  if(type == "complex") {
    on.exit(close(con))
    con <- textConnection(response)
    tsv_data_frame <- read.csv(con, sep = "\t", stringsAsFactors=FALSE)
    tsv_data_table <- as.data.table(tsv_data_frame)
  }  
  tsv_data_table[ ,ignored := as.logical(ignored)]
  tsv_data_table[ ,publicData := as.logical(publicData)]
  return(tsv_data_table)
}
get_dose_response_values <- function(id, type = c("analysisgroupvalues", "treatmentgroupvalues", "subjectgroupvalues"), by = c("experiment", "analysisgroup")) {
  type <- match.arg(type)
  by <- match.arg(by)
  typeRoute <- switch(type,
                      "analysisgroupvalues" = list("/agvalues/bystate/data/dose response/tsv"),
                      "treatmentgroupvalues" = list("/tgvalues/bystate/data/results/tsv"),
                      "subjectgroupvalues" = list("/subjectvalues/bystate/data/results/tsv", "/subjectvalues/bystate/data/test compound treatment/tsv")
  )
  outputType <- switch(type,
                       "analysisgroupvalues" = "complex",
                       "treatmentgroupvalues" = "simple",
                       "subjectgroupvalues" = "simple"
  )
  byRoute <- switch(by,
                    experiment = "experiments/",
                    analysisgroup = "analysisgroups/"
  )
  urls <- lapply(paste0(racas::applicationSettings$client.service.persistence.fullpath, byRoute, id, typeRoute), URLencode)
  values <- lapply(urls, tsv_url_to_data_table, type = outputType)
  values <- rbindlist(values)
  return(values)  
}

get_analysisgroup_values <- function(experimentID, analysisGroupID) {
  myMessenger <- messenger()
  if(missing(experimentID)) {
    ag_values <- get_dose_response_values(analysisGroupID, type = "analysisgroupvalues", by = "analysisgroup")
  } else {
    ag_values <- get_dose_response_values(experimentID, type = "analysisgroupvalues", by = "experiment")    
  }
  return(ag_values)
}
curve_fit_controller_getFitDataByExperimentIdOrCodeName <- function(experiment, format = "tsv") {
  url <- URLencode(paste0(racas::applicationSettings$client.service.persistence.fullpath,"curvefit/fitdata?format=",format,"&experiment=",experiment))
  response <- getURL(url)
  return(response)
}
curve_fit_controller_getRawDataByExperimentIdOrCodeName <- function(experiment, format = "tsv") {
  url <- URLencode(paste0(racas::applicationSettings$client.service.persistence.fullpath,"curvefit/rawdata?format=",format,"&experiment=",experiment))
  response <- getURL(url)
  return(response)
}
curve_fit_controller_getRawDataByCurveId <- function(curveids, format = "tsv") {
  response <- getURL(
    paste0(racas::applicationSettings$client.service.persistence.fullpath, "curvefit/rawdata?format=",format),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(list(curveids))
  )
  return(response)
}
curve_fit_controller_getFitDataByCurveId <- function(curveids, format = "tsv") {
  response <- getURL(
    paste0(racas::applicationSettings$client.service.persistence.fullpath, "curvefit/fitdata?format=",format),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(list(curveids))
  )
  return(response)
}
get_treatmentgroup_values <- function(experimentID, analysisGroupID) {
  myMessenger <- messenger()
  if(missing(experimentID)) {
    tg_values <- get_dose_response_values(analysisGroupID, type = "treatmentgroupvalues", by = "analysisgroup")
  } else {
    tg_values <- get_dose_response_values(experimentID, type = "treatmentgroupvalues", by = "experiment")    
  }
  return(tg_values)
}
get_subjectgroup_values <- function(experimentID, analysisGroupID) {
  myMessenger <- messenger()
  if(missing(experimentID)) {
    tg_values <- get_dose_response_values(analysisGroupID, type = "subjectgroupvalues", by = "analysisgroup")
  } else {
    tg_values <- get_dose_response_values(experimentID, type = "subjectgroupvalues", by = "experiment")    
  }
  return(tg_values)
}
curve_fit_controller_fitData_response_to_data_table <- function(curveFitControllerFitDataResponse) {
  firstRow <- fread(curveFitControllerFitDataResponse, nrow = 1)
  fitData <- switch(firstRow$renderingHint,
                    "4 parameter D-R" = fread(curveFitControllerFitDataResponse,   colClasses = c(curveId = "character",
                                                                                                  analysisGroupCode = "integer",
                                                                                                  recordedBy = "character",
                                                                                                  batchCode = "character",
                                                                                                  Category = "character",
                                                                                                  renderingHint = "character",
                                                                                                  min = "numeric",
                                                                                                  max = "numeric",
                                                                                                  ec50 = "numeric",
                                                                                                  minUnits = "character",
                                                                                                  maxUnits = "character",
                                                                                                  ec50Units = "character",
                                                                                                  slope = "numeric",
                                                                                                  fittedMin = "numeric",
                                                                                                  fittedMax = "numeric",
                                                                                                  fittedEC50 = "numeric",
                                                                                                  fittedSlope = "numeric",
                                                                                                  sse = "numeric",
                                                                                                  sst = "numeric",
                                                                                                  rsquared = "numeric",
                                                                                                  curveErrorsClob = "character",
                                                                                                  reportedValuesClob = "character",
                                                                                                  parameterStdErrorsClob = "character",
                                                                                                  fitSettings = "character",
                                                                                                  fitSummaryClob = "character",
                                                                                                  userFlagStatus = "character",
                                                                                                  algorithmFlagStatus = "character"
                    ), sep = "\t"),
                    "Ki Fit" = fread(curveFitControllerFitDataResponse,   colClasses = c(curveId = "character",
                                                                                         analysisGroupCode = "integer",
                                                                                         recordedBy = "character",
                                                                                         batchCode = "character",
                                                                                         Category = "character",
                                                                                         renderingHint = "character",
                                                                                         min = "numeric",
                                                                                         max = "numeric",
                                                                                         ki = "numeric",
                                                                                         minUnits = "character",
                                                                                         maxUnits = "character",
                                                                                         kiUnits = "character",
                                                                                         fittedMin = "numeric",
                                                                                         fittedMax = "numeric",
                                                                                         fittedKi = "numeric",
                                                                                         ligandConc = "numeric",
                                                                                         kd = "numeric",
                                                                                         sse = "numeric",
                                                                                         sst = "numeric",
                                                                                         rsquared = "numeric",
                                                                                         curveErrorsClob = "character",
                                                                                         reportedValuesClob = "character",
                                                                                         parameterStdErrorsClob = "character",
                                                                                         fitSettings = "character",
                                                                                         fitSummaryClob = "character",
                                                                                         userFlagStatus = "character",
                                                                                         algorithmFlagStatus = "character"
                    ), sep = "\t")
  )
  setkey(fitData, "curveId")
  return(fitData)
}
curve_fit_controller_fitData_dataTable_to_fitData <- function(serviceDataTable) {
  #Flags
  #Parameters Rules
  serviceDataTable[ , c("parameterRules", "inactiveRule", "fixedParameters", "inverseAgonistMode", "biphasicRule") := list(list(list(goodnessOfFits = list(), limits = list())),
                                                                                                                           list(list()),
                                                                                                                           list(list()),
                                                                                                                           TRUE,
                                                                                                                           list(list()))]
  serviceDataTable[ , model.synced := FALSE]
  return(serviceDataTable)
}
curve_fit_controller_rawData_response_to_data_table <- function(curveFitControllerRawDataResponse) {
  rawData <- fread(curveFitControllerRawDataResponse,   colClasses = c(curveId = "character",
                                                                       responseSubjectValueId = "integer",
                                                                       dose = "numeric",
                                                                       doseUnits = "character",
                                                                       response = "numeric",
                                                                       responseUnits = "character",
                                                                       algorithmFlagStatus = "character",
                                                                       algorithmFlagObservation = "character",
                                                                       algorithmFlagReason = "character",
                                                                       algorithmFlagComment = "character",
                                                                       preprocessFlagStatus = "character",
                                                                       preprocessFlagObservation = "character",
                                                                       preprocessFlagReason = "character",
                                                                       preprocessFlagComment = "character",
                                                                       userFlagStatus = "character",
                                                                       userFlagObservation = "character",
                                                                       userFlagReason = "character",
                                                                       userFlagComment = "character"
  ))
  setkey(rawData, "curveId")
  return(rawData)
}
get_fit_data_experiment_code <- function(experimentCode, full_object = FALSE, ...) {
  myMessenger <- messenger()
  myMessenger$logger$debug("getting fitData")
  curveFitController_fitDataResponse <- curve_fit_controller_getFitDataByExperimentIdOrCodeName(experimentCode)
  serviceFitData <- curve_fit_controller_fitData_response_to_data_table(curveFitController_fitDataResponse)
  if(nrow(serviceFitData) == 0) {
    msg <- "no experiment results found"
    myMessenger$logger$error(msg)
    stop(msg)
  }
  myMessenger$logger$debug("converting service return to fit_data object") 
  fitData <- curve_fit_controller_fitData_dataTable_to_fitData(serviceFitData)
  #Treatmeng Groups and Subject Groups
  if(full_object) {
    myMessenger$logger$debug("getting rawData")
    curveFitController_rawDataResponse <- curve_fit_controller_getRawDataByExperimentIdOrCodeName(experimentCode)
    rawData <- curve_fit_controller_rawData_response_to_data_table(curveFitController_rawDataResponse)
    rawData[ ,tempFlagStatus := ""]
    rawData[ ,flagchanged := FALSE]
    rawData <- rawData[ , list(list(.SD)), keyby = "curveId"]
    setnames(rawData, "V1", "points")
    fitData <- fitData[rawData]
  }
  myMessenger$logger$debug(paste0("returning with ", nrow(fitData), " curves"))
  return(fitData)
}
get_analysis_group_id_from_curve_id <- function(curveID) {
  analyisGroupIDOfCurveID <- query(paste0("select ags.analysis_group_id 
                                          from analysis_group_state ags 
                                          join analysis_group_value agv 
                                          on agv.analysis_state_id=ags.id where agv.string_value = ", sqliz(curveID)))
  return(analyisGroupIDOfCurveID)
}
get_fit_data_curve_id <- function(curveids, full_object = TRUE) {
  curveFitController_fitDataResponse <- curve_fit_controller_getFitDataByCurveId(curveids)
  serviceFitData <- curve_fit_controller_fitData_response_to_data_table(curveFitController_fitDataResponse)
  fitData <- curve_fit_controller_fitData_dataTable_to_fitData(serviceFitData)
  #Treatmeng Groups and Subject Groups
  if(full_object) {
    curveFitController_rawDataResponse <- curve_fit_controller_getRawDataByCurveId(curveids)
    rawData <- curve_fit_controller_rawData_response_to_data_table(curveFitController_rawDataResponse)
    rawData[ ,tempFlagStatus := ""]
    rawData[ , flagchanged := FALSE]
    rawData <- rawData[ , list(list(.SD)), .SDcols = 1:ncol(rawData), keyby = "curveId"]
    setnames(rawData, "V1", "points")
    fitData <- fitData[rawData]
  }
  return(fitData)
}
dose_response_fit <- function(fitData, refit = FALSE, ...) {
  fitDataNames <- names(fitData)
  ###Fit
  fitData[model.synced == FALSE, model := list(model = list(switch(renderingHint,
                                                                   "4 parameter D-R" = get_drc_model(points[[1]], drcFunction = LL.4, paramNames = c("slope", "min", "max", "ec50"), fixed = fixedParameters[[1]]),
                                                                   "Ki Fit" = get_drc_model(points[[1]], drcFunction = ki_fct.5, paramNames = c("min", "max", "ki", "ligandConc", "kd"), fixed = fixedParameters[[1]]),
                                                                   "MM.2" = get_drc_model(points[[1]], drcFunction = MM.2, paramNames = c("max", "kd"), fixed = fixedParameters[[1]])
  ))
  ), by = curveId]
  
  ###Collect Stats
  fitData[ model.synced == FALSE, fitConverged := ifelse(unlist(lapply(model, is.null)), FALSE, model[[1]]$fit$convergence), by = curveId]
  fitData[ model.synced == FALSE, c("pointStats","fittedParameters", "goodnessOfFit.model", "goodnessOfFit.parameters") := {
    list(pointStats = list(get_point_stats(points[[1]])),
         fittedParameters = list(get_parameters_drc_object(model[[1]])),
         goodnessOfFit.model = list(get_fit_stats_drc_object(model[[1]], points[[1]])),
         goodnessOfFit.parameters = list(get_goodness_of_fit_parameters_drc_object(model[[1]]))
    )}
    , by = curveId]
  #Fail Heuristics  
  fitData[ model.synced == FALSE, results.parameterRules := list(list(list(goodnessOfFits = apply_parameter_rules_goodness_of_fits(goodnessOfFit.parameters[[1]], parameterRules[[1]]$goodnessOfFits),
                                                                           limits = apply_parameter_rules_limits(fittedParameters[[1]],pointStats[[1]], parameterRules[[1]]$limits)
  ))), by = curveId]
  fitData[ model.synced == FALSE, c("inactive", "insufficientRange", "potent") := apply_inactive_rules(pointStats[[1]],points[[1]], inactiveRule[[1]], inverseAgonistMode), by = curveId]
  fitData[ model.synced == FALSE, algorithmFlagStatus := ifelse((fitConverged | inactive | insufficientRange | potent) & !pointStats[[1]]$dose.count < 2, as.character(""), "no fit"), by = curveId]
  returnCols <- unique(c(fitDataNames, "model", "fitConverged", "pointStats", "fittedParameters", "goodnessOfFit.model", "goodnessOfFit.parameters", "inactive", "insufficientRange", "potent"))
  
  fitData[ model.synced == FALSE, model.synced := TRUE]
  return(fitData[, returnCols, with = FALSE])
}

categorize_fit_data <- function(renderingHint, results.parameterRules, fitSettings, inactive, converged, insufficientRange, potent, pointStats) {
  #   -weak tested potency
  #   strong tested potency
  #   -inactive
  #   Low Quality Fit - failed PValue Test
  #   hockey stick
  #   sigmoid 
  #   -lack of fit - fit did not converge
  category <- "sigmoid"
  resultList <- unlist(results.parameterRules)
  category <- switch(renderingHint,
                     "4 parameter D-R" = {
                       if(!converged) {
                         category <- "lack of fit - fit did not converge"
                       }
                       if(insufficientRange) {
                         category <- "insufficient range"
                       }
                       if("maxUncertaintyRule" %in% resultList | "ec50ThresholdHigh" %in% resultList) {
                         category <- "weak tested potency"
                       }
                       if("ec50ThresholdLow" %in% resultList | potent) {
                         category <- "strong tested potency"
                       }
                       if(inactive) {
                         category <- "inactive"
                       }
                       if(pointStats$dose.count < 2) {
                         category <- "insufficient data"
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
                       if(pointStats$dose.count < 2) {
                         category <- "insufficient data"
                       }
                       category
                     },
                     "Ki Fit" = {
                       if(!converged) {
                         category <- "lack of fit - fit did not converge"
                       }
                       if(insufficientRange) {
                         category <- "insufficient range"
                       }
                       if("maxUncertaintyRule" %in% resultList | "kiThresholdHigh" %in% resultList) {
                         category <- "weak tested potency"
                       }
                       if("kiThresholdLow" %in% resultList | potent) {
                         category <- "strong tested potency"
                       }
                       if(inactive) {
                         category <- "inactive"
                       }
                       if(pointStats$dose.count < 2) {
                         category <- "insufficient data"
                       }
                       category
                     },{
                       warnUser(paste0("Categorization rules not implemented for ", renderingHint))
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
    means <- points[ userFlagStatus!="knocked out" & preprocessFlagStatus!="knocked out" & algorithmFlagStatus!="knocked out" & tempFlagStatus!="knocked out", list("dose" = dose, "mean.response" = mean(response)), by = dose]
    numDoses <- nrow(means)
    #inverseAgonistMode = FALSE = inverse agonists are inactive
    if(!inverseAgonistMode) {
      dosesAboveThreshold <- length(which(means$mean.response >= threshold))
      inverseAgonist <- coefficients(lm(dose ~ mean.response, means))[[2]] < 0
    } else {
      dosesAboveThreshold <- length(which(abs(means$mean.response) >= threshold))
    }
    potent <- dosesAboveThreshold == numDoses
    inactive <- (dosesAboveThreshold < rule$activeDoses) || ifelse(inverseAgonistMode, FALSE, inverseAgonist && !potent)
    
    insufficientRange <- abs(pointStats$response.empiricalMax - pointStats$response.empiricalMin) < threshold
  } else {
    inactive <- FALSE
    insufficientRange <- FALSE
    potent <- FALSE
  }
  return(list(inactive = inactive, insufficientRange = insufficientRange, potent = potent))  
}

get_drc_model <- function(dataSet, drcFunction = LL.4, subs = NA, paramNames = eval(formals(drcFunction)$names), fixed, robust = "mean") {
  opt <- options()
  on.exit(options(opt)) 
  fixedParams <- data.frame(matrix(NA,1,length(paramNames)))
  names(fixedParams) <- paramNames
  fixed[unlist(lapply(fixed, is.null))] <- NULL
  matches <- match(paramNames, names(fixed), nomatch= FALSE)
  fixedParams[which(matches != 0)] <- fixed[matches]
  fixed <- unlist(fixedParams)
  fct <- drcFunction(fixed=fixed, names=paramNames)
  if(!"weight" %in% names(dataSet)) {
    dataSet$weight <- 1
  }
  drcObj <- NULL
  tryCatch({
    options(show.error.messages=FALSE)
    drcObj <- drm(formula = response ~ dose, data = dataSet, weights = dataSet$weight, subset = userFlagStatus!="knocked out" & preprocessFlagStatus!="knocked out" & algorithmFlagStatus!="knocked out" & tempFlagStatus!="knocked out", robust=robust, fct = fct, control = drmc(errorm=TRUE))
  }, error = function(ex) {
    #Turned of printing of error message because shiny was printing to the browser because of a bug
    #print(ex$message)    
  })
  return(drcObj)
}

get_point_stats <- function(pts) {
  pts <- copy(pts)
  pts[ , knockedOut := userFlagStatus=="knocked out" & preprocessFlagStatus=="knocked out" & algorithmFlagStatus=="knocked out" & tempFlagStatus=="knocked out"]
  pts[, meanByDose := as.numeric(NA)]
  pts[ knockedOut == FALSE, meanByDose := mean(response), by = dose ]
  dose.count <- nrow(pts[ knockedOut == FALSE, .N, by = dose])
  response.empiricalMax <- pts[ knockedOut == FALSE, max(meanByDose)]
  response.empiricalMin <- pts[ knockedOut == FALSE, min(meanByDose)]
  dose.min <- min(pts[knockedOut == FALSE, ]$dose)
  dose.max <- max(pts[knockedOut == FALSE, ]$dose)
  dose.empiricalMaxResponse <- pts[(knockedOut == FALSE) &  meanByDose == response.empiricalMax, min(dose)]
  dose.empiricalMinResponse <- pts[(knockedOut == FALSE) &  meanByDose == response.empiricalMin, min(dose)]
  doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax <- pts[(knockedOut == FALSE) & dose > dose.empiricalMaxResponse & meanByDose < response.empiricalMax, unique(dose)]
  count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax <- length(doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax)
  doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin <- pts[(knockedOut == FALSE) & dose < dose.empiricalMinResponse & meanByDose > response.empiricalMin, unique(dose)]
  count.doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin <- length(doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin)
  return(list(dose.count = dose.count,
              response.empiricalMax = response.empiricalMax, 
              response.empiricalMin = response.empiricalMin, 
              dose.min = dose.min, 
              dose.max = dose.max, 
              doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax = doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax, 
              doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin = doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin,
              count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax = count.doses.withDoseAbove.doseEmpiricalMax.andResponseBelow.responseEmpiricalMax,
              count.doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin = count.doses.withDoseBelow.doseEmpiricalMin.andResponseAbove.responseEmpiricalMin))
}

ki_fct.5 <- function(fixed = c(NA, NA, NA, NA, NA), names = c("b", "c", "d", "e", "f")) {
  numParm <- 5
  if (!is.character(names) | !(length(names) == numParm)) {
    stop("Not correct names argument")
  }
  if (!(length(fixed) == numParm)) {
    stop("Not correct length of 'fixed' argument")
  }
  return(ki_fct(fixed = fixed, names = names)) 
}
ki_fct <- function(fixed = c(NA, NA, NA, NA, NA), names = c("b", "c", "d", "e", "f"), method = c("1"), ssfct = NULL, fctName, fctText)  {
  method <- "1"
  numParm <- 5
  if (!is.character(names) | !(length(names) == numParm)) {
    stop("Not correct 'names' argument")
  }
  if (!(length(fixed) == numParm)) {
    stop("Not correct 'fixed' argument")
  }
  notFixed <- is.na(fixed)
  parmVec <- rep(0, numParm)
  parmVec[!notFixed] <- fixed[!notFixed]
  fct <- function(dose, parm) 
    {
    parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
    parmMat[, notFixed] <- parm
    cParm <- parmMat[, 2]
    #Bottom + (Top-Bottom)/(1+10^(X-log(10^logKi*(1+HotNM/HotKdNM))))
    #Max + (Min - Max)/(1+10^(X-log(10^logKi*(1+ligandConc/Kd))))
    #c("min", "max", "ki", "ligandConc", "kd")
    cParm + (parmMat[,1]-cParm)/(1+10^(log10(dose)-log10(parmMat[,3]*(1+parmMat[,4]/parmMat[,5]))))    
  }
  retFct <- function(doseScaling, respScaling) {
    fct <- function(dose, parm) {
      parmMat <- matrix(parmVec/c(respScaling, respScaling, doseScaling), nrow(parm), numParm, byrow = TRUE)
      parmMat[, notFixed] <- parm
      cParm <- parmMat[, 2]
      cParm + (parmMat[,1] - cParm)/(1+10^(log10(dose)-log10(parmMat[,3]*(1+parmMat[,4]/parmMat[,5]))))      
    }
    fct
  }
  scaleFct <- function(doseScaling, respScaling) {        
    c(respScaling, respScaling, doseScaling, 1)[notFixed]
  }
  if (!is.null(ssfct)) {
    ssfct <- ssfct
  } else {
    ssfct <- ki.ssf(fixed)
  }
  names <- names[notFixed]
  
  lowerAs <- drc:::pickParm(parmVec, notFixed, 2)
  upperAs <- drc:::pickParm(parmVec, notFixed, 3)
  monoton <- drc:::monoParm(parmVec, notFixed, 1, -1)
  returnList <- list(fct = fct, 
                     ssfct = ssfct, 
                     names = names, 
#                      scaleFct = scaleFct, 
                     name = ifelse(missing(fctName),as.character(match.call()[[1]]), fctName), 
                     text = ifelse(missing(fctText), "Ki Fct (Ki as parameter)", fctText), 
                     noParm = sum(is.na(fixed)), 
                     lowerAs = lowerAs, 
                     upperAs = upperAs, 
                     monoton = monoton, 
#                   retFct = retFct, 
                     fixed = fixed)
  class(returnList) <- "kifit"
  return(returnList)
}
findbe1 <- function(doseTr, respTr, sgnb = 1, back = exp) {
  function(x, y, cVal, Val) {
    lmFit <- lm(respTr(y, cVal, dVal) ~ doseTr(x))
    coefVec <- coef(lmFit)
    bVal <- sgnb * coefVec[2]        
    eVal <- back(-coefVec[1] / (sgnb * bVal))
    return(as.vector(c(eVal)))
  }
}  
ki.ssf <- function(fixed, useFixed = FALSE) {
  ## Defining helper functions (used below)
  ytrans <- function(y, bVal, cVal) {log((cVal - y)/(y - bVal))}
  xfct <- function(x, y, bVal, cVal, dVal) {ytrans(y, bVal, cVal) / log(x / dVal)}
  #    efct <- function(x, y, bVal, cVal, dVal) {x * (((dVal - y) / (y - cVal))^(-1 / bVal))}
  dfct <- function(x, y, xVal, bVal, cVal) {x * exp(-ytrans(y, bVal, cVal)/xVal)}
  ## Assigning function for finding initial b and e parameter values    
  findbe1 <- function(doseTr, respTr, sgnb = 1, back = exp) {
    function(x, y, bVal, cVal) {
      lmFit <- lm(respTr(y, bVal, cVal) ~ doseTr(x))
      coefVec <- coef(lmFit)
      bVal <- sgnb * coefVec[2]      
      eVal <- back(-coefVec[1] / (sgnb * bVal))
      return(as.vector(c(eVal)))
    }
  }  
  finde <- findbe1(function(x) {rVec <- log(x); rVec[!x>0 | !is.finite(x)] <- NA; rVec}, ytrans)
  function(dframe) {
    ncoldf <- ncol(dframe)
    x <- dframe[, 1]        
    y <- dframe[, ncoldf]
    ## Finding initial values for c and d parameters
    findbc <- function (x, y, scaleInc = 0.001) {
      yRange <- range(y)
      lenyRange <- scaleInc * diff(yRange)
      c(yRange[1] - lenyRange, yRange[2] + lenyRange)
    }
    bcVal <- findbc(x, y)
    bVal <- bcVal[1]
    cVal <- bcVal[2]
    ## Finding initial values for b and e parameters    
    eVal <- finde(x, y, bcVal[1], bcVal[2])         
    return(c(bcVal, eVal[1])[is.na(fixed)])
  }
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
  SST <- sum((points$response-mean(points[userFlagStatus!="knocked out" & preprocessFlagStatus!="knocked out" & algorithmFlagStatus!="knocked out" & tempFlagStatus!="knocked out",]$response))^2)
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
fit_data_to_acas_experiment_response <- function(fitData, experimentCode, transactionId = -1, status, hasWarning, errorMessages = as.character(), ...) {
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
                                "small.ll4" = system.file("tests", "data", "doseResponse", "docs", "Example-Dose-Response-SEL-LL4.xlsx", package="racas"),
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
  #   file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas")
  #   simpleSettingsJSON <- readChar(file, file.info(file)$size)
  #   simpleSettings <- fromJSON(simpleSettingsJSON)
  #   api_doseResponse.experiment(simpleSettings, recordedBy="bbolt", experimentCode=experimentCode)
  return(experimentCode)
}
save_dose_response_data <- function(fitData, recorded_by) {
  lsTransaction <- createLsTransaction()$id
  fitData[ , dto := list(list({
    ans <- switch(renderingHint,
                  "4 parameter D-R" = {
                    list(
                      "renderingHint" =  renderingHint[[1]],
                      "max" = reportedParameters[[1]]$max$value,
                      "maxOperatorKind" = reportedParameters[[1]]$max$operator,
                      "maxUncertainty" = reportedParameters[[1]]$max$stdErr,
                      "maxUncertaintyType" = na_to_null(ifelse(is.null(reportedParameters[[1]]$max$stdErr), NA,"standard error")),
                      "min" = reportedParameters[[1]]$min$value,
                      "minOperatorKind" = reportedParameters[[1]]$min$operator,
                      "minUncertainty" = reportedParameters[[1]]$min$stdErr,
                      "minUncertaintyType" = na_to_null(ifelse(is.null(reportedParameters[[1]]$min$stdErr), NA,"standard error")),
                      "ec50" = reportedParameters[[1]]$ec50$value,
                      "ec50OperatorKind" = reportedParameters[[1]]$ec50$operator,
                      "ec50Uncertainty" = reportedParameters[[1]]$ec50$stdErr,
                      "ec50UncertaintyType" = na_to_null(ifelse(is.null(reportedParameters[[1]]$ec50$stdErr), NA,"standard error")),
                      "slope" = reportedParameters[[1]]$slope$value,
                      "slopeOperatorKind" = reportedParameters[[1]]$slope$operator,
                      "slopeUncertainty" = reportedParameters[[1]]$slope$stdErr,
                      "slopeUncertaintyType" = na_to_null(ifelse(is.null(reportedParameters[[1]]$slope$stdErr), NA,"standard error")),
                      "recordedBy" = recorded_by,
                      "curveId" = paste0(analysisGroupCode,"_", lsTransaction),
                      "analysisGroupCode" = analysisGroupCode[[1]],
                      "batchCode" = batchCode[[1]],
                      "category" = category[[1]],
                      "minUnits" = points[[1]]$responseUnits[[1]],
                      "maxUnits" = points[[1]]$responseUnits[[1]],
                      "ec50Units" = points[[1]]$responseUnits[[1]],
                      "fittedMin" = fittedParameters[[1]]$min,
                      "fittedMax" = fittedParameters[[1]]$max,
                      "fittedEC50" = fittedParameters[[1]]$ec50,
                      "fittedSlope" = fittedParameters[[1]]$slope,
                      "sse" = goodnessOfFit.model[[1]]$SSE,
                      "sst" = goodnessOfFit.model[[1]]$SST,
                      "rsquared" = goodnessOfFit.model[[1]]$rSquared,
                      "curveErrorsClob" = curveErrorsClob[[1]],
                      "reportedValuesClob" = reportedValuesClob[[1]],
                      "parameterStdErrorsClob" = parameterStdErrorsClob[[1]],
                      "fitSettings" = simpleFitSettings[[1]],
                      "fitSummaryClob" = fitSummaryClob[[1]],
                      "userFlagStatus" = userFlagStatus[[1]],
                      "algorithmFlagStatus" = algorithmFlagStatus[[1]]
                    )
                  },
                  "Ki Fit" = {
                    list(
                      "renderingHint" =  renderingHint[[1]],
                      "max" = reportedParameters[[1]]$max$value,
                      "maxOperatorKind" = reportedParameters[[1]]$max$operator,
                      "maxUncertainty" = reportedParameters[[1]]$max$stdErr,
                      "maxUncertaintyType" = na_to_null(ifelse(is.null(reportedParameters[[1]]$max$stdErr), NA,"standard error")),
                      "min" = reportedParameters[[1]]$min$value,
                      "minOperatorKind" = reportedParameters[[1]]$min$operator,
                      "minUncertainty" = reportedParameters[[1]]$min$stdErr,
                      "minUncertaintyType" = na_to_null(ifelse(is.null(reportedParameters[[1]]$min$stdErr), NA,"standard error")),
                      "ki" = reportedParameters[[1]]$ki$value,
                      "kiOperatorKind" = reportedParameters[[1]]$ec50$operator,
                      "kiUncertainty" = reportedParameters[[1]]$ec50$stdErr,
                      "kiUncertaintyType" = na_to_null(ifelse(is.null(reportedParameters[[1]]$ec50$stdErr), NA,"standard error")),
                      "recordedBy" = recorded_by,
                      "curveId" = paste0(analysisGroupCode,"_", lsTransaction),
                      "analysisGroupCode" = analysisGroupCode[[1]],
                      "batchCode" = batchCode[[1]],
                      "category" = category[[1]],
                      "minUnits" = points[[1]]$responseUnits[[1]],
                      "maxUnits" = points[[1]]$responseUnits[[1]],
                      "kiUnits" = points[[1]]$responseUnits[[1]],
                      "fittedMin" = fittedParameters[[1]]$min,
                      "fittedMax" = fittedParameters[[1]]$max,
                      "fittedKi" = fittedParameters[[1]]$ki,
                      "ligandConc" = fixedParameters[[1]]$ligandConc,
                      "ligandConcUnits" = 'nM',
                      "kd" = fixedParameters[[1]]$kd[[1]],
                      "kdUnits" = 'nM',
                      "sse" = goodnessOfFit.model[[1]]$SSE,
                      "sst" = goodnessOfFit.model[[1]]$SST,
                      "rsquared" = goodnessOfFit.model[[1]]$rSquared,
                      "curveErrorsClob" = curveErrorsClob[[1]],
                      "reportedValuesClob" = reportedValuesClob[[1]],
                      "parameterStdErrorsClob" = parameterStdErrorsClob[[1]],
                      "fitSettings" = simpleFitSettings[[1]],
                      "fitSummaryClob" = fitSummaryClob[[1]],
                      "userFlagStatus" = userFlagStatus[[1]],
                      "algorithmFlagStatus" = algorithmFlagStatus[[1]]
                    )
                  },{
                    warning(paste0("saving not implemented for ",renderingHint))
                  }
    )
    ans
  })), by = curveId]
  fitDataSaveRequest <- toJSON(fitData$dto)
  curveids <- unlist(lapply(fitData$dto,function(x) x$curveId))
  url <- switch(fitData$renderingHint[[1]],
                "4 parameter D-R" = paste0(racas::applicationSettings$client.service.persistence.fullpath, "curvefit"),
                "Ki Fit" = paste0(racas::applicationSettings$client.service.persistence.fullpath, "curvefit/ki")
                )
  response <- getURL(
    url,
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=fitDataSaveRequest)
  if(response != "") {
    stop(response)
  }
  changedPoints <- rbindlist(fitData$points)[flagchanged == TRUE,]
  if(nrow(changedPoints) > 0)
  {
    changedPoints[ , dto := list(list({
      list(      
        "userFlagStatus"= userFlagStatus,
        "algorithmFlagStatus" = algorithmFlagStatus,
        "responseSubjectValueId" = responseSubjectValueId,
        "algorithmFlagObservation" = algorithmFlagObservation,
        "algorithmFlagReason" = algorithmFlagReason,
        "algorithmFlagComment" = algorithmFlagComment,
        "preprocessFlagStatus" = preprocessFlagStatus,
        "preprocessFlagObservation" = preprocessFlagObservation,
        "preprocessFlagReason" = preprocessFlagReason,
        "preprocessFlagComment" = preprocessFlagComment,
        "userFlagObservation" = userFlagObservation,
        "userFlagReason" = userFlagReason,
        "userFlagComment" = userFlagComment,
        "recordedBy" = recorded_by
      )
    })), by = responseSubjectValueId]
    dtos <- toJSON(changedPoints$dto)
    response <- getURL(
      paste0(racas::applicationSettings$client.service.persistence.fullpath, "curvefit/flagWells"),
      customrequest='POST',
      httpheader=c('Content-Type'='application/json'),
      postfields=dtos)
    if(response != "") {
      stop(response)
    }
  }
  return(curveids)
}
get_ls_type <- function(valueType) {
  valueTypesList <- fromJSON(getURL(paste0(racas::applicationSettings$client.service.persistence.fullpath, "valuetypes")))
  index <- which(rbindlist(valueTypesList)$typeName==valueType)
  if(length(index) != 0) {
    ls_type <- valueTypesList[[which(rbindlist(valueTypesList)$typeName==valueType)]]
  } else {
    ls_type <- NULL
  }
  return(ls_type)
}
create_ls_kind <- function(lsType, kindName) {
  typeKindList <- list(kindName = kindName, lsType = lsType)
  response <- getURL(
    paste0(racas::applicationSettings$client.service.persistence.fullpath, "valuekinds/"),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(typeKindList))
}
update_or_replace_experiment_metadata_value <- function(experimentCode, experimentID, lsType, lsKind, value) {   
  if(length(checkValueKinds(lsKind, lsType)$goodValueKinds)==0) {
    type <- get_ls_type(lsType)
    create_ls_kind(lsType = type, kindName = lsKind)
  }
  if(missing(experimentCode)) experimentCode <- experimentID
  url <- URLencode(paste0(racas::applicationSettings$client.service.persistence.fullpath,"values/experiment/", experimentCode,"/bystate/metadata/experiment metadata/byvalue/",lsType,"/",lsKind,"/"))
  response <- getURL(
    url,
    customrequest='PUT',
    httpheader=c('Content-Type'='application/json'),
    postfields=value
  )
  return(response)
}


update_experiment_model_fit_status <- function(experimentCodeOrID, status) {
  response <- update_or_replace_experiment_metadata_value(experimentCodeOrID, lsType = "codeValue", lsKind = "model fit status", value = status)
  return(response)
}
update_experiment_model_fit_html <- function(experimentCodeOrID, html) {
  response <- update_or_replace_experiment_metadata_value(experimentCodeOrID, lsType = "clobValue", lsKind = "model fit result html", value = html)
  return(response)
}

save_fit_data <- function(fitData, recordedBy, lsTransaction) {
  ignoredAnalysisGroupStates <- lapply(fitData$stateId, function(x) {
    response <- getURL(
      paste0(racas::applicationSettings$client.service.persistence.fullpath,"/analysisgroupstates/ignoreStateAndValues/jsonArray"),
      customrequest='PUT',
      httpheader=c('Content-Type'='application/json'),
      postfields=toJSON(list(as.list(list(id = x)))))
  })
  fitData[ , newStates := list(list(createAnalysisGroupState(
    analysisGroup = list(id =  analysisGroupId[[1]], version = 0),
    analysisGroupValues = analysisGroupValues[[1]]$analysisGroupValues,
    recordedBy = recordedBy,
    lsType = "data",
    lsKind = "dose response",
    lsTransaction = lsTransaction    
  ))), by = curveid]
  savedAnalysisGroupStates <- saveAcasEntities(fitData$newStates, "analysisgroupstates")
  return(fitData$newStates)
}

doseResponse_update_user_flag <- function(fitData, userFlagStatus, recordedBy) {
  savedUserFlag <- fitData[1]$ag_values[[1]][lsKind == "flag" & stringValue == "user" & ignored == FALSE]
  curated <- nrow(savedUserFlag) > 0
  #If the the curve has not been curated
  #If the flag_user approved
  #add approved flag
  #If the flag_user rejected
  #add rejected flag & ignore reportedParameters ...etc.
  #If the curve has been curated
  #If the current flag is approved
  #If the flag_user is approved
  #do nothing
  #If the flag_user is rejected
  #ignore the old flag & add the new flag & ignore reportedParameters ...etc.
  #If the current flag is rejected
  #If the flag_user is rejected
  #do nothing
  #If the flag_user is approved
  #ignore the old flag & add the new flag & ignore reportedParameters ...etc.
  reject_curve_ag_values <- function() {
    ids_to_reject <- fitData[1]$ag_values[[1]][ignored == FALSE & (publicData == TRUE) & !lsKind %in% c("batch code","curve id") ]$id
    values_to_reject <- lapply(ids_to_reject, get_entity_by_id, "analysisgroupvalues")
    rejected_analysis_group_values <- lapply(values_to_reject, function(x) {
      x$lsType <- "stringValue"
      x$stringValue <- flagUser
      updateAcasEntity(x, "analysisgroupvalues")
    })
    reported_parameters_clob_id <- fitData[1]$ag_values[[1]][lsKind == "reportedValuesClob"]$id
    reported_parameters_valu <- lapply(reported_parameters_clob_id, get_entity_by_id, "analysisgroupvalues")
    
    
    return(rejected_analysis_group_values)
  }
  approve_curve_ag_values <- function() {
    idsToIgnore <- fitData[1]$ag_values[[1]][ignored == FALSE & (publicData == TRUE | lsKind %in% c("reportedValuesClob")) & !lsKind %in% c("batch code","curve id") ]$id
    valuesToIgnore <- lapply(idsToIgnore, get_entity_by_id, "analysisgroupvalues")
    ignoredAnalysisGroupValues <- lapply(valuesToIgnore, function(x) {
      x$ignored <- FALSE
      updateAcasEntity(x, "analysisgroupvalues")
    })
    return(ignoredAnalysisGroupValues)
  }
  
  ignore_flag <- function() {
    idsToIgnore <- fitData[1]$ag_values[[1]][lsKind == "flag" & stringValue == "user"]$id
    valuesToIgnore <- lapply(idsToIgnore, get_entity_by_id, "analysisgroupvalues")
    ignoredAnalysisGroupValues <- lapply(valuesToIgnore, function(x) {
      x$ignored <- TRUE
      updateAcasEntity(x, "analysisgroupvalues")
    })
    return(ignored_ag_values)
  }
  add_flag <- function() {
    lsTransactionID <- createLsTransaction()$id
    flag_user_state_value <- createStateValue(lsState=get_entity_by_id(fitData[1]$stateId,acasCategory="analysisgroupstates"),
                                              lsType = 'comments',
                                              lsKind = 'flag',
                                              stringValue = 'user',
                                              comments = flagUser,
                                              publicData = FALSE,
                                              lsTransaction=lsTransactionID,
                                              recordedBy = as.character(recordedBy))
    flag_user_state_value <- saveAcasEntities(list(flag_user_state_value), "analysisgroupvalues")
    return(TRUE)
  }
  
  if(!curated) {
    add_flag()
    if(flagUser == "rejected") {
      reject_curve_ag_values()
    }
  } else {
    current_flag <- savedUserFlag$comments[[1]]
    switch(current_flag,
           "approved" = {
             switch(flagUser,
                    "approved" = {
                      return(TRUE)
                    },
                    "rejected" = {
                      add_flag()
                      reject_curve_ag_values()
                    })
           },
           "rejected" = {
             switch(flagUser,
                    "approved" = {
                      ignore_flag()
                      add_flag()
                      approve_curve_ag_values()
                    },
                    "rejected" = {
                      return(TRUE)
                    })
           })
  }
}

get_ls_state_from_entity <- function(entities, ...) {
  unlistEntities <- unlist(entities, recursive = FALSE)
  lsStatesList <- unlistEntities[names(unlistEntities) == "lsStates"]
  lsStates <- do.call("c", lsStatesList)
  match_list_criteria <- function(lsState, listCriteria) {
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
  pointData <- Reduce(function(x,y) rbind(x,y,fill = TRUE), points)
  pointData <- pointData[flagchanged == TRUE, ]
  addTheseFlags <- pointData[ !is.na(flag_user) | !is.na(flag_on.load) | !is.na(flag_algorithm)]
  ignoreTheseFlags <- pointData[!is.na(flag_sv_id), list(flag_sv_id, response_ss_id, response_ss_version, treatmentGroupId, flag_user, flag_on.load, flag_algorithm)]
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
  #Treatment group value updates
  update_tg_id <- unique(pointData$treatmentGroupId)
  updateTheseValues <- pointData[pointData$treatmentGroupId %in% update_tg_id]
  treatmentGroups <- lapply(update_tg_id, get_entity_by_id, "treatmentgroups")
  treatmentGroupDF <- ldply(treatmentGroups, flattenEntity, acasCategory= "treatmentGroup", includeFromState = c("id", "lsType", "lsKind", "version"))
  valuesToIgnoreDF <- treatmentGroupDF[treatmentGroupDF$lsKind == "Response", ]
  valuesToIgnore <- lapply(valuesToIgnoreDF$id, get_entity_by_id, "treatmentgroupvalues")
  ignoredValues <- lapply(valuesToIgnore, function(x) {
    x$ignored <- T
    updateAcasEntity(x, "treatmentgroupvalues")
  })
  updateTheseValues$tgs_id <- treatmentGroupDF$stateId[match(updateTheseValues$treatmentGroupId, treatmentGroupDF$treatmentGroupId)]
  updateTheseValues$tgs_version <- treatmentGroupDF$stateVersion[match(updateTheseValues$treatmentGroupId, treatmentGroupDF$treatmentGroupId)]
  updateTheseValues$tgv_id <- treatmentGroupDF$id[match(updateTheseValues$treatmentGroupId, valuesToIgnoreDF$TreatmentGroupId)]
  if(nrow(updateTheseValues) > 0) {
    newValues <- updateTheseValues[, list(list(createStateValue(lsType = "numericValue",
                                                                lsKind = "Response", 
                                                                numericValue = na_to_null(suppressWarnings(mean(response))), 
                                                                numberOfReplicates=length(response), 
                                                                uncertaintyType="standard deviation", 
                                                                uncertainty = na_to_null(sd(response)), 
                                                                lsTransaction=lsTransaction,
                                                                recordedBy=recordedBy, 
                                                                lsState=list(id=unique(tgs_id), 
                                                                             version=unique(tgs_version))))), 
                                   by = treatmentGroupId]$V1
    saveAcasEntities(newValues, "treatmentgroupvalues")
  }
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

add_clob_values_to_fit_data <- function(fitData) {
  fitData <- copy(fitData)
  addingColumns <- c("reportedValuesClob", "fitSummaryClob", "parameterStdErrorsClob", "curveErrorsClob")
  removeColumns <- addingColumns[ addingColumns %in% names(fitData)]
  if(length(removeColumns) > 0) fitData[ , removeColumns := NULL, with = FALSE]
  fitData[ , c("reportedValuesClob", "fitSummaryClob", "parameterStdErrorsClob", "curveErrorsClob") := {
    if(model.synced) {
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
      if(fitConverged) {
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
        list(reportedValuesClob = list(reportedValuesClob), fitSummaryClob = list(NULL), parameterStdErrorsClob= list(NULL), curveErrorsClob = list(NULL))
      }
    } else {
      fitData[1]$parameters[[]]
    }
  }, by = curveId]
  
  return(fitData)
}

LL4 <- 'min + (max - min)/(1 + exp(slope * (log(x/ec50))))'
OneSiteKi <- 'max + (min-max)/(1+10^(log10(x)-log10(ki*(1+ligandConc/kd))))'



MM2 <- '(max*x)/(kd + x)'





