
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
  fitData[ , fixedParameters := list(list(myFixedParameters))]
  fitData[ , parameterRules := list(list(myParameterRules))]
  fitData[ , inactiveRule := list(list(myInactiveRule))]
  
  #Update all of the flags to those that are in the fitSettings json
  updateFlags <- as.data.table(fitSettings$updateFlags)
  if(nrow(updateFlags) > 0 ) {
    updateFlags[flag=="NA", flag := as.character(NA)]
    setnames(updateFlags, "id", "response_sv_id")
    setkey(updateFlags,"response_sv_id" )
    #First update the flags in points
    updFlags <- function(pts) {
      returnCols <- names(pts)
      setkey(pts, "response_sv_id")
      pts <- merge(pts,updateFlags, all.x = TRUE, by = "response_sv_id", suffixes = c("",".y"))
      pts[, flag := as.character(flag)]
      pts[ ,flagChanged := !identical(flag,flag.y) | flagChanged, by = "response_sv_id" ]
      pts[flagChanged==TRUE ,flag := flag.y]
      return(pts[, returnCols, with = FALSE])
    }
    #pts <- fitData[1]$points[[1]]
    fitData[, points := list(list(updFlags(points[[1]]))) , by = curveid]
  }
  
  #Fit the data
  fitData <- doseResponseFit(fitData)
  
  #While refit is true, keep refitting using fixed parameters
  refit <- checkRefit(fitData)
  iterations <- 20
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
  
  #Categorize the fit data
  fitData[ , category := categorizeFitData(results.parameterRules, inactive, fitConverged, insufficientRange), by = curveid]
  #Extract the reported Parameters
  fitData[ , reportedParameters := list(list(getReportedParameters(modelHint, results.parameterRules[[1]], inactive, fitConverged, insufficientRange, fixedParameters[[1]], fittedParameters[[1]], pointStats[[1]], goodnessOfFit.parameters[[1]], goodnessOfFit.model[[1]]))), by = curveid]
  
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
simpleToAdvancedFitSettings <- function(simpleSettings, modelHint = "LL.4") {
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
  modifiedSettings <- switch(modelHint,
                             "LL.4" = updateFitSettings.LL4(modifiedSettings,simpleSettings),
                             warning(paste0("Simple to Advanced fit settings not implemented for modelHint: ",modelHint))
  )
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
#Check if limits have been exceeded and refit if not inactive, non-converged or insufficient range
checkRefit <- function(fitData) {
  refit <- fitData[ , switch(modelHint,
                             "LL.4" = {  maxExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "maxThreshold" %in% results.parameterRules[[1]]$limits)
                                         minExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "minThreshold" %in% results.parameterRules[[1]]$limits)
                                         slopeExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "slopeThreshold" %in% results.parameterRules[[1]]$limits)
                                         exceededAThreshold <- (maxExceeded | minExceeded | slopeExceeded)
                                         refit <-  exceededAThreshold & (!inactive | !fitConverged | !insufficientRange)
                                         refit
                             },{
                               warning(paste0("Refit rule not implemented for ", modelHint))
                               FALSE
                             }),
                   by = curveid]$V1
  return(refit)
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
#' @return A json object with the sessionid among a number of html divs to display back to the user
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
#' #Inspect the saved fit session
#' parsedResponse <- fromJSON(response)
#' session <- parsedResponse$sessionID
#' loadSession(session)
#' head(fitData)
doseResponse <- function(fitSettings, curveids = NA, sessionID = NA, fitData = NA) {
  if(all(is.na(c(curveids, sessionID, fitData)))) stop("Must provide curveids or sessionID or fitData, all are NA")
  if(class(curveids) == "character") {
    fitData <- getFitData.curve(curveids)
  }
  if(!is.na(sessionID)) {
    fitSettings_new <- fitSettings
    sessionID_new <- sessionID
    loadSession(sessionID)
    fitSettings <- fitSettings_new
    sessionID <- sessionID_new
    rm(fitSettings_new,sessionID_new)
    if(exists("fitData")) {
      fitData[, model.synced := FALSE]      
    } else {
      stop("fitData object does not exist in session, are you sure this was a fit session?")
    }
  }
  if(any(class(fitData) == "data.table")) {
    fitData <- doseResponse.fitData(fitSettings, fitData) 
  } else {
    stop("fitData not a data.table")
  }
  if(is.na(sessionID)) {
    sessionID <- saveSession()
  } else {
    sessionID <- saveSession(sessionID)
  }
  response <- fitDataToResponse.curation(fitData, sessionID = sessionID)
  return(response)
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
#' fitDataToResponse.curation(fitData, cars)
fitDataToResponse.curation <- function(fitData, ...) {
  listToDataTable <- function(l) {
    dt <- Reduce(function(x,y) rbind(x,y,fill = TRUE), lapply(1:length(l), function(x) {dt <- as.data.table(l[[x]])
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
  reportedParameters <- listToDataTable(fitData[1]$reportedParameters[[1]])
  reportedValues <- objToHTMLTableString(reportedParameters[ , c("name", "value"), with = FALSE], include.colnames = FALSE)
  fitSummary <- captureOutput(summary(fitData[1]$model[[1]]))
  goodnessOfFit.parameters <- listToDataTable(fitData[1]$goodnessOfFit.parameters[[1]])
  goodnessOfFit.parameters[ , c("name", "type") := {sp <- strsplit(name, "\\.")[[1]]
                                                    list(name = sp[[1]], type = sp[[2]])
  }, by = c("V1", "name")]
  goodnessOfFit.parameters <- dcast.data.table(goodnessOfFit.parameters, name ~ type, value.var = "V1")
  parameterStdErrors <- objToHTMLTableString(goodnessOfFit.parameters)
  curveErrors <- objToHTMLTableString(listToDataTable(fitData[1]$goodnessOfFit.model[[1]])[, c("name", "V1"), with = FALSE])
  category <- fitData[1]$category[[1]]
  algorithmApproved = fitData[1]$approved[[1]]
  points <- fitData[1]$points[[1]][ , c("response_sv_id", "dose", "doseUnits", "response", "responseUnits", "flag"), with = FALSE]
  points <- split(points, points$response_sv_id)
  names(points) <- NULL
  plotData <- list(plotWindow = plotWindow(fitData[1]$points[[1]]),
                   points  = points,
                   curve = c(type = fitData[1]$modelHint,
                                fitData[1]$fittedParameters[[1]])
  )
  curveAttributes <- list(EC50 = fitData[1]$reportedParameters[[1]]$ec50$value,
                          Operator = fitData[1]$reportedParameters[[1]]$ec50$operator,
                          SST = fitData[1]$goodnessOfFit.model[[1]]$SST,
                          SSE =  fitData[1]$goodnessOfFit.model[[1]]$SSE,
                          rSquared =  fitData[1]$goodnessOfFit.model[[1]]$rSquared,
                          compoundCode = fitData[1]$tested_lot
  )
  return(toJSON(list(reportedValues = reportedValues,
                     fitSummary = fitSummary,
                     parameterStdErrors = parameterStdErrors,
                     curveErrors = curveErrors,
                     category = category,
                     algorithmApproved = algorithmApproved,
                     curveAttributes = curveAttributes,
                     plotData = plotData,
                     ...
  )))   
}

predictPoints <- function(pts, drcObj) {
  if(nrow(pts)==0 || is.null(drcObj)) {
    return(NULL)
  }
  x <- unique(pts$dose)
  if(grepl("LOG",toupper(pts$doseUnits[1]))) {
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
captureOutput <- function(obj) {
  return(paste(capture.output({
    result <- withVisible(obj)
    if (result$visible)
      print(result$value)
  }), collapse=""))
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
                           print.results = FALSE)
  return(htmlTableString)
}

getReportedParameters <- function(modelHint, results, inactive, fitConverged, insufficientRange, fixedParameters, fittedParameters, pointStats, goodnessOfFit.parameters, goodnessOfFit.model) {
  switch(modelHint,
         "LL.4" = {
           if(inactive | insufficientRange) {
             max <- list(value = pointStats$response.empiricalMax, operator = NULL, stdErr = NULL)
             min <- list(value = pointStats$empiricalMin, operator = NULL, stdErr = NULL)
             ec50 <- list(value = pointStats$dose.max, operator = ">", stdErr = NULL)
             reportedValues <- list(Min = min, max = max, ec50 = ec50)
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
             min <- list(value = pointStats$empiricalMin, operator = NULL, stdErr = NULL)
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
           if("ec50Threshold" %in% results$limits) {
             ec50 <- list(value = pointStats$dose.max, operator = ">", stdErr = NULL)
           } else {
             ec50 <- list(value = fittedParameters$ec50, operator = NULL, stdErr = goodnessOfFit.parameters$ec50.stdErr)
           }
           reportedValues <- list(min = min,max = max, slope = slope, ec50 = ec50)
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

getFitData.curve <- function(curveids, ...) {
  fitData <- getCurveData(curveids, flagsAsLogical = FALSE, ...)
  fitData$points <- cbind(fitData$points, flagChanged = FALSE)
  fitData <- data.table(curveid = unique(as.character(fitData$points$curveid,fitData$parameters$curveid))[order(unique(as.character(fitData$points$curveid,fitData$parameters$curveid)))], 
                        modelHint = unlist(lapply(fitData$parameters$renderingHint, 
                                                  function(x) {
                                                    ans <- switch(x,
                                                                  "4 parameter D-R" = "LL.4",
                                                                  "2 parameter Michaelis Menten" = "MM.2")
                                                    return(ans)
                                                  })),
                        points = split(as.data.table(fitData$points), fitData$points$curveid),
                        parameters = split(as.data.table(fitData$parameters), fitData$parameters$curveid),
                        key = "curveid")
  fitData[ , tested_lot := as.character(rbindlist(fitData$parameters)$tested_lot)]
  myParameterRules <- list(goodnessOfFits = list(), limits = list())
  myInactiveRule <- list()
  myFixedParameters <- list()
  fitData[ , c("parameterRules", "inactiveRule", "fixedParameters") := list(list(myParameterRules),
                                                                            list(myInactiveRule),
                                                                            list(myFixedParameters))]
  fitData[ , model.synced := FALSE]
  return(fitData)
}
getFitData.experimentCode <- function(experimentCode, ...) {
  myMessenger <- messenger()
  myMessenger$logger$debug("Calling experiment service")
  experimentJSON <- getURL(paste0(racas::applicationSettings$client.service.persistence.fullpath, "experiments/codename/", experimentCode, "?with=fullobject"))
  
  myMessenger$logger$debug("Parsing experiment json")
  experiment <- jsonlite::fromJSON(experimentJSON[[1]])
  fitData <- as.data.table(experiment$analysisGroups[[1]][!experiment$analysisGroups[[1]]$ignored,])
  
  myMessenger$logger$debug("Extracting curve parameters")
  fitData[ , parameters := list(list(
    rbindlist(rbindlist(lsStates)[ignored == FALSE & lsKind=="Dose Response"][ , lsValues:= list(list(lsValues[[1]][ , order(names(lsValues[[1]]))])), by = id]$lsValues)
  )), by = id]
  
  myMessenger$logger$debug("Extracting curve points")
  fitData[ ,  points:= list(list({
    treatmentGroups <- rbindlist(treatmentGroups)[ignored == FALSE]
    subjects <<- treatmentGroups[ , rbindlist(subjects)[ , subj_id := id], by = id]
    subjectStates <- subjects[ , rbindlist(lsStates)[ , subj_id:= subj_id], by = subj_id]
    points <- subjectStates[ , {
      lsValues <- Reduce(function(x,y) rbind(x,y,fill = TRUE), lapply(lsValues, as.data.table))
      list(list(lsValues[ , subj_id:=subj_id]))
    }, by = subj_id]
    Reduce(function(x,y) rbind(x,y,fill = TRUE), points$V1)
  }
  )), by = id]
  myMessenger$logger$debug("Pivoting the curve points")
  fitData[ , points := list(list({ 
    dr <- data.table::dcast.data.table(points[[1]][lsKind %in% c("Dose", "Response")], subj_id ~ lsKind, value.var = "numericValue")[ , id:=points[[1]][lsKind=="Response"]$id]    
    drUnits <- dcast.data.table(points[[1]][lsKind %in% c("Dose", "Response")], subj_id ~ lsKind, value.var = "unitKind")
    setnames(drUnits, "Dose", "doseUnits")
    setnames(drUnits, "Response", "responseUnits")
    dr <- dr[drUnits]
    setnames(dr, "id", "response_sv_id")
    if(nrow(points[[1]][lsKind=="flag"]) > 0) {
      fl <- dcast.data.table(points[[1]][lsKind=="flag"], subj_id ~ lsKind, value.var = "stringValue")
    } else {
      fl <- data.table(subj_id = as.integer(),flag = as.character())
      setkey(fl, subj_id)
    }
    bc <-  dcast.data.table(points[[1]][lsKind=="batch code"], subj_id ~ lsKind, value.var = "codeValue")
    setkey(fl, subj_id)
    setkey(bc, subj_id)
    setkey(dr, subj_id)
    if(nrow(fl) == 0) {
      pts <- bc[dr]
    } else {
      pts <- fl[bc][dr]
    }
    if(is.null(pts$flag)) {
      pts[ , flag := as.character(NA)]
    }
    setnames(pts, names(pts), tolower(names(pts)))
  })), by = id]
  myMessenger$logger$debug("Filling out the rest of the fit data object")
  
  fitData[ , curveid := parameters[[1]][grepl('.*curve id', lsKind)]$stringValue , by = id]
  myParameterRules <- list(goodnessOfFits = list(), limits = list())
  myInactiveRule <- list()
  myFixedParameters <- list()
  fitData[ , c("parameterRules", "inactiveRule", "fixedParameters") := list(list(myParameterRules),
                                                                            list(myInactiveRule),
                                                                            list(myFixedParameters))]  
  fitData[ , modelHint := unlist(lapply(rbindlist(parameters)[lsKind == "Rendering Hint"]$stringValue, 
                                        function(x) {
                                          ans <- switch(x,
                                                        "4 parameter D-R" = "LL.4",
                                                        "2 parameter Michaelis Menten" = "MM.2")
                                          return(ans)
                                        }))
          ]  
  if(is.null(fitData$modelHint)) {
    myMessenger$addUserError(paste0("No Rendering Hint found for ", experimentCode))
    myMessenger$logger$error(paste0("Attempted to fit an expt code with no rendering hint stored in analysis group parameters"))
  }
  fitData[ , model.synced := FALSE]
  myMessenger$logger$debug(paste0("Returning from getting experiment curve data with ", nrow(fitData), " curves"))
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
  
  fitData[ model.synced == FALSE, c("inactive", "insufficientRange") := applyInactiveRule(pointStats[[1]],points[[1]], inactiveRule[[1]]), by = curveid]
  fitData[ model.synced == FALSE, "approved" := fitConverged | insufficientRange, by = curveid]
  returnCols <- unique(c(fitDataNames, "model", "fitConverged", "pointStats", "fittedParameters", "goodnessOfFit.model", "goodnessOfFit.parameters", "inactive", "insufficientRange"))
  
  fitData[ model.synced == FALSE, model.synced := TRUE]
  return(fitData[, returnCols, with = FALSE])
}

categorizeFitData <- function(results.parameterRules, inactive, converged, insufficientRange) {
  category <- "sigmoid"
  resultList <- unlist(results.parameterRules)
  #Parameter Failed Categories
  if(length(resultList) > 0 ) {
    reasons <- paste0("Failed ",resultList)
    parameterFails <- paste0(reasons,collapse = ", ")
    if(length(parameterFails) > 0 ) {
      category <- sort(parameterFails)
    }
  }
  if(!converged) {
    category <- "Convergence Failed"
  }
  if(insufficientRange) {
    category <- "Insufficient Range"
  }
  if(inactive) {
    category <- "Inactive"
  }
  return(category)
}
categorizeFitData <- function(results.parameterRules, inactive, converged, insufficientRange) {
  category <- "sigmoid"
  resultList <- unlist(results.parameterRules)
  #Parameter Failed Categories
  if(length(resultList) > 0 ) {
    reasons <- paste0("Failed ",resultList)
    parameterFails <- paste0(reasons,collapse = ", ")
    if(length(parameterFails) > 0 ) {
      category <- sort(parameterFails)
    }
  }
  if(!converged) {
    category <- "Convergence Failed"
  }
  if(insufficientRange) {
    category <- "Insufficient Range"
  }
  if(inactive) {
    category <- "Inactive"
  }
  return(category)
}

getDNETCategory <- function(results.parameterRules, inactive, converged, insufficientRange) {
  #   -weak tested potency
  #   strong tested potency
  #   -inactive
  #   Low Quality Fit - failed PValue Test
  #   hockey stick
  #   sigmoid 
  #   -lack of fit - fit did not converge
  category <- "sigmoid"
  resultList <- unlist(results.parameterRules)
  if("maxUncertaintyRule" %in% resultList | "ec50Threshold" %in% resultList) {
    category <- "weak tested potency"
  }
  if("minUncertaintyRule" %in% resultList) {
    category <- "strong tested potency"
  }
  if(!converged) {
    category <- "lack of fit - fit did not converge"
  }
  if(insufficientRange) {
    category <- "insufficient range"
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
                   "logAboveReference" = eval(parse( text = paste('fittedParameters[parameter]',operator,'10^(log10(pointStats[reference][[1]]) + value)'))),
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

applyInactiveRule <- function(pointStats, points, rule) {
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
    means <- points[ is.na(flag), list("dose" = dose, "mean.response" = mean(response)), by = dose]
    dosesAboveThreshold <- length(which(means$mean.response >= threshold))
    inactive <- dosesAboveThreshold < rule$activeDoses
    insufficientRange <- abs(pointStats$response.empiricalMax - pointStats$response.empiricalMin) < threshold
  } else {
    inactive <- FALSE
    insufficientRange <- FALSE
  }
  return(list(inactive = inactive, insufficientRange = insufficientRange))  
}

getDRCModel <- function(dataSet, drcFunction = LL.4, subs = NA, paramNames = eval(formals(drcFunction)$names), fixed, robust = "mean") {
  fixedParams <- data.frame(matrix(NA,1,length(paramNames)))
  names(fixedParams) <- paramNames
  fixed[unlist(lapply(fixed, is.null))] <- NULL
  matches <- match(paramNames, names(fixed), nomatch= FALSE)
  fixedParams[which(matches != 0)] <- fixed[matches]
  fixed <- unlist(fixedParams)
  fct <- drcFunction(fixed=fixed, names=paramNames)
  if(class(dataSet$flag) == "NULL") {
    dataSet$flag <- NA
  }
  drcObj <- NULL
  tryCatch({
    drcObj <- drm(formula = response ~ dose, data = dataSet, subset = is.na(flag), robust=robust, fct = fct, control = drmc(errorm=TRUE))
  }, error = function(ex) {
    #Turned of printing of error message because shiny was printing to the browser because of a bug
    #print(ex$message)
  })
  return(drcObj)
}

getPointStats <- function(pts) {
  pts <- copy(pts)
  pts[ is.na(flag), meanByDose := mean(response), by = dose ]
  response.empiricalMax <- pts[is.na(flag), max(meanByDose)]
  response.empiricalMin <- pts[is.na(flag), min(meanByDose)]
  dose.min <- min(pts[is.na(flag), ]$dose)
  dose.max <- max(pts[is.na(flag), ]$dose)
  return(list(response.empiricalMax = response.empiricalMax, response.empiricalMin = response.empiricalMin, dose.min = dose.min, dose.max = dose.max))
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
  myList <- as.list(coefficients(drcObj))
  names(myList) <- gsub("\\:\\(Intercept\\)","", names(myList))
  return(myList)
}
drcObject.getDRCFitStats <- function(drcObject, points) {
  if(is.null(drcObject)) return(NULL)
  SSE <- sum((residuals(drcObject))^2)
  SST <- sum((points$response-mean(points$response))^2)
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

loadDoseResponseTestData <- function(size = c("small","large")) {
  size <- match.arg(size)
  doseResponseSELFile <- switch(size,
                                "small" = system.file("docs", "Example-Dose-Response-SEL.xlsx", package="racas"),
                                "large" = system.file("docs", "Example-Dose-Response-SEL-Large.xlsx", package="racas")
  )
  t <- tempfile(fileext = ".xls")
  file.copy(doseResponseSELFile,t)
  originalWD <- getwd()
  acasHome <- normalizePath(file.path(path.package("racas"),"..",".."))
  selCode <- file.path(acasHome,"public","src","modules","GenericDataParser","src","server","generic_data_parser.R")
  setwd(acasHome)
  source(selCode, local = TRUE)
  request <- list(fileToParse=t, dryRunMode = "false", user="bbolt")
  response <- parseGenericData(request)
  if(response$hasError) {
    cat(response$errorMessages[[1]]$message)
  }
  setwd(originalWD)
  return(response$results$experimentCode)
}
