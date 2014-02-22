
LL4 <- 'min + (max - min)/((1 + exp(slope * (log(x/ec50))))^1)'
OneSiteKi <- 'min + (max-min)/(1+10^(x-log10((10^Log10Ki)*(1+ligandConc/kd))))'
MM2 <- '(max*x)/(kd + x)'

defaultDoseResponse <- function(fitData, fitSettingsJSON) {
  #Need to copy fitData so we are working with our own copy (data.table does objects by reference)
  fitData <- copy(fitData)
  
  #Extract the fit variables from json
  request <- fromJSON(fitSettingsJSON)
  myFixedParameters <- request$fixedParameters
  myParameterRules <- request$parameterRules
  myInactiveRule <- request$inactiveRule
  fitData[ , fixedParameters := list(list(myFixedParameters))]
  fitData[ , parameterRules := list(list(myParameterRules))]
  fitData[ , inactiveRule := list(list(myInactiveRule))]
  #The JSON really should have "NA" init should just be null or blank but the rjson package doesn't deal with this very well. For now, just convert "NA" to NA
  #updateFlags <- rbindlist(lapply(request$updateFlags, function(x) {x$flag <- ifelse(is.null(x$flag),as.character(NA), x$flag); return(x) })) 
  updateFlags <- as.data.table(request$updateFlags)
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
  
  #Initial fit of the data
  fitData <- doseResponseFit(fitData)
  
  #While refit is true, keep refitting using fixed parameters
  refit <- checkRefit(fitData)
  iterations <- 20
  i <- 1
  while(any(refit) & i < iterations) {
    fitData[refit, model.synced := FALSE]
    fitData[refit, fixedParameters := switch(renderingHint,
                                             "4 parameter D-R" = {
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
                                             "2 parameter Michaelis Menten" = {
                                               list(list(myfixedParameters = list(kd = NA, max = NA)))
                                             }),
            by = curveid]
    fitData <- doseResponseFit(fitData)
    refit <- checkRefit(fitData)
    i <- i + 1
  }
  
  #Categorize the fit data
  fitData[ , category := categorizeFitData(results.parameterRules, inactive, fitConverged, insufficientRange), by = curveid]
  fitData[ , DNETCategory := getDNETCategory(results.parameterRules, inactive, fitConverged, insufficientRange), by = curveid]
  #Extract the reported Parameters
  fitData[ , reportedParameters := list(list(getReportedParameters(renderingHint, results.parameterRules[[1]], inactive, fitConverged, insufficientRange, fixedParameters[[1]], fittedParameters[[1]], pointStats[[1]]))), by = curveid]
  fitData[ , analysisGroupParameters := list(list(getAnalysisGroupParameters(reportedParameters[[1]], fixedParameters[[1]], fittedParameters[[1]], goodnessOfFit.model[[1]], goodnessOfFit.parameters[[1]], category, approved))), by = curveid]
  
  return(fitData)  
}

getAnalysisGroupParameters <- function(reportedParameters, fixedParameters, fittedParameters, goodnessOfFit.model, goodnessOfFit.parameters, category, approved) {
  fitParameters <- c(fixedParameters,fittedParameters)
  names(fitParameters) <- paste0("fitted",names(fitParameters))
  analysisGroupParameters <- c(reportedParameters,fitParameters, goodnessOfFit.model, goodnessOfFit.parameters, category = category, algorithmApproved = approved)
  analysisGroupParameters[unlist(lapply(analysisGroupParameters, is.na))] <- NULL
  return(analysisGroupParameters)
}
simpleToAdvancedBulkFitRequest <- function(simpleRequest) {
  simpleRequest <- simpleBulkDoseResponseFitRequest
  file <- system.file("docs", "default-ec50-bulkFitRequest.json", package = "racas")
  defaultEC50RequestJSON <- readChar(file, file.info(file)$size)
  defaultRequest <- fromJSON(defaultEC50RequestJSON)
  
  updateDefaultRequest <- function(name, simpleRequestParameter, defaultRequest) {
    #simpleRequestParameter <- simpleRequest$min
    #name <- "min"
    if(simpleRequestParameter$limitType=="none") {
      defaultRequest$parameterRules$limits[[paste0(name,"Threshold")]] <- NULL
      defaultRequest$fixedParameters[[name]] <- NULL
    }
    if(simpleRequestParameter$limitType=="pin") {
      defaultRequest$parameterRules$limits[[paste0(name,"Threshold")]] <- NULL
      defaultRequest$fixedParameters[[name]] <- ifelse(name=="slope",-simpleRequestParameter$value,simpleRequestParameter$value)
    }
    if(simpleRequestParameter$limitType=="limit") {
      defaultRequest$parameterRules$limits[[paste0(name,"Threshold")]] <- list(parameter = name,
                                                                               type = "threshold",
                                                                               operator = switch(name,
                                                                                                 max = ">",
                                                                                                 min = "<",
                                                                                                 ec50 = ">",
                                                                                                 slope = "<"),
                                                                               value = ifelse(name=="slope",-simpleRequestParameter$value,simpleRequestParameter$value),
                                                                               displayName = paste0(name," threshold exceeded")
        )
      defaultRequest$fixedParameters[[name]] <- NULL
    }
    return(defaultRequest)
  }
  modifiedRequest <- defaultRequest
  modifiedRequest$inactiveRule$value <- simpleRequest$inactiveThreshold
  modifiedRequest$inverseAgonistMode <- simpleRequest$inverseAgonistMode
  modifiedRequest <- updateDefaultRequest("min",simpleRequest$min, modifiedRequest)
  modifiedRequest <- updateDefaultRequest(name = "max", simpleRequestParameter = simpleRequest$max, modifiedRequest)
  modifiedRequest <- updateDefaultRequest("slope", simpleRequest$slope, modifiedRequest)
  return(modifiedRequest)
}
#Check if limits have been exceeded and refit if not inactive, non-converged or insufficient range
checkRefit <- function(fitData) {
  refit <- fitData[ , switch(renderingHint,
                             "4 parameter D-R" = {  maxExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "maxThreshold" %in% results.parameterRules[[1]]$limits)
                                                    minExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "minThreshold" %in% results.parameterRules[[1]]$limits)
                                                    slopeExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "slopeThreshold" %in% results.parameterRules[[1]]$limits)
                                                    exceededAThreshold <- (maxExceeded | minExceeded | slopeExceeded)
                                                    refit <-  exceededAThreshold & (!inactive | !fitConverged | !insufficientRange)
                                                    refit
                             },{
                               warning(paste0("Refit rule not implemented for ", renderingHint))
                               FALSE
                             }),
                   by = curveid]$V1
  return(refit)
}

doseResponse <- function(fitSettingsJSON, curveids = NA, sessionID = NA, fitData = NA) {
  if(all(is.na(c(curveids, sessionID, fitData)))) stop("Must provide curveids or sessionID or fitData, all are NA")
  if(class(curveids) == "character") {
    fitData <- getFitData(curveids)
  }
  if(!is.na(sessionID)) {
    fitSettingsJSON_new <- fitSettingsJSON
    sessionID_new <- sessionID
    loadSession(sessionID)
    fitSettingsJSON <- fitSettingsJSON_new
    sessionID <- sessionID_new
    rm(fitSettingsJSON_new,sessionID_new)
    if(exists("fitData")) {
      fitData[, model.synced := FALSE]      
    } else {
      stop("fitData object does not exist in session, are you sure this was a fit session?")
    }
  }
  if(any(class(fitData) == "data.table")) {
    fitData <- defaultDoseResponse(fitData, fitSettingsJSON) 
  } else {
    stop("fitData not a data.table")
  }
  if(is.na(sessionID)) {
    sessionID <- saveSession()
  } else {
    sessionID <- saveSession(sessionID)
  }
  response <- fitToJSONReponse(fitData, sessionID = sessionID)
  return(response)
}

fitToJSONReponse <- function(fitData, ...) {
  reportedValues <- objToHTMLTableString(fitData[1]$reportedParameters[[1]])
  fitSummary <- captureOutput(summary(fitData[1]$model[[1]]))
  parameterStdErrors <- objToHTMLTableString(fitData[1]$goodnessOfFit.parameters[[1]])
  curveErrors <- objToHTMLTableString(fitData[1]$goodnessOfFit.model[[1]])
  category <- fitData[1]$category[[1]]
  acceptReject <- "FALSE"
  approved = fitData[1]$approved[[1]]
  plotData <- list(plotWindow = plotWindow(fitData[1]$points[[1]]),
                   points  = fitData[1]$points[[1]],
                   curve = predictPoints(fitData[1]$points[[1]], fitData[1]$model[[1]])
  )
  return(toJSON(list(reportedValues = reportedValues,
                     fitSummary = fitSummary,
                     parameterStdErrors = parameterStdErrors,
                     curveErrors = curveErrors,
                     category = category,
                     acceptReject = acceptReject,
                     plotData = plotData,
                     approved = approved,
                     ...     
  )))   
}

predictPoints <- function(pts, drcObj) {
  if(nrow(pts)==0 || is.null(drcObj)) {
    return(NULL)
  }
  x <- unique(pts$dose)
  if(grepl("LOG",toupper(pts$doseUnits[1]))) {
    valuesToPredict <- data.frame(x = exp( seq(log(min(x)), log(max(x)), length.out=8*length(x)) ))
  } else {
    valuesToPredict <- data.frame(x = seq(min(x), max(x), length.out=8*length(x)))
  }
  curveData <- data.frame(dose = valuesToPredict$x, response = predict(drcObj, newdata = valuesToPredict))
  return(curveData)
}


plotWindow <- function(pts){
  if(nrow(pts)==0) {
    return(NULL)
  } else {
    maxDose <- max(pts$dose)
    minDose <- min(pts$dose)
    maxResponse <- max(pts$response)
    minResponse <- min(pts$response)
    range <- abs(maxResponse-minResponse)
    xmin <- minDose - minDose/2
    ymax <- (maxResponse + 0.04*range)
    xmax <- maxDose + maxDose/2
    ymin <- (minResponse - 0.04*range)
    return(c(log(xmin),ymax,log(xmax),ymin))
  }
}
captureOutput <- function(obj) {
  return(paste(capture.output({
    result <- withVisible(obj)
    if (result$visible)
      print(result$value)
  }), collapse="\n"))
}


objToHTMLTableString <- function(obj) {
  htmlTableString <- ""
  if(is.null(obj)) {return(htmlTableString)}
  if(class(obj) == "list") {
    obj <- as.data.frame(lapply(obj, function(x) if(is.null(x)){return(NA)} else {return(x)}))
    #obj <- as.data.frame(obj)
  }
  if(nrow(obj) == 0) {
    return(htmlTableString)
  }
  t <- tempfile()
  htmlTableString <- print(xtable(obj), 
                           type = "html", 
                           include.rownames = FALSE, 
                           comment = FALSE, 
                           timestamp = FALSE, 
                           rotate.rownames = TRUE, 
                           file = t,
                           html.table.attributes = "")
  unlink(t)
  return(htmlTableString)
}

getReportedParameters <- function(renderingHint, results, inactive, fitConverged, insufficientRange, fixedParameters, fittedParameters, pointStats) {
  switch(renderingHint,
         "4 parameter D-R" = {
           if(inactive | insufficientRange) {
             max <- pointStats$response.empiricalMax
             min <- pointStats$empiricalMin
             slope <- NULL
             ec50 <- pointStats$dose.max
             ec50Operator <- ">"
             reportedValues <- list(min = min,max = max, slope = slope, ec50 = ec50, ec50Operator = ec50Operator)
             reportedValues[which(unlist(lapply(reportedValues, is.null)))] <- NULL
             return(reportedValues)
             
           }
           if(!fitConverged) {
             return(NULL)
           }
           if("maxUncertaintyRule" %in% results$goodnessOfFits) {
             max <- pointStats$response.empiricalMax
           } else {
             if(is.NULLorNA(fixedParameters$max)) {
               max <- fittedParameters$max
             } else {
               max <- fixedParameters$max
             }
           }
           if("minUncertaintyRule" %in% results$goodnessOfFits) {
             min <- pointStats$empiricalMin
           } else {
             if(is.NULLorNA(fixedParameters$min)) {
               min <- fittedParameters$min
             } else {
               min <- fixedParameters$min
             }
           }
           if(is.NULLorNA(fixedParameters$slope)) {
             slope <- -fittedParameters$slope
           } else {
             slope <- -fixedParameters$slope
           }
           if("ec50Threshold" %in% results$limits) {
             ec50 <- pointStats$dose.max
             ec50Operator <- ">"
           } else {
             ec50 <- fittedParameters$ec50
             ec50Operator <- ""
           }
           reportedValues <- list(min = min,max = max, slope = slope, ec50 = ec50, ec50Operator = ec50Operator)
           reportedValues[which(unlist(lapply(reportedValues, is.null)))] <- NULL
           return(reportedValues)
         },
         "2 parameter Michaelis Menten" = {
           if(inactive | insufficientRange) {
             max <- pointStats$response.empiricalMax
             kd <- pointStats$dose.max
             kdOperator <- ">"
             reportedValues <- list(max = max, kd = kd, kdOperator = kdOperator)
             reportedValues[which(unlist(lapply(reportedValues, is.null)))] <- NULL
             return(reportedValues)
           }
           if(!fitConverged) {
             return(list())
           }
           if("maxUncertaintyRule" %in% results$goodnessOfFits) {
             max <- pointStats$response.empiricalMax
           } else {
             if(is.NULLorNA(fixedParameters$max)) {
               max <- fittedParameters$max
             } else {
               max <- fixedParameters$max
             }
           }
           if("kdThreshold" %in% results$limits) {
             kd <- pointStats$dose.max
             kdOperator <- ">"
           } else {
             kd <- fittedParameters$kd
             kdOperator <- ""
           }
           reportedValues <- list(max = max, kd = kd, kdOperator = kdOperator)
           reportedValues[which(unlist(lapply(reportedValues, is.null)))] <- NULL
           return(reportedValues)
         },
{warning("Not implemented for ", renderingHint)
 return(list())
}
  )
}

getCurveCuratorInfo <- function(experimentCode = NA) {
  experimentCode <- "EXPT-00012386"
  curveids <- query(paste0("SELECT p.string_value as curveid
                FROM p_api_analysis_group_results p join experiment e on p.EXPERIMENT_ID=e.id
                WHERE p.LS_KIND LIKE '%curve id'
                and e.code_name = ",sqliz(experimentCode)))
  names(curveids) <- "curveid"
}
getFitData <- function(curveids, ...) {
  fitData <- getCurveData(curveids, flagsAsLogical = FALSE, ...)
  fitData$points <- cbind(fitData$points, flagChanged = FALSE)
  fitData <- data.table(curveid = unique(as.character(fitData$points$curveid,fitData$parameters$curveid))[order(unique(as.character(fitData$points$curveid,fitData$parameters$curveid)))], 
                        renderingHint = fitData$parameters$renderingHint, 
                        points = split(as.data.table(fitData$points), fitData$points$curveid),
                        parameters = split(as.data.table(fitData$parameters), fitData$parameters$curveid),
                        key = "curveid")
  myParameterRules <- list(goodnessOfFits = list(), limits = list())
  myInactiveRule <- list()
  myFixedParameters <- list()
  fitData[ , c("parameterRules", "inactiveRule", "fixedParameters") := list(list(myParameterRules),
                                                                            list(myInactiveRule),
                                                                            list(myFixedParameters))]
  fitData[ , model.synced := FALSE]
  return(fitData)
}

doseResponseFit <- function(fitData, refit = FALSE, ...) {
  fitDataNames <- names(fitData)
  ###Fit
  fitData[model.synced == FALSE, model := list(model = list(switch(renderingHint,
                                                                   "4 parameter D-R" = getDRCModel(points[[1]], drcFunction = LL.4, paramNames = c("slope", "min", "max", "ec50"), fixed = fixedParameters[[1]]),
                                                                   "3 parameter Michaelis Menten" = getDRCModel(points[[1]], drcFunction = MM.3, paramNames = c("slope","max", "kd"), fixed = fixedParameters[[1]]),
                                                                   "2 parameter Michaelis Menten" = getDRCModel(points[[1]], drcFunction = MM.2, paramNames = c("max", "kd"), fixed = fixedParameters[[1]])
  ))
  ), by = curveid]
  
  setkey(fitData, "curveid")
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
  category <- "Good Fit"
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
  category <- "Good Fit"
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
    inactive <- length(which(means$mean.response >= threshold)) < rule$activeDoses
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
#fitModelNormalized <- drm(NORMALIZEDRESULT ~ CONCENTRATION, data = points, fct = list(kifct, kissfct, kiNames), curveid=PTODWELLLITERAL, robust = "mean")
#fitModelEfficacy <- drm(EFFICACY ~ CONCENTRATION, data = points, fct = list(kifct, kissfct, kiNames),curveid=PTODWELLLITERAL, robust = "mean")


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

drcObject.getKeyValues.as.dataFrame <- function(drcObj = drcObject) {
  #Get calculated values (non fixed parameters)
  fitValues <- as.data.frame(drcObj$parmMat)
  row.names(fitValues) <- drcObj$fct$names
  fixedValues <- as.data.frame(drcObj$fct$fixed)
  fixedValues <- subset(fixedValues, row.names(fixedValues) <= length(drcObj$paramNames))
  row.names(fixedValues) <- drcObj$paramNames
  names(fixedValues) <- drcObj$name
  fixedValues <- subset(fixedValues, !is.na(fixedValues))	
  keyValues <- rbind(fitValues,fixedValues)
  keyValues <- as.data.frame(t(keyValues))
  return(keyValues)
}

drcList.getKeyValues.as.dataFrame <- function(drcList = drcObjectList) {
  keyValueMatrix <- t(sapply(drcList, drcObject.getKeyValues.as.dataFrame))
  mode(keyValueMatrix) <- "numeric"
  keyValueDataFrame <- as.data.frame(keyValueMatrix)
  return(keyValueDataFrame)
}

drcObject.getMaxDose.as.numeric <- function(drcObj = drObject) {
  name <- drcObj$dataList$names$dName
  value <- max(drcObj$data[,name])
  return(value)
}

drcObject.getMinDose.as.numeric <- function(drcObj = drObject) {
  name <- drcObj$dataList$names$dName
  value <- min(drcObj$data[,name])
  return(value)
}

drcObject.getMaxActivity.as.numeric <- function(drcObj = drObject) {
  name <- drcObj$dataList$names$orName
  dose <- max(drcObj$data[,name])
  return(dose)
}

drcObject.getMinActivity.as.numeric <- function(drcObj = drObject) {
  name <- drcObj$dataList$names$orName
  value <- min(drcObj$data[,name])
  return(value)
}

drcObject.getDRCFitStats <- function(drcObject, points) {
  if(is.null(drcObject)) return(NULL)
  SSE <- sum(residuals(drcObject)^2)
  SST <- sum(points$response-mean(points$response)^2)
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
drcObject.getEC50Intercept.as.numeric <- function(drcObj = drcObject) {
  ed50 <- as.data.frame(ED(drcObj, respLev = 50, display = FALSE))[1]
  value <- predict(drcObj,data.frame(conc=ed50))[[1]]
  return(value)
}

drcObject.getPredictedResponseFromDose.as.numeric <- function(drcObj = drcObject, responseLevel = numericValue) {
  value <- predict(drcObj,data.frame(conc=responseLevel))[[1]]
  return(value)
}

fitParams.getOperatorParams <- function(fixedValues) {
  #Removed fixed values with operators
  fixedValues <- subset(fixedValues,!is.na(fixedValues$resultoperator))
  return(fixedValues)
}

fitParams.getAboveMaxTestedParams <- function(drData = data$rawPoints, curveID = "curve_id", fixedValues = data$fitParams) {
  #Function to remove IC50 if above the max concentration tested
  IC50AboveMaxTested <- function(id) {
    returnValues <- subset(fixedValues,FALSE)
    if(length(fixedValues$resultvalue[fixedValues$curveid==id]) > 0) {
      maxTestedConc <- max(drData$dose[drData$TREATMENTGROUP==id])
      fixedIC50 <- fixedValues$resultvalue[fixedValues$curveid==id]
      if(fixedIC50 > maxTestedConc) {
        returnValues <- subset(fixedValues,fixedValues$curveid==id)
      }
      
    }
    return(returnValues)
  }
  treatmentGroups <- split(drData, drData[,curveID])
  drData$TREATMENTGROUP <- factor(drData[,curveID])
  fixedValuesList <- sapply(levels(drData$TREATMENTGROUP), simplify = FALSE, USE.NAMES = TRUE, FUN = IC50AboveMaxTested)
  fixedValues <- do.call("rbind", lapply(fixedValuesList, data.frame, stringsAsFactors = FALSE))
  row.names(fixedValues) <- NULL
  return(fixedValues)
}

setdiff.data.frame <- function(A,B) {
  if(nrow(B) == 0) {
    ans <- A
  } else {
    ans <- A[!duplicated( rbind(B,A) )[ -seq_len(nrow(B))] , ]
  }
  return(ans)
}

fitParams.getOperatorParams <- function(fixedValues) {
  #Removed fixed values with operators
  fixedValues <- subset(fixedValues,!is.na(fixedValues$resultoperator))
  return(fixedValues)
}

fitParams.getAboveMaxTestedParams <- function(drData = data$rawPoints, curveID = "curve_id", fixedValues = data$fitParams) {
  #Function to remove IC50 if above the max concentration tested
  IC50AboveMaxTested <- function(id) {
    returnValues <- subset(fixedValues,FALSE)
    if(length(fixedValues$resultvalue[fixedValues$curveid==id]) > 0) {
      maxTestedConc <- max(drData$dose[drData$TREATMENTGROUP==id])
      fixedIC50 <- fixedValues$resultvalue[fixedValues$curveid==id]
      if(fixedIC50 > maxTestedConc) {
        returnValues <- subset(fixedValues,fixedValues$curveid==id)
      }
      
    }
    return(returnValues)
  }
  treatmentGroups <- split(drData, drData[,curveID])
  drData$TREATMENTGROUP <- factor(drData[,curveID])
  fixedValuesList <- sapply(levels(drData$TREATMENTGROUP), simplify = FALSE, USE.NAMES = TRUE, FUN = IC50AboveMaxTested)
  fixedValues <- do.call("rbind", lapply(fixedValuesList, data.frame, stringsAsFactors = FALSE))
  row.names(fixedValues) <- NULL
  return(fixedValues)
}

