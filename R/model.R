
LL4 <- 'min + (max - min)/((1 + exp(slope * (log(x/ec50))))^1)'
OneSiteKi <- 'min + (max-min)/(1+10^(x-log10((10^Log10Ki)*(1+ligandConc/kd))))'
MM2 <- '(max*x)/(kd + x)'

defaultDoseResponse <- function(fitData, fitSettingsJSON) {
  #Need to copy fitData so we are working with our own copy (data.table does objects by reference)
  fitData <- copy(fitData)
  
  #Extract the fit variables from json
  request <- fromJSON(fitSettingsJSON)
  myFixedParameters <- as.data.frame(lapply(request$fixedParameters, function(x) if(is.null(x$value)) {return(NA)} else {return(x$value)}))
  myParameterRules <- request$parameterRules
  myInactiveRule <- request$inactiveRule
  fitData[ , fixedParameters := list(list(myFixedParameters))]
  fitData[ , parameterRules := list(list(myParameterRules))]
  fitData[ , inactiveRule := list(list(myInactiveRule))]
  updateFlags <- as.data.table(request$updateFlags) 
  
  #First update the flags in points
  updFlags <- function(crv,i,fl, chng) {
    sel <- updateFlags[updateFlags$id %in% i & updateFlags$curveid == unique(crv),]
    matches <- match(i, sel$id)
    newFlags <- sel[matches,]$flag
    noMatch <- which(is.na(newFlags))
    newFlags[noMatch] <- fl[noMatch]
    changed <- chng | (fl != newFlags)
    return(list(flag = newFlags, flagChanged = changed))
  }
  if(nrow(updateFlags) > 0 ) {
    fitData[, points := list(list(points[[1]][, c("flag","flagChanged") := updFlags(curveid,id,flag,flagChanged)])) , by = curveid]
  }
  
  #Initial fit of the data
  fitData <- doseResponseFit(fitData)
  
  #While refit is true, keep refitting using fixed parameters
  refit <- checkRefit(fitData)
  while(any(refit)) {
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
                                               list(list(myfixedParameters = list(max = fixedMax, min = fixedMin, slope = fixedSlope, ec50 = NA)))
                                             },
                                             "2 parameter Michaelis Menten" = {
                                               list(list(myfixedParameters = list(kd = NA, max = NA)))
                                             }),
            by = curveid]
    fitData <- doseResponseFit(fitData)
    refit <- checkRefit(fitData)
  }
  
  #Categorize the fit data
  fitData[ , category := categorizeFitData(results.parameterRules, inactive, fitConverged, insufficientRange), by = curveid]
  #Extract the reported Parameters
  fitData[ , reportedParameters := list(list(getReportedParameters(renderingHint, results.parameterRules[[1]], inactive, fitConverged, insufficientRange, fixedParameters[[1]], fittedParameters[[1]], pointStats[[1]]))), by = curveid]
  
  return(fitData)  
}

#Check if limits have been exceeded and refit if not inactive, non-converged or insufficient range
checkRefit <- function(fitData) {
  refit <- fitData[ , switch(renderingHint,
                             "4 parameter D-R" = {  maxExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "maxThreshold" %in% results.parameterRules[[1]]$limits)
                                                    minExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "minThreshold" %in% results.parameterRules[[1]]$limits)
                                                    slopeExceeded <- ifelse(is.null(results.parameterRules[[1]]$limits), FALSE, "slopeThreshold" %in% results.parameterRules[[1]]$limits)
                                                    exceeded <- (maxExceeded | minExceeded | slopeExceeded)
                                                    refit <-  exceeded | inactive & !fitConverged & insufficientRange
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
  if(grepl("LOG",toupper(pts$dose_unit[1]))) {
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
    #obj <- as.data.frame(lapply(obj, function(x) if(is.null(x)){return(NA)} else {return(x)}))
    obj <- as.data.frame(obj)
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
             return(list(min = min, max = max, slope = slope, ec50 = ec50, ec50Operator = ec50Operator))
             
           }
           if(!fitConverged) {
             max <- NULL
             min <- NULL
             slope <- NULL
             ec50 <- NULL
             ec50Operator <- NULL
             return(list(min = min, max = max, slope = slope, ec50 = ec50, ec50Operator = ec50Operator))
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
             slope <- fittedParameters$slope
           } else {
             slope <- fixedParameters$slope
           }
           if("EC50 threshold exceeded" %in% results$limits) {
             ec50 <- pointStats$dose.max
             ec50Operator <- ">"
           } else {
             ec50 <- fittedParameters$ec50
             ec50Operator <- ""
           }
           
           return(list(min = min, max = max, slope = slope, ec50 = ec50, ec50Operator = ec50Operator))
   
         },
         "2 parameter Michaelis Menten" = {
           if(inactive | insufficientRange) {
             max <- pointStats$response.empiricalMax
             kd <- pointStats$dose.max
             kdOperator <- ">"
             return(list(max = max, kd = kd, kdOperator = kdOperator)) 
           }
           if(!fitConverged) {
             max <- NULL
             kd <- NULL
             kdOperator <- NULL
             return(list(max = max, kd = kd, kdOperator = kdOperator))
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
           return(list(max = max, kd = kd, kdOperator = kdOperator))
         },
         {warning("Not implemented for ", renderingHint)
          return(list())
         }
         )
}

is.NULLorNA <- function(value) {
  if(is.null(value)) return(TRUE)
  return(is.na(value))
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
  fitData <- getCurveData(curveids, ...)
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
    means <- points[ flag == FALSE ,list("dose" = dose, "mean.response" = mean(response)), by = dose]
    inactive <- length(which(means$mean.response >= threshold)) < rule$activeDoses
    insufficientRange <- abs(pointStats$response.empiricalMax - pointStats$response.empiricalMin) < threshold
  } else {
    inactive <- FALSE
    insufficientRange <- FALSE
  }
  return(list(inactive = inactive, insufficientRange = insufficientRange))  
}

getDRCModel <- function(dataSet, drcFunction = LL.4, subs = NA, paramNames = eval(formals(drcFunction)$names), fixed, robust = "mean") {
  if(length(fixed) == 0) fixed <- eval(formals(drcFunction)$fixed)
  if(is.null(names(fixed))) names(fixed) <- paramNames
  fixed <- unlist(fixed[match(paramNames, names(fixed))])
  fct <- drcFunction(fixed=fixed, names=paramNames)
  if(class(dataSet$flag) == "NULL") {
    dataSet$flag <- FALSE
  }
  drcObj <- NULL
  tryCatch({
    drcObj <- drm(formula = response ~ dose, data = dataSet, subset = !dataSet$flag, robust=robust, fct = fct, control = drmc(errorm=TRUE))
  }, error = function(ex) {
    #Turned of printing of error message because shiny was printing to the browser because of a bug
    #print(ex$message)
  })
  return(drcObj)
}

getPointStats <- function(pts) {
  pts <- copy(pts)
  pts[ flag == FALSE, meanByDose := mean(response), by = dose ]
  response.empiricalMax <- pts[flag == FALSE, max(meanByDose)]
  response.empiricalMin <- pts[flag == FALSE, min(meanByDose)]
  dose.min <- min(pts[flag==FALSE, ]$dose)
  dose.max <- max(pts[flag==FALSE, ]$dose)
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

#' Function to save dose response curve data
#' 
#' This function takes in changes to an already saved curve and updates the flags and fitted parameters
#'
#' @param subjectData A data frame of the changed flag ids (see examples for more on format)
#' 
#' Format:
#' \itemize{
#'   \item analysisGroupdID  The analysisGroupID of the flag
#'   \item valueKind A string that says "Flag"
#'   \item valueType A string that says "stringValue"
#'   \item subjectID The subject id of the flag
#'   \item subjectStateID The subjectStateID of the flag
#'   \item treatmentGroupID The treatmentGroupdID of the flag 
#'   \item stringValue A string value for the flag (E.G. "User" or "Algorithm")
#' }
#' @param analysisGroupData A data frame of the parameters to be changed (see examples for more on format)
#' 
#' Format: 
#' \itemize{
#'   \item analysisGroupdID
#'   \item batchCode
#'   \item valueType
#'   \item valueKind
#'   \item resultUnits
#'   \item numericValue
#'   \item StringValue
#' }
#' @keywords save, update, dose, response, curve
#' @export
#' 
#' 


updateDoseResponseCurve <- function(exptCode, fitData) {
  
  
}

saveDoseResponseCurveSet <- function(experimentCode, fitData) {
  
  #Verify the experiment code given exists
  experimentCode <- "EXPT-00012386"
  experimentExists <- checkExistence(experimentCode, type = "experimentCode")
  if(!experimentExists[[1]]) stop(paste0("experiment code \'",experimentCode,"\' not found"))
  
  #Update the subject data with any new flags
  subjectIDs <- unique(rbindlist(fitData$points)$s_id)
  subjects <- fromJSON(getURL(
    paste0(racas::applicationSettings$client.service.persistence.fullpath, "subjects/stateTypeAndKind/data_results/jsonArray"),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(lapply(unique(subjectIDs[[1]]), function(x) list(id = x))))
  )
  
  lsTransaction <- createLsTransaction()
  copyStates <- function(subject) {
    # Args:
    #   subject:    A list that is a subject
    #
    # Returns:
    #   a list of new states that are copies of the old ones
    
    # Copy and update these states
    lsTypeAndKind <- sapply(subject$lsStates, function(x) x$lsTypeAndKind)
    nonIgnoredStates <- sapply(subject$lsStates, function(x) !x$ignored)
    statesToChange <- subject$lsStates[lsTypeAndKind =="data_results" & nonIgnoredStates]
    
    setTransaction <- function(stateToChange, subjectId) {
      stateToChange$lsTransaction_Id <- lsTransaction$id
      stateToChange$subjectValues <- NULL
      stateToChange$subject = list(id = subjectId, version = 0)
      return(stateToChange)
    }
    statesToChange <- lapply(statesToChange, setTransaction, subject$id)
    return (statesToChange)
  }
  
  newStates <- unlist(lapply(subjects, copyStates), recursive = FALSE)
  originalStateIds <- sapply(newStates, function(x) x$id)
  newStates <- lapply(newStates, function(x) {x$id <- NULL; return(x)})
  
  savedSubjectStates <- saveAcasEntities(newStates, "subjectstates")
  
  subjectStateIds <- sapply(savedSubjectStates, function(x) x$id)
  
  stateIdsToIgnore <- unique(originalStateIds)
  
  subjectStateTranslation <- data.frame(subjectStateId=subjectStateIds, originalStateId=originalStateIds)
  
  
}

updateDoseResponseCurve <- function (analysisGroupData, subjectData) {
  require(racas)
  require(RCurl)
  require(rjson)
  require(plyr)
  
  lsServerURL <- racas::applicationSettings$client.service.persistence.fullpath
  
  lsTransaction <- createLsTransaction()
  
  ##### Subject States ====================================================================================
  
  updateFlags <- function (valueTable, newFlags) {
    require(plyr)
    
    addFlags <- function(stateID, stringValue) {
      newRow <- data.frame(ignored = FALSE, publicData = TRUE, stringValue = stringValue, valueKind = "flag", 
                           valueType = "stringValue", stateID = stateID, stringsAsFactors=FALSE)
    }
    
    valueTableWithoutFlags <- valueTable[valueTable$valueKind != "flag", ]
    flagValueTable <- mdply(newFlags, .fun = addFlags)
    
    return(rbind.fill(valueTableWithoutFlags, flagValueTable))
  }
  
  # This is a closure. Don't move it out without pulling lsTransaction through
  copyStates <- function(subject) {
    # Args:
    #   subject:    A list that is a subject
    #
    # Returns:
    #   a list of new states that are copies of the old ones
    
    
    # Copy and update these states
    lsTypeAndKind <- sapply(subject$lsStates, function(x) x$lsTypeAndKind)
    nonIgnoredStates <- sapply(subject$lsStates, function(x) !x$ignored)
    statesToChange <- subject$lsStates[lsTypeAndKind =="data_results" & nonIgnoredStates]
    
    setTransaction <- function(stateToChange, subjectId) {
      stateToChange$lsTransaction <- lsTransaction
      stateToChange$subjectValues <- NULL
      stateToChange$subject = list(id = subjectId, version = 0)
      return(stateToChange)
    }
    statesToChange <- lapply(statesToChange, setTransaction, subject$id)
    return (statesToChange)
  }
  
  subjects <- fromJSON(getURL(
    paste0(lsServerURL, "subjects/stateTypeAndKind/data_results/jsonArray"),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(lapply(unique(subjectData$subjectID), function(x) list(id = x))))
  )
  
  newStates <- unlist(lapply(subjects, copyStates), recursive = FALSE)
  originalStateIds <- sapply(newStates, function(x) x$id)
  newStates <- lapply(newStates, function(x) {x$id <- NULL; return(x)})
  
  savedSubjectStates <- saveAcasEntities(newStates, "subjectstates")
  subjectStateIds <- sapply(savedSubjectStates, function(x) x$id)
  
  stateIdsToIgnore <- unique(originalStateIds)
  
  subjectStateTranslation <- data.frame(subjectStateId=subjectStateIds, originalStateId=originalStateIds)
  
  ##### Subject Values ====================================================================================
  
  valuesWithStatesIncluded <- ldply(subjects, getValuesFromSubject)
  
  valuesToReplace <- valuesWithStatesIncluded[valuesWithStatesIncluded$stateID %in% subjectData$subjectStateID & 
                                                !(valuesWithStatesIncluded$ignored), ]
  
  valueIdsToIgnore <- unique(valuesToReplace$id)
  
  # Not sure why this is here anymore...
  valuesToReplace <- valuesToReplace[!(is.na(valuesToReplace$stateID)), ]
  
  valuesToReplace$id <- NULL
  
  newFlags <- subjectData[subjectData$valueKind == "flag", c("subjectStateID", "stringValue")]
  names(newFlags) <- c("stateID","stringValue")
  valuesToReplace <- updateFlags(valuesToReplace, newFlags)
  
  valuesToReplace$oldSubjectStateID <- valuesToReplace$stateID
  valuesToReplace$stateID <- subjectStateTranslation$subjectStateId[match(valuesToReplace$stateID,
                                                                          subjectStateTranslation$originalStateId)]
  
  valuesToReplace$stateVersion <- 0
  valuesToReplace$stateGroupIndex <- 1
  
  savedSubjectValues <- saveValuesFromLongFormat(valuesToReplace, stateGroupIndices=1, entityKind="subject", lsTransaction=lsTransaction)
  
  # Ignore old subject states
  query(paste0(
    "UPDATE subject_state
    SET ignored       = 1,
    version                   = version+1
    WHERE id          IN (", paste(stateIdsToIgnore, collapse=","), ")"))
  # Ignore old subject values
  query(paste0(
    "UPDATE subject_value
    SET ignored       = 1,
    version                   = version+1
    WHERE id          IN  (", paste(valueIdsToIgnore, collapse=","), ")"))
  
  
  ##### Treatment Groups ======================================================
  treatmentGroupStateGroups <- list(list(entityKind = "treatmentgroups",
                                         stateType = "data", 
                                         stateKind = "results",
                                         includesOthers = TRUE,
                                         includesCorpName = FALSE))
  
  treatmentGroupInput <- valuesToReplace[valuesToReplace$valueKind %in% c("Response", "flag"), 
                                         c('numericValue', 'stringValue', 'valueKind', 'valueOperator', 'valueType', 'valueUnit','oldSubjectStateID')]
  
  treatmentGroupIdTranslation <- subjectData[,c('subjectStateID', 'treatmentGroupID')]
  
  treatmentGroupInput$treatmentGroupID <- treatmentGroupIdTranslation$treatmentGroupID[match(treatmentGroupInput$oldSubjectStateID, 
                                                                                             treatmentGroupIdTranslation$subjectStateID)]
  
  createTreatmentGroupData <- function(inputData) {
    flaggedStateIDs <- inputData$oldSubjectStateID[inputData$valueKind == "flag" & ((inputData$stringValue != "") | is.na(inputData$stringValue))]
    dataToAverage <- inputData[!(inputData$oldSubjectStateID %in% flaggedStateIDs) & inputData$valueType=="numericValue", ]
    return(data.frame(
      valueKind = "Response", 
      valueType = "numericValue",
      # TODO: once there is more real data, add this back in and check
      #valueUnit = inputData$valueUnit[1],
      treatmentGroupID = inputData$treatmentGroupID[1],
      numberOfReplicates = nrow(dataToAverage),
      uncertainty = sd(dataToAverage$numericValue),
      uncertaintyType = "standard deviation",
      numericValue = if (is.numeric(dataToAverage$numericValue)) mean(dataToAverage$numericValue) else NA,
      stringValue = if (!is.numeric(dataToAverage$numericValue)) "flagged" else NA
    ))
  }
  treatmentGroupData <- ddply(treatmentGroupInput, .(treatmentGroupID), createTreatmentGroupData)
  
  treatmentGroupData$stateGroupIndex <- 1
  treatmentGroupData$publicData <- TRUE
  
  query(paste0(
    "UPDATE treatment_group_state
    SET ignored               = 1,
    version                   = version+1
    WHERE state_type_and_kind = 'data_results'
    AND ignored               = 0
    AND treatment_group_id    IN (", paste(treatmentGroupData$treatmentGroupID, collapse=","), ")"))
  
  query(paste0(
    "UPDATE treatment_group_value
    SET ignored               = 1,
    version                   = version+1
    WHERE treatment_state_id    IN
    (SELECT id
    FROM treatment_group_state
    WHERE state_type_and_kind = 'data_results'
    AND ignored               = 0
    AND treatment_group_id IN (", paste(treatmentGroupData$treatmentGroupID, collapse=","), "))"))
  
  treatmentGroupData$stateID <- saveStatesFromLongFormat(entityData = treatmentGroupData, entityKind = "treatmentgroup", 
                                                         stateGroups=treatmentGroupStateGroups, stateGroupIndices = 1, 
                                                         idColumn = "treatmentGroupID", recordedBy = "curveCuration", lsTransaction = lsTransaction)[['entityStateId']]
  treatmentGroupData$stateVersion <- 0
  savedTreatmentGroupValues <- saveValuesFromLongFormat(entityData = treatmentGroupData, entityKind = "treatmentgroup", 
                                                        stateGroups = treatmentGroupStateGroups, stateGroupIndices = 1,
                                                        lsTransaction = lsTransaction)
  
  #####  AnalysisGroups =======================================================
  
  analysisGroupStateGroups <- list(list(entityKind = "analysisgroups",
                                        stateType = "data", 
                                        stateKind = "Dose Response",
                                        includesOthers = TRUE,
                                        includesCorpName = TRUE))
  
  analysisGroupData$stateGroupIndex <- 1
  analysisGroupData$publicData <- TRUE
  
  analysisGroupsToIgnore <- unique(analysisGroupData$analysisGroupID)
  
  sqlAnalysisGroupIds <- paste(analysisGroupData$analysisGroupID, collapse = ",")
  query(paste0(
    "UPDATE analysis_group_state
    SET ignored               = 1,
    version                   = version+1
    WHERE state_type_and_kind = 'data_Dose Response'
    AND analysis_group_id     IN (", sqlAnalysisGroupIds, ")"))
  query(paste0(
    "UPDATE analysis_group_value
    SET ignored               = 1,
    version                   = version+1
    WHERE analysis_state_id    IN
    (SELECT id
    FROM analysis_group_state
    WHERE state_type_and_kind = 'data_Dose Response'
    AND analysis_group_id IN (", sqlAnalysisGroupIds, "))"))
  
  analysisGroupData$stateID <- saveStatesFromLongFormat(entityData = analysisGroupData, entityKind = "analysisgroup", 
                                                        stateGroups=analysisGroupStateGroups, stateGroupIndices = 1, 
                                                        idColumn = "analysisGroupID", recordedBy = "curveCuration", lsTransaction = lsTransaction)[['entityStateId']]
  analysisGroupData$stateVersion <- 0
  analysisGroupData <- rbind.fill(analysisGroupData, meltBatchCodes(analysisGroupData, 1))
  saveValuesFromLongFormat(entityData = analysisGroupData, entityKind = "analysisgroup", 
                           stateGroups = analysisGroupStateGroups, stateGroupIndices = 1,
                           lsTransaction = lsTransaction)
  return(TRUE)
}

replaceNullWithNA <- function(inputList) {
  inputList[sapply(inputList,is.null)] <- NA
  return(inputList)
}
addSubjectState <- function(subjectValue, subjectStateId) {
  subjectValue$stateID <- subjectStateId
  subjectValue$lsTransaction <- subjectValue$lsTransaction$id
  subjectValue <- replaceNullWithNA(subjectValue)
  return(as.data.frame(subjectValue, stringsAsFactors=FALSE))
  i <<- i+1
}
getValuesFromState <- function(subjectState) {
  require('plyr')
  return(ldply(subjectState$subjectValues, addSubjectState, subjectStateId=subjectState$id))
}
getValuesFromSubject <- function(subject) {
  require('plyr')
  return(ldply(subject$lsStates, getValuesFromState))
}
