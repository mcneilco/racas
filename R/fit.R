require(drc)
LL4 <- 'min + (max - min)/((1 + exp(-hill * (log(x/ec50))))^1)'
KiFCT <- 'Bottom + (Top-Bottom)/(1+10^(x-log10((10^Log10Ki)*(1+HotNM/HotKDNM))))'
getFitModel <- function(dataSet, drcFunction = LL.4, subs = NA, paramNames = eval(formals(drcFunction)$names), fixedValues = eval(formals(drcFunction)$fixed), robust = "mean") {
  fct <- drcFunction(fixed=fixedValues, names=paramNames)
  drcObj <- NULL
  tryCatch({
    drcObj <- drm(formula = response ~ dose, data = dataSet, subset = !dataSet$flag, robust=robust, fct = fct)
  },
           error = function(ex) {
             #Turned of printing of error message because shiny was printing to the browser because of a bug
             #print(ex$message)
           })
  return(drcObj)
}

getCurveData <- function(curveids, ...) {
  points <- query(paste("SELECT curveid, dose, doseunits, response, responseunits, flag, response_ss_id, s_id, tg_id, ag_id from api_dose_response where curveid in (",sqliz(curveids),")",sep=""), ...)
  names(points) <- tolower(names(points))
  
  points <- data.frame(  curveid = as.factor(points$curveid),
                         dose = as.numeric(points$dose), 
                         doseUnits = as.factor(points$doseunits), 
                         response = as.numeric(points$response),
                         responseUnits = as.factor(points$responseunits),
                         flag = as.factor(points$flag),
                         response_ss_id = as.numeric(points$response_ss_id),
                         s_id = as.numeric(points$s_id),
                         tg_id = as.numeric(points$tg_id),
                         ag_id = as.numeric(points$tg_id)
  )
  
  if(nrow(points) > 0) {
    points$flag <- factor(points$flag, levels = c(levels(points$flag), TRUE, FALSE))
    points$flag <- !is.na(points$flag)
    points$id <- 1:nrow(points)
  }
  
  parameters <- query(paste("SELECT TESTED_LOT,
  						  AG_ID,
							  AGV_ID,
							  VALUE_KIND,
							  VALUE_OPERATOR,
							  NUMERIC_VALUE,
							  STRING_VALUE,
							  VALUE_UNIT,
							  RECORDED_DATE
							FROM api_analysis_group_results
							WHERE ag_id IN
							  (SELECT ag_id
							  FROM api_analysis_group_results
							  WHERE value_kind='curve id'
							  AND string_value in (",sqliz(curveids),"))
						"))
  names(parameters) <- tolower(names(parameters))
  
  longFormat <- parameters
  row.names(longFormat) <- parameters$agv_id
  longFormat <- longFormat[,c("ag_id","tested_lot","value_kind","numeric_value","string_value","value_unit")]
  wideFormat <- reshape(longFormat,
                        timevar="value_kind",
                        idvar=c("ag_id","tested_lot"),direction="wide")
  
  wideName = c("tested_lot", "string_value.curve id", "numeric_value.Min","numeric_value.Fitted Min",
               "numeric_value.Max", "numeric_value.Fitted Max", "numeric_value.Hill slope", "numeric_value.Fitted Hill slope", 
               "numeric_value.EC50", "numeric_value.Fitted EC50", "string_value.Operator")
  newName = c("tested_lot", "curveid", "min", "fittedmin",
                           "max", "fittedmax", "hill", "fittedhillslope",
                           "ec50", "fittedec50", "operator")
  valuesToGet <- data.frame(wideName = as.character(wideName), newName = as.character(newName))
  parameters <- data.frame(ag_id = as.factor(wideFormat$ag_id))
  for(i in 1:nrow(valuesToGet)) {
    colName <- as.character(valuesToGet$wideName[i])
    newName <- as.character(valuesToGet$newName[i])
    if(colName %in% colnames(wideFormat)) {
      split <- strsplit(colName,"\\.")[[1]]
      if(length(split)==1) {
        parameters <- cbind(parameters,  wideFormat[,split])
      } else {
        parameters <- cbind(parameters, switch(split[1],
               "numeric_value" = as.numeric(wideFormat[,colName]),
               "string_value" = as.factor(wideFormat[,colName])
               ))        
      } 
    } else {
      parameters <- cbind(parameters, switch(split[1],
                               "numeric_value" = as.numeric(rep(NA, times = nrow(parameters))),
                               "string_value" = factor(rep(NA, times = nrow(parameters)))
      ))      
    }
    names(parameters)[ncol(parameters)] <- newName
  }
  
  return (list(
    points = points,
    parameters = parameters
  ))
}

drcObject.getKeyValues <- function(drcObj = drcObject) {
  #Get calculated values (only non-fixed parameters)
  fitValues <- as.data.frame(drcObj$parmMat)
  row.names(fitValues) <- drcObj$fct$names
  fixedValues <- as.data.frame(drcObj$fct$fixed)
  fixedValues <- subset(fixedValues, row.names(fixedValues) <= length(drcObj$paramNames))
  row.names(fixedValues) <- drcObj$paramNames
  names(fixedValues) <- drcObj$name
  fixedValues <- subset(fixedValues, !is.na(fixedValues))	
  keyValues <- rbind(fitValues,fixedValues)
  keyValues <- as.data.table(t(keyValues))
  return(keyValues)
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

drcObject.getDRCFitStats <- function(drcObject) {
  SSE <- sum(residuals(drcObject)^2)
  SST <- sum(treatmentData$RESPONSE-mean(treatmentData$RESPONSE)^2)
  rSquared <- 1-(SSE/SST)
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