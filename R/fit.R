require(drc)
fit <- function(dataSet, formula = RESPONSE ~ DOSE, drcFunction = LL.4, paramNames = c("SLOPE","MIN","MAX","EC50"), fixedValues = eval(formals(drcFunction)$fixed)) {
  fct <- drcFunction(names=paramNames,fixed=fixedValues)
  drcObj <- list("failed")
  tryCatch({
    drcObj <- drm(formula = formula, data = dataSet, fct = fct)
  },
           error = function(ex) {
             cat(ex$message,"\n")
           })
  return(drcObj)
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