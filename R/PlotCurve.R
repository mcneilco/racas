#' Curve plotting function
#'
#' This function takes in a set of data points, curve parameters, and an equation and plots the data
#'
#' @param curveData a data frame with the points with column names curveid, dose, response, flag
#' @param params the set of parameters used to enumerate the curve
#' @param outFile file to plot image to, if not specified then the function plots to graphic device
#' @param ymin specify the ymin axes location
#' @param ymax specify the ymax axes location
#' @param xmin specify the xmin axes location
#' @param xmax specify the xmax axes location
#' @param logDose specify if x axis is in log space
#' @param logResponse specify if y axis is in log space
#' @param height height of the plot in pixels
#' @param width width of the plot in pixels
#' @param showGrid adds a grid to the plot
#' @param showLegend shows a legend with curve ids on the right hand side of the plot
#' @param showAxes turns axes on or off
#' @param plotMeans will plot the mean Y given a given X
#' @param drawStdDevs will plot the Standard Deviations for each dose response combination
#' @param connectPoints will draw a line between the means of each mean X - Y
#' @param drawCurve turn the curve drawing on and off
#' @param drawCurve turn the curve drawing on and off
#' 
#' @return If  outFile is specified, then the function prints an image to the out file, if outFile is not specified, then then an image is plotted to a graphics device
#' @keywords plot, render, curve
#' @export
#' @examples
#' LL4 <- 'min + (max - min)/((1 + exp(-hill * (log(x/ec50))))^1)'
#' data(curveData)
#' params <- curveData$parameters
#' curveData <- curveData$points
#' PlotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE)
#' PlotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE)
#' PlotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE, connectPoints = TRUE, drawCurve = FALSE)
#' PlotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = TRUE)
#' 
#' #Ki Data (using raw data)
#' data(kiData)
#' params <- kiData$parameters
#' points <- kiData$points
#' paramNames <- c("Top", "Bottom", "HotNM", "HotKDNM", "Log10Ki")
#' KiFCT <- 'Bottom + (Top-Bottom)/(1+10^(x-log10((10^Log10Ki)*(1+HotNM/HotKDNM))))'
#' PlotCurve(points, params, KiFCT, paramNames, drawIntercept= "Log10Ki", outFile = NA, ymin = NA, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE)
#'
#' #PK Curves
#' #PO
#' data(poPKCurveData)
#' params <- poPKCurveData$parameters
#' curveData <- poPKCurveData$points
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE, addShapes = TRUE)
#' 
#' #IV
#' data(ivPKCurveData)
#' params <- ivPKCurveData$parameters
#' curveData <- ivPKCurveData$points
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE, addShapes = TRUE)
#' 
#' #PO IV
#' data(poIVPKCurveData)
#' params <- poIVPKCurveData$parameters
#' curveData <- poIVPKCurveData$points
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, addShapes = TRUE, drawStdDevs = TRUE)
PlotCurve <-  function(curveData, params, fitFunction, paramNames = c("ec50", "min", "max", "hill"), drawIntercept = "ec50", outFile = NA, ymin = NA, logDose = FALSE, logResponse = FALSE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, drawCurve = TRUE, connectPoints = FALSE, plotMeans = FALSE, drawStdDevs = FALSE, addShapes = FALSE, labelAxes = FALSE, ...) {

  #Check if paramNames match params column headers
  if(!is.na(paramNames) && drawCurve == TRUE) {
    if(any(is.na(match(paramNames, names(params))))) {
      stop("paramNames not found in names of params")
    }
  } else {
    drawCurve <- FALSE
    drawIntercept <- NA
  }
  
  #Assign Colors
  plotColors <- rep(c("black","red","green", "blue", "orange","purple", "cyan"),100, replace = TRUE)
  params$color <- plotColors[1:nrow(params)]
  curveData$color <- plotColors[match(curveData$curveid,params$curveid)] 
  if(addShapes) {
    params$pch <- 1:nrow(params)
    curveData$pch <- match(curveData$curveid,params$curveid)
  }
  #Determine axes ranges
  maxDose <- max(curveData$dose)
  minDose <- min(curveData$dose)
  maxResponse <- max(curveData$response)
  minResponse <- min(curveData$response)
  responseRange <- abs(maxResponse-minResponse)
  doseRange <- abs(maxDose-minDose)
  if(is.na(ymin)) {
    if(logResponse) {
      ymin <- 10^(log10(min(curveData$response[curveData$response>0])) - 0.5)
      curveData$response[curveData$response <= 0] <- ymin
    } else {
      ymin <- (minResponse - 0.01*responseRange)
    } 
  }
  if(is.na(ymax)) {
    if(logResponse) {
      ymax <- 10^(log10(maxResponse) + 0.5)
    } else {
      ymax <- (maxResponse + 0.01*responseRange)
    } 
  }
  if(is.na(xmax)) {
    if(logDose) {
      xmax <- 10^(log10(maxDose) + 0.5)
    } else {
      xmax <- maxDose + abs(0.01 * doseRange)
    }  
  }
  if(is.na(xmin)) {
    if(logDose) {
      xmin <- 10^(log10(min(curveData$dose[curveData$response>0])) - 0.5)
    } else {
      xmin <- minDose - abs(0.01 * doseRange)
    }
  }
  #If plotting log data then xrange vals cannot be negative
  if(logDose) {
    if(!is.na(xmin)) {
      if(xmin <= 0) {
        xmin = 0.001
      }
    }
    if(!is.na(xmax) && !is.na(xmin)) {
      if(xmax <= xmin) {
        xmin = NA
        xmax = NA
      }
    }
  }
  if(logResponse) {
    if(!is.na(ymin)) {
      if(ymin <= 0) {
        ymin = 0.001
      }
    }
    if(!is.na(xmax) && !is.na(ymin)) {
      if(ymax <= ymin) {
        ymin = NA
        ymax = NA
      }
    }
  }
  
  xrn <- c(xmin, xmax)
  yrn <- c(ymin, ymax)
  
  ##Seperate Flagged and good points for plotting different point shapes..etc.
  flaggedPoints <- subset(curveData, curveData$flag)
  goodPoints <- subset(curveData, !curveData$flag)
  
  ##Caldulate Means and SDs
  sds <- aggregate(goodPoints$response,list(dose=goodPoints$dose,curveid=goodPoints$curveid, color = goodPoints$color), sd)
  names(sds)[ncol(sds)] <- "sd"
  means <- aggregate(goodPoints$response,list(dose=goodPoints$dose,curveid=goodPoints$curveid, color = goodPoints$color), mean)
  names(means)[ncol(means)] <- "mean"
  
  ###Begin Drawing the Plot
  if(!is.na(outFile)) {
    png(file = outFile, height = height, width = width)
  }
  
  #showLegend is the signal to put curve ids in the legend, for this push the image to be bigger on right and then put legend
  defaultMargins=c(0.1,1,0.3,0.8)
  #par(mar=c(2.1,3,0.1,0.1)) #Set margin to east to fit legend
  margins <- defaultMargins
  if(labelAxes) {
    margins[c(1,2)] <- defaultMargins[c(1,2)] + 4
  } else {
    if(showAxes) {
      margins[c(1,2)] <- defaultMargins[c(1,2)] + 2
    }
  }
  par(mar = margins)
  
  plotLog <- paste0(ifelse(logDose, "x", ""),ifelse(logResponse, "y", ""))
  #First Plot Good Points so we that can see the flagged points if they are overlayed
  if(!plotMeans) {
    #TODO: what if plotMeans but also plotPoints? deal with that later
    plot(goodPoints$dose, goodPoints$response, log = plotLog, col = goodPoints$color, pch = goodPoints$pch, xlim = xrn, ylim = yrn, xaxt = "n", family = "sans", axes = FALSE, ylab = "", xlab = "")
  } else {
    plot(means$dose, means$mean, log = plotLog, col = means$color, xlim = xrn, ylim = yrn, xaxt = "n", family = "sans", axes = FALSE, ylab = "", xlab = "")
  }
  if(drawStdDevs) {
    plotCI(x=goodPoints$dose,y=goodPoints$response, uiw=goodPoints$standardDeviation, col = goodPoints$color, add=TRUE,err="y",pch=NA)
  }
  if(connectPoints) {
    cids <- unique(means$curveid)
    for(c in 1:length(cids)) {
      cid <- cids[c]
      lineData <- subset(means, means$curveid == cid)
      lines(x = lineData$dose, y = lineData$mean, col = lineData$color, pch = 4, lty = 'dotted')
    }
  }
  
  #If grid, then add grid
  if(showGrid) {
    grid(lwd = 1.7)
  }
  #Now Plot Flagged Points
  points(x = flaggedPoints$dose, y = flaggedPoints$response, col = flaggedPoints$color, pch = 4)
  
  #Draw Error Bars and Means
  #plotCI(x=means$dose,y=means$MEAN,uiw=sds$SD,add=TRUE,err="y",pch="-")
  getDrawValues <- function(params) {
    reportedValueColumns <- match(paramNames, names(params))
    reportedValueColumns <- reportedValueColumns[!is.na(reportedValueColumns)]
    reportedValues <- params[,reportedValueColumns]
    reportedValues <- reportedValues[sapply(reportedValues, function(x) !any(is.na(x)))] 
    
    tmp <- data.frame(matrix(nrow=1, ncol=length(paramNames))) 
    names(tmp) <- paramNames
    tmp[1,match(names(reportedValues), paramNames)] <- reportedValues
    
    fittedColumnNames <- paste0("fitted",paramNames)
    fittedValueColumns <- match(fittedColumnNames,names(params))
    fittedValueColumns <- fittedValueColumns[!is.na(fittedValueColumns)]
    
    if(length(fittedValueColumns) > 0) {
      fittedValues <-  params[,fittedValueColumns]
      fittedValues <- fittedValues[sapply(fittedValues, function(x) !any(is.na(x)))] 
      tmp[1,match(names(fittedValues),fittedColumnNames)] <- fittedValues
    }
    return(tmp)
  }
  #Curve Drawing Function
  drawCurveID <- function(cid) {
    drawValues <- getDrawValues(params = params[cid,])
    curveID <- params$curveid[cid]
    curveParams <- subset(params, params$curveid == curveID)
    for(i in 1:ncol(drawValues)) {
      assign(names(drawValues)[i], drawValues[,i])
    }
    fct <- eval(parse(text=paste0('function(x) ', fitFunction)))
    curve(fct, from = xrn[1], to = xrn[2], add = TRUE, col = curveParams$color)	
  }
  #Actually Draw Curves
  if(drawCurve) {
    null <- lapply(1:length(params$curveid),drawCurveID)
  }
  ##DO axes and Grid
  box()
  if(showAxes) {
    if(logDose) {
      xTickRange <- par("xaxp")[1:2]
      log10Range <- log10(abs(xTickRange[2]/xTickRange[1]))+1
      major.ticks <- unlist(lapply(1:log10Range,ten <- function(x) {xTickRange[1]*10^(x-1)}))
      axis(1,at=major.ticks,labels=formatC(major.ticks),tcl=par("tcl")*1.8)
      intervals <- c(major.ticks/10,major.ticks[-1],major.ticks*10)
      minor.ticks <- 1:9 * rep(intervals / 10, each = 9)
      axis(1, at= minor.ticks, tcl = -0.5, labels = FALSE, tcl=par("tcl")*0.7) 
    } else {
      axis(1)
    }
    if(logResponse) {
      yTickRange <- par("yaxp")[1:2]
      log10Range <- log10(abs(yTickRange[2]/yTickRange[1]))+1
      major.ticks <- unlist(lapply(1:log10Range,ten <- function(x) {yTickRange[1]*10^(x-1)}))
      axis(2,at=major.ticks,labels=formatC(major.ticks),tcl=par("tcl")*1.8,,las=1)
      intervals <- c(major.ticks/10,major.ticks[-1],major.ticks*10)
      minor.ticks <- 1:9 * rep(intervals / 10, each = 9)
      axis(2, at= minor.ticks, tcl = -0.5, labels = FALSE, tcl=par("tcl")*0.7) 
    } else {
      axis(2)
    }
  }
  ##If only one curve then draw ac50 lines
  #Get coordinates to draw lines through curve at AC50
  #Vertical
  if(!is.na(drawIntercept)) {
    if(nrow(params) == 1) {
      drawValues <- getDrawValues(params = params[1,])
      for(i in 1:ncol(drawValues)) {
        assign(names(drawValues)[i], drawValues[,i])
      }
      fct <- eval(parse(text=paste0('function(x) ', fitFunction)))
      curveIntercept <- fct(params[,drawIntercept])
      ylin <- c()
      ylin$x <- c(get(drawIntercept), get(drawIntercept))
      ylin$y <- c(par("usr")[3],curveIntercept)
      #Horizontal
      xlin <- c()
      if(logDose) {
        xlin$x <- c(0.0000000000000001,get(drawIntercept))
      } else {
        xlin$x <- c(par("usr")[1],get(drawIntercept))
      }
      xlin$y <- c(curveIntercept,curveIntercept)
      #Draw AC50 Lines
      lines(ylin,lwd=0.7,col="red")
      lines(xlin,lwd=0.7,col="red")
    }
  }
  #Draw Legend if specified
  if(showLegend) {
    #par(xpd=TRUE) # allows legends to be printed outside plot area
    #legendYPosition <- 10 ^ par("usr")[2]
    #legendXPosition <- par("usr")[4]
    legendText <- params$curveid
    legendTextColor <- params$color
    legendPCH <- params$pch
    legendLineWidth <- 1
    legend("topright",legend = legendText, col = legendTextColor, lty = legendLineWidth, pch = legendPCH)
  }
  if(labelAxes) {
    xlabel <- paste0(curveData$doseType[1], " (",curveData$doseUnits[1],")")
    ylabel <- paste0(curveData$responseType[1], " (",curveData$responseUnits[1],")")
    title(xlab = xlabel, ylab = ylabel)
  }
  if(!is.na(outFile)) {
    dev.off()
  }
}


