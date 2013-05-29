\name{PlotCurve}
\alias{PlotCurve}
\title{Curve plotting function}
\usage{
  PlotCurve(curveData, params, fitFunction,
    paramNames = c("ec50", "min", "max", "hill"),
    drawIntercept = "ec50", outFile = NA, ymin = NA,
    logDose = TRUE, ymax = NA, xmin = NA, xmax = NA,
    height = 300, width = 300, showGrid = FALSE,
    showLegend = FALSE, showAxes = TRUE, ...)
}
\arguments{
  \item{curveData}{a data frame with the points with column
  names curveid, dose, response, flag}

  \item{params}{the set of parameters used to enumerate the
  curve}

  \item{outFile}{file to plot image to, if not specified
  then the function plots to graphic device}

  \item{ymin}{specify the ymin axes location}

  \item{ymax}{specify the ymax axes location}

  \item{xmin}{specify the xmin axes location}

  \item{xmax}{specify the xmax axes location}

  \item{logDose}{specify if x axis is in log space}

  \item{height}{height of the plot in pixels}

  \item{width}{width of the plot in pixels}

  \item{showGrid}{adds a grid to the plot}

  \item{showLegend}{shows a legend with curve ids on the
  right hand side of the plot}

  \item{showAxes}{turns axes on or off}
}
\value{
  If outFile is specified, then the function prints an
  image to the out file, if outFile is not specified, then
  then an image is plotted to a graphics device
}
\description{
  This function takes in a set of data points, curve
  parameters, and an equation and plots the data
}
\examples{
LL4 <- 'min + (max - min)/((1 + exp(-hill * (log(x/ec50))))^1)'
data(curveData)
params <- curveData$parameters
curveData <- curveData$points
PlotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE)

data(kiData)
params <- kiData$parameters
points <- kiData$points
paramNames <- c("Top", "Bottom", "HotNM", "HotKDNM", "Log10Ki")
KiFCT <- 'Bottom + (Top-Bottom)/(1+10^(x-log10((10^Log10Ki)*(1+HotNM/HotKDNM))))'
PlotCurve(points, params, KiFCT, paramNames, drawIntercept= "Log10Ki", outFile = NA, ymin = NA, logDose = FALSE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE)
}
\keyword{curve}
\keyword{plot,}
\keyword{render,}

