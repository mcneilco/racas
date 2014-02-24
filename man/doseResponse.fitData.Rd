\name{doseResponse.fitData}
\alias{doseResponse.fitData}
\title{Fit dose response data}
\usage{
doseResponse.fitData(fitData, fitSettingsJSON)
}
\arguments{
  \item{fitSettingsJSON}{a fit settings json object (see
  examples)}

  \item{curveids}{a character list of curveids}

  \item{sessionID}{a path to a curve fit session}

  \item{fitData}{a fidData object to refit}
}
\value{
A json object with the sessionid among a number of html
divs to display back to the user
}
\description{
Converts a fitData object to a fitted fitData object
}
\examples{
#Fit all the curveids in the database:

#get fitData
data("example-ec50-fitData")

#fitSettingsJSON
file <- system.file("docs", "default-ec50-fitSettings.json", package = "racas")
fitSettingsJSON <- readChar(file, file.info(file)$size)

#fit the data
fitData <- doseResponse.fitData(fitData, fitSettingsJSON)
}

