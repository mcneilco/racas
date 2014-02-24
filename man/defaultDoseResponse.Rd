\name{defaultDoseResponse}
\alias{defaultDoseResponse}
\title{Fit dose response data}
\usage{
defaultDoseResponse(fitData, fitSettingsJSON)
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

#get curveids
curveids <- as.character(query("select curveid from api_curve_params")[[1]])

#fitSettingsJSON
file <- system.file("docs", "default-ec50-fitSettings.json", package = "racas")
fitSettingsJSON <- readChar(file, file.info(file)$size)

#fit the data
system.time(response <- doseResponse(fitSettingsJSON, curveids = curveids))

#Inspect the saved fit session
parsedResponse <- fromJSON(response)
session <- parsedResponse$sessionID
loadSession(session)
head(fitData)
}

