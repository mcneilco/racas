\name{doseResponse}
\alias{doseResponse}
\title{Fit dose response data}
\usage{
doseResponse(fitSettings, curveids = NA, sessionID = NA, fitData = NA)
}
\arguments{
  \item{fitSettings}{a fit settings list object (see
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
Converts a character vector of curveids, character
sessionsID or data.table fitData object to a fitted fitData
object, saves the fitData object to either a new session or
back to the session provided and then returns a json
representation of the fitted fitData object with the
savedSessionID
}
\examples{
#Fit all the curveids in the database:

#get curveids
curveids <- as.character(query("select curveid from api_curve_params")[[1]])

#fitSettings
file <- system.file("docs", "default-ec50-fitSettings.json", package = "racas")
fitSettingsJSON <- readChar(file, file.info(file)$size)
fitSettings <- fromJSON(fitSettingsJSON)
#fit the data
system.time(response <- doseResponse(fitSettings, curveids = curveids))

#Inspect the saved fit session
parsedResponse <- fromJSON(response)
session <- parsedResponse$sessionID
loadSession(session)
head(fitData)
}

