\name{api_doseResponse.experiment}
\alias{api_doseResponse.experiment}
\title{Fits experiment dose response data}
\usage{
api_doseResponse.experiment(simpleFitSettings, recordedBy, experimentCode,
  testMode = NULL)
}
\arguments{
  \item{simpleFitSettings}{list of fit settings (see
  details)}

  \item{recordedBy}{character user that curve data will be
  saved as}

  \item{experimentCode}{character saved experiment code
  that contains dose response data}

  \item{testMode}{logical unimplemented}
}
\value{
a list of the entities returned from the server
}
\description{
This function retrieves the dose response data for a given
experiment code, refits it using the simple settings and
then saves the data back to the database
}
\examples{
file <- system.file("docs", "example-ec50-simple-fitSettings.json", package = "racas" )
simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
recordedBy <- "bbolt"
experimentCode <- "EXPT-00000441"
api_doseResponse.experiment(simpleFitSettings, recordedBy, experimentCode)

#Loading fake data first
# requires 1. that a protocol named "Target Y binding") be saved first (see \\code{\\link{api_createProtocol}})
#          2. have a valid recordedBy (or that acas is set to client.require.login=false)

file <- system.file("docs", "example-ec50-simple-fitSettings.json", package = "racas" )
simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
experimentCode <- loadDoseResponseTestData()
recordedBy <- "bbolt"
api_doseResponse.experiment(simpleFitSettings, recordedBy, experimentCode)
}
\keyword{dose,}
\keyword{experimet}
\keyword{fit,}
\keyword{model,}
\keyword{response,}

