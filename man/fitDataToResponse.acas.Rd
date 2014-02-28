\name{fitDataToResponse.acas}
\alias{fitDataToResponse.acas}
\title{fitData object to json response}
\usage{
fitDataToResponse.acas(fitData, ...)
}
\arguments{
  \item{a}{fitData object}

  \item{...}{addition arguments to be passed to
  \code{\link{toJSON}}}
}
\value{
A json object of the fitData and any other objects coerced
to json by ... \code{\link{toJSON}}
}
\description{
Converts a fitData object to a json response to return to
the GUI
}
\examples{
#Load and example fitData object
data("example-ec50-fitData-fitted")
#FitData object plus the "cars" data to a json string
fitDataToResponse.acas(fitData, cars)
}

