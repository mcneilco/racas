\name{getPreferredId}
\alias{getPreferredId}
\title{Gets preferred ids}
\usage{
getPreferredId(batchIds, preferredIdService = NULL, testMode = FALSE)
}
\arguments{
  \item{batchIds}{a character vector of batch codes}

  \item{preferredIdService}{the url of the preferred id
  service. Defaults to
  \code{racas::applicationSettings$preferredBatchIdService}}

  \item{testMode}{deprecated, was used for a test mode}
}
\value{
a list of pairs of requested IDs and preferred IDs. On
error, empty list
}
\description{
Entered batch codes (ids) are checked against a preferred
id service
}
\keyword{batchCode,}
\keyword{preferred}

