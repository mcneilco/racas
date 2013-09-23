\name{checkExistence}
\alias{checkExistence}
\title{Check for entity existence}
\usage{
  checkExistence(x,
    type = c("protocolName", "experimentName", "protocolCodeName", "experimentCodeName"))
}
\arguments{
  \item{x}{entity names or code names of the same type}

  \item{type}{entity type of x (protocolName,
  experimentCodeName...etc.)}
}
\value{
  boolean list with names that match x
}
\description{
  This function checks to see if a given entity or list of
  entities exists
}
\examples{
checkExistence("ADME_Human Liver Microsome Stability", "protocolName")
checkExistence(c("ADME_Human Liver Microsome Stability","some other protocol name"), "protocolName")
checkExistence("PROT-00000001", "protocolCodeName")
checkExistence("EXPT-00000001", "experimentCodeName")
}
\keyword{check,}
\keyword{existence,}
\keyword{exists}

