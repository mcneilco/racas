\name{convertSELExperimentMetaDataToSEL}
\alias{convertSELExperimentMetaDataToSEL}
\title{Converts an Experiment Meta Data data.frame to a pivoted version}
\usage{
  convertSELExperimentMetaDataToSEL(experimentMetaData)
}
\arguments{
  \item{experimentMetaData}{typically a data.frame created
  by \code{\link{createSELExperimentMetaData}}}
}
\value{
  data.frame pivoted version of the experimentMetaData
}
\description{
  Pivotes a data.frame like that returned by
  \code{\link{createSELExperimentMetaData}} to a pivoted
  data.frame
}
\examples{
experimentMetaData <- createSELExperimentMetaData(protocolName = "Cytotox Assay",
                                                experimentName = "2013123_cytotox_bbolt",
                                                assayDate = "2013-01-12")

convertSELExperimentMetaDataToSEL(experimentMetaData)
}

