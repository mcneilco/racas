\name{deleteLinkFile}
\alias{deleteLinkFile}
\title{Deletes a file that shows up as a link}
\usage{
  deleteLinkFile(experiment)
}
\arguments{
  \item{experiment}{A list that is an experiment object,
  needs to have values for id, verison, and codeName}
}
\description{
  Deletes the file referenced as an annotation file in the
  experiment (in stateKind: "report locations").
}
\details{
  Will delete more than one file, but not tested
}

