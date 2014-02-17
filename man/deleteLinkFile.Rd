\name{deleteLinkFile}
\alias{deleteLinkFile}
\title{Deletes a file that shows up as a link}
\usage{
deleteLinkFile(experiment, testMode = FALSE)
}
\arguments{
  \item{experiment}{A list that is an experiment object;
  needs to have values for id, version, and codeName}

  \item{testMode}{boolean used for testing}
}
\description{
Deletes the file referenced as an annotation file in the
experiment (in stateKind: "report locations").
}
\details{
Will delete all annotation files associated with the
experiment
}

