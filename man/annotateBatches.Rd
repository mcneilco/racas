\name{annotateBatches}
\alias{annotateBatches}
\title{Saves batch annotations}
\usage{
  annotateBatches(batchCodeList, recordedBy, experiment,
    lsTransaction, reportFileSummary = NULL,
    reportFilePath = NULL, url = NULL,
    annotationType = NULL)
}
\arguments{
  \item{batchCodeList}{A list or vector of batch codes}

  \item{recordedBy}{The username of the person recording
  the data}

  \item{experiment}{A list that is an experiment object,
  needs to have values for id, verison, and codeName}

  \item{lsTransaction}{A number that is a transaction id}

  \item{summary}{A description of the file or url
  annotation}

  \item{reportFilePath}{The path to the report file (may be
  relative)}

  \item{url}{A url (e.g. http://google.com)}

  \item{annotationType}{The type of the annotation, used by
  some custom services}
}
\value{
  NULL
}
\description{
  Saves an analysis group with states and values holding a
  file or url. Each batchCode has a separate state with the
  annotation.
}
\details{
  Must have either a reportFilePath or a url, but having
  both will result in the url being ignored.
}

