\name{meltBatchCodes}
\alias{meltBatchCodes}
\title{Turns a batchCode column into rows in a long format}
\usage{
  meltBatchCodes(entityData, batchCodeStateIndices)
}
\arguments{
  \item{entityData}{a data frame with data}

  \item{batchCodeStateIndices}{a numeric vector of indices
  in the stateGroupIndexColumn which should have batchCodes
  melted

  Does not work with data.table}
}
\description{
  Turns a batchCode column into rows in a long format
}

