\name{meltBatchCodes}
\alias{meltBatchCodes}
\title{Turns a batchCode column into rows in a long format}
\usage{
  meltBatchCodes(entityData, batchCodeStateIndices,
    replacedFakeBatchCode = NULL)
}
\arguments{
  \item{entityData}{a data frame with data}

  \item{batchCodeStateIndices}{a numeric vector of indices
  in the stateGroupIndexColumn which should have batchCodes
  melted}

  \item{replacedFakeBatchCode}{a character vector of fake
  batch id's that were replaced, marking invalid batch
  codes}
}
\value{
  A data frame with rows for all code values
}
\description{
  Turns a batchCode column into rows in a long format
}
\details{
  Does not work with data.table
}

