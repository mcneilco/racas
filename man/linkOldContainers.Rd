\name{linkOldContainers}
\alias{linkOldContainers}
\title{Links new subjects to old containers}
\usage{
  linkOldContainers(entityData, stateGroups,
    labelPrefix = NULL, stateGroupIndices = NULL)
}
\arguments{
  \item{entityData}{data.frame: must have columns
  stringValue, stateGroupIndices, numericValue}

  \item{stateGroups}{a list of state (and label) groups}

  \item{labelPrefix}{a string, often the experiment name}

  \item{stateGroupIndices}{numeric vector, use to list
  indices rather than having them automatically be found}
}
\value{
  a list of two data frames, one with new data and one with
  old
}
\description{
  Links new subjects to old containers
}

