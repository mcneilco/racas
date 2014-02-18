\name{linkOldContainers}
\alias{linkOldContainers}
\title{Links new subjects to old containers}
\usage{
linkOldContainers(entityData, stateGroups, labelPrefix = NULL,
  stateGroupIndices = NULL, testModeData = NULL)
}
\arguments{
  \item{entityData}{data.frame: must have columns
  stringValue, stateGroupIndices, numericValue}

  \item{stateGroups}{a list of state (and label) groups}

  \item{labelPrefix}{a string, often the experiment name.
  If NULL (default), only exact matches are checked}

  \item{stateGroupIndices}{numeric vector, use to list
  indices rather than having them automatically be found}

  \item{testModeData}{a data frame of test data, testMode
  happends when not null}
}
\value{
a list of two data frames, one with new data and one with
old- the old only has subjectID and containerID
}
\description{
Tests for both containers created by other experiments and
for ones created by an experiment of the same name
(reloads)
}
\details{
If there are both prefixed and unprefixed labels that
match, the prefixed labels will have precedence
}

