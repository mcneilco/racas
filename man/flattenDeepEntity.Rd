\name{flattenDeepEntity}
\alias{flattenDeepEntity}
\title{Flattens Nested ACAS Entities}
\usage{
flattenDeepEntity(entity, desiredAcasCategory,
  currentAcasCategory = "experiment", includeFromState = c("id", "lsType",
  "lsKind"), includeFromEntity = c("id"))
}
\arguments{
  \item{entity}{an ACAS entity such as a protocol or
  subject}

  \item{desiredAcasCategory}{acasCategory where the desired
  values are stored}

  \item{currentAcasCategory}{acasCategory of the entity
  provided}

  \item{includeFromState}{a character vector of column
  names to include from the state}

  \item{includeFromEntity}{a character vector of column
  names to include from the state}
}
\description{
Gets values within nested ACAS entities
}
\details{
\code{flattenDeepEntity} pulls values out of nested
objects. This can be used on any ACAS object that has
lsStates that have lsValues. If no information is needed
from the state or entity, \code{includeFromState} and
\code{includeFromEntity}, respectively, can be set to an
empty list, \code{c()}. columns in \code{includeFromState}
will have "state" prepended and the first letter
capitalized, while columns in \code{includeFromEntity} will
have \code{acasCategory} prepended and the first letter
capitalized. The list of ACAS categories can be found in
\code{racas::acasEntityHierarchy}
(\link{acasEntityHierarchy})
}
\examples{
\dontrun{
experiment <- getExperimentByCodeName("EXPT-00012398", include = "fullobject")
x <- flattenDeepEntity(experiment, "subject", "experiment")
}
}

