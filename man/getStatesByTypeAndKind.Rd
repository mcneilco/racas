\name{getStatesByTypeAndKind}
\alias{getStatesByTypeAndKind}
\title{Gets states from an entity}
\usage{
getStatesByTypeAndKind(entity, typeAndKind)
}
\arguments{
  \item{entity}{A list that represents a single ACAS
  entity}

  \item{typeAndKind}{A string value that follows the form
  "lsType_lsKind"}
}
\value{
A list of lsStates
}
\description{
Allows you to select states of a specific lsTypeAndKind
from an entity (protocol, experiment, analysis group,
treatment group, subject, container, or interaction)
}

