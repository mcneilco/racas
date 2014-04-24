\name{meltConcentrations}
\alias{meltConcentrations}
\title{Turns concentration columns into rows in a long format}
\usage{
meltConcentrations(entityData, entityKind = "treatmentGroup")
}
\arguments{
  \item{entityData}{a data frame with data, must include
  rows "concentration" and "concentrationUnit"}

  \item{entityKind}{the current acas entityKind (in
  racas::acasEntityHierarchyCamel)}
}
\description{
Turns concentration columns into rows in a long format
}

