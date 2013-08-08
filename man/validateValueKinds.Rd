\name{validateValueKinds}
\alias{validateValueKinds}
\title{Checks that column headers are valid valueKinds (or creates them if they are new)}
\usage{
  validateValueKinds(neededValueKinds,
    neededValueKindTypes, serverPath, dryRun)
}
\arguments{
  \item{neededValueKinds}{A character vector listed column
  headers}

  \item{neededValueKindTypes}{A character vector of the
  valueTypes of the above kinds}

  \item{serverPath}{The path to the acas server}

  \item{dryRun}{A boolean marking if changes should be made
  or only checked}
}
\value{
  NULL
}
\description{
  Checks that column headers are valid valueKinds (or
  creates them if they are new)
}

