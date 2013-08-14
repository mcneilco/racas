\name{createNewACASExperiment}
\alias{createNewACASExperiment}
\title{Creates and saves a new experiment}
\usage{
  createNewACASExperiment(metaData, protocol,
    lsTransaction, pathToGenericDataFormatExcelFile)
}
\arguments{
  \item{metaData}{a data.frame including columns:
  \describe{ \item{Experiment Name}{the name of the
  experiment} \item{Scientist}{the username of the
  scientist} \item{Notebook}{the name of the notebook, or
  the ELN name and page} \item{Assay Date}{the date the
  assay was completed in format YYYY-MM-DD} \item{Page}{the
  page number (optional)} \item{Project}{the project code
  (optional)} }}

  \item{protocol}{a list that is a protocol object}

  \item{lsTransaction}{an integer that is an lsTransaction
  id}
}
\value{
  a list which is an experiment object
}
\description{
  Currently deprecated, needs to be updated to be used.
  Using a set list of meta data inputs, this creates and
  saves an experiment that can be used as an input for
  analysis groups.
}

