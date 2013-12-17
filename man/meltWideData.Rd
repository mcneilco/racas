\name{meltWideData}
\alias{meltWideData}
\title{Melt our data sets}
\usage{
  meltWideData(wideData, resultTypes, splitColumn = NULL,
    splitFunction = NULL)
}
\arguments{
  \item{wideData}{a data.frame with columns of the same
  valueKind}

  \item{splitColumn}{the column name in wideData to split
  groups on}

  \item{splitFunction}{a function to use in secondary
  splitting of rows}

  \item{resultTypes}{a data.frame with columns: \describe{
  \item{DataColumn}{the string that will match column names
  of wideData} \item{Type}{the valueKind of each row}
  \item{Units}{the unit of that row} \item{Conc}{the tested
  concentration of that row} \item{ConcUnits}{the tested
  concenration unit of that row} }}
}
\description{
  melts our specific format
}
\details{
  resultTypes should not have a factor in DataColumn- order
  breaks
}

