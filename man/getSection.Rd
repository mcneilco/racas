\name{getSection}
\alias{getSection}
\title{Get dataframe sections}
\usage{
getSection(genericDataFileDataFrame, lookFor, transpose = FALSE)
}
\arguments{
  \item{genericDataFileDataFrame}{A data frame of lines}

  \item{lookFor}{A string identifier to user as regext for
  the line before the start of the seciton}

  \item{transpose}{a boolean to set if the data should be
  transposed}
}
\value{
A dataframe of the of section in the generic excel file

Intended for data.frames that have been read directly from
a csv or xls and have sections with names
}
\description{
Gets a section of a data.frame that is divided by NA's or
""
}

