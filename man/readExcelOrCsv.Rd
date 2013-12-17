\name{readExcelOrCsv}
\alias{readExcelOrCsv}
\title{Reading of Excel or CSV}
\usage{
  readExcelOrCsv(filePath)
}
\arguments{
  \item{filePath}{The path the the file to read}
}
\value{
  a data.frame of the file
}
\description{
  Read in xls, xlsx, or csv files and output a data frame
  of the information
}
\details{
  Right now, this turns dates into
  "A_date_was_in_Excel_Date_format" and does not treat
  headers as column names. in fileRead.R
}

