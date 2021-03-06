% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/directDatabaseSave.R
\name{saveAllViaDirectDatabase}
\alias{saveAllViaDirectDatabase}
\title{Save data frames using direct database load}
\usage{
saveAllViaDirectDatabase(analysisGroupData, treatmentGroupData, subjectData,
  appendCodeName = list())
}
\arguments{
\item{analysisGroupData}{A data frame of analysis group values}

\item{treatmentGroupData}{A data frame of treatment group values}

\item{subjectData}{A data frame of subject values}

\item{appendCodeName}{A list with entries "analysisGroupData",
 "treatmentGroupData", "subjectData", each has a vector of valuekinds that
 should have the code name appended to the front}
}
\description{
Saves data frames all in one transaction
}
\details{
each of the data frames must have these columns: unitKind, valueType,
 valueKind, numericValue, publicData, stateType, stateKind, tempStateId,
 tempId, lsType, lsKind. Other optional columns can be found in the source code for
 \code{\link{formatEntityAsTsvAndUpload}}. You can get updating and more
 checks using \code{\link{saveAllViaTsv}}, but it will be slower.
}

