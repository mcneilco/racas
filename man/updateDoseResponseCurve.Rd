\name{updateDoseResponseCurve}
\alias{updateDoseResponseCurve}
\title{Function to save dose response curve data}
\usage{
updateDoseResponseCurve(analysisGroupData, subjectData)
}
\arguments{
  \item{subjectData}{A data frame of the changed flag ids
  (see examples for more on format)

  Format: \itemize{ \item analysisGroupdID The
  analysisGroupID of the flag \item valueKind A string that
  says "Flag" \item valueType A string that says
  "stringValue" \item subjectID The subjedtID of the flag
  \item subjectStateID The subjectStateID of the flag \item
  treatmentGroupID The treatmentGroupdID of the flag \item
  stringValue A string value for the flag (E.G. "User" or
  "Algorithm") }}

  \item{analysisGroupData}{A data frame of the parameters
  to be changed (see examples for more on format)

  Format: \itemize{ \item analysisGroupdID \item batchCode
  \item valueType \item valueKind \item resultUnits \item
  numericValue \item StringValue }}
}
\description{
This function takes in changes to an already saved curve
and updates the flags and fitted parameters
}
\keyword{curve}
\keyword{dose,}
\keyword{response,}
\keyword{save,}
\keyword{update,}

