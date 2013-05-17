\name{createHtmlSummary}
\alias{createHtmlSummary}
\title{Create an HTML summary}
\usage{
  createHtmlSummary(hasError, errorList, hasWarning,
    warningList, summaryInfo, dryRun)
}
\arguments{
  \item{hasError}{A boolean marking if the function
  returned an error}

  \item{errorList}{A list of errors}

  \item{hasWarning}{A boolean marking if the function had
  warnings}

  \item{warningList}{A list of warnings}

  \item{summaryInfo}{A list of information to return to the
  user}

  \item{dryRun}{A boolean marking if information should be
  saved to the server}
}
\description{
  Takes a list of summary information and turns it into
  HTML that can be displayed in a web interface.
}

