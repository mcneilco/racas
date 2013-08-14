\name{saveAnalysisResults}
\alias{saveAnalysisResults}
\title{Save the HTML summary into the experiment}
\usage{
  saveAnalysisResults(experiment, hasError, htmlSummary,
    lsTransaction = NULL)
}
\arguments{
  \item{experiment}{An experiment list of lists}

  \item{hasError}{A boolean in the analysis had an error}

  \item{htmlSummary}{A string that is html}

  \item{lsTransaction}{The id of the transaction}
}
\description{
  Saves a summary to the given experiment
}

