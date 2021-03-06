% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/dose_response.R
\name{simple_to_advanced_fit_settings}
\alias{simple_to_advanced_fit_settings}
\title{Convert a simple dose response request to an advanced dose response request}
\usage{
simple_to_advanced_fit_settings(defaultFitSettings, simpleSettings,
  update_function, updateFlags = NULL)
}
\arguments{
\item{simpleSettings}{a list object of simple fit settings}

\item{renderingHint}{a character string to identify the dose response model to fit}
}
\value{
an advanced fit settings list object
}
\description{
Reads the default fit settings for the given model hint and updates it based on the simple request
}
\examples{
file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas")
simpleSettingsJSON <- readChar(file, file.info(file)$size)
simpleSettings <- fromJSON(simpleSettingsJSON)
simple_to_advanced_fit_settings(simpleSettings)
}

