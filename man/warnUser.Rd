% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/errorLogging.R
\name{warnUser}
\alias{userWarning}
\alias{warnUser}
\title{Warning tracking}
\usage{
warnUser(message)
}
\arguments{
\item{message}{The warning message that the user should see}
}
\description{
Declares a warning to be of class "userWarning", to distinguish between warnings we programmed
into the system (e.g. "Found extra meta data") and errors R gives (e.g. "argument has length >1")
}
\details{
Adds the class "userWarning" to the warning object.

All helpful errors should be thrown using \code{warnUser}. Any warning
thrown using \code{warning} will be treated as an internal warning by the simple
experiment loader.

When using this function, it is important to give it a single string as a message.
This means using paste. While \code{warning("text ", variable, " text")} is okay syntax,
you will get an error if you try \code{warnUser("text ", variable, " text")}. Use paste0
instead: \code{warnUser(paste0("text ", variable, " text"))} (note that this syntax is also
perfectly acceptable inside \code{warning})
}

