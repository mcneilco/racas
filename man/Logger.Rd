\name{logger}
\alias{logger}
\title{Creates a new logger object or returns the "bubbleLogger"}
\usage{
logger(bubble = TRUE, envir = parent.frame(), ...)
}
\arguments{
  \item{bubble}{Name for the logger output line within the
  log file (default: "com.default.logger)}

  \item{envir}{name of the log file to write to (default:
  "acas.log")}

  \item{...}{further arguments to be passed to
  \code{\link{createLogger}}}
}
\value{
object of class \code{\link{Logger}}
}
\description{
Optionally creates a new logger or returns a logger stored
in the racas namespace. The default call 'logger()' will
compare the field racas::bubbleLogger$envir with the
current list of frames in the call stack. If the
environment in the bubbleLogger matches one of the
environments in the current call stack, then the
bubbleLogger is returned.
}
\examples{
#From a fres

#Will write
myLogger$error("a warn statement")
myLogger$warn("a warn statement")

#Won't write items that are below "WARN" in names(logging::loglevels)
myLogger$debug("a debug statement")
myLogger$info("a warn statement")

#Creating a logger with logLevel = "DEBUG"
myLogger <- createLogger(logName = "com.mycompany.newprocessor",
                         logFileName = "newprocessor.log",
                         logLevel = "DEBUG")

#Now it will log debug and info statements
myLogger$debug("a debug statement")
myLogger$info("a warn statement")
}

