\name{createLogger}
\alias{createLogger}
\title{Check if protocol exists}
\usage{
  createLogger(logName = "com.default.logger",
    logFileName = "output.log",
    logDir = racas::applicationSettings$logDir,
    logLevel = racas::applicationSettings$logLevel)
}
\arguments{
  \item{logName}{Name for the logger output line within the
  log file (default: "com.default.logger)}

  \item{logFileName}{name of the log file to write to
  (default: "output.log")}

  \item{logDir}{name directory to write to (default:
  racas::applicationSettings$logDir)}

  \item{logLevel}{the level at which to write logs
  (default: racas::applicationSettings$logLevel, see
  names(logging::loglevels))}
}
\value{
  object of class logger
}
\description{
  Checks to see if a protocol exists
}
\examples{
#Creating a logger with logLevel = "WARN"
myLogger <- createLogger(logName = "com.mycompany.newprocessor",
                         logFileName = "newprocessor.log",
                         logLevel = "WARN")

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

