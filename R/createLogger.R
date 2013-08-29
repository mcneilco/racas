#'Check if protocol exists
#'
#'Checks to see if a protocol exists
#'
#'@param logName  Name for the logger output line within the log file (default: "com.default.logger)
#'@param logFileName name of the log file to write to (default: "output.log")
#'@param logDir name directory to write to (default: racas::applicationSettings$logDir)
#'@param logLevel the level at which to write logs (default: racas::applicationSettings$logLevel, see names(logging::loglevels))
#'@return object of class logger
#'
#'@examples
#' #Creating a logger with logLevel = "WARN"
#' myLogger <- createLogger(logName = "com.mycompany.newprocessor", 
#'                          logFileName = "newprocessor.log", 
#'                          logLevel = "WARN")
#' 
#' #Will write
#' myLogger$error("a warn statement")
#' myLogger$warn("a warn statement")
#' 
#' #Won't write items that are below "WARN" in names(logging::loglevels)
#' myLogger$debug("a debug statement")
#' myLogger$info("a warn statement")
#' 
#' #Creating a logger with logLevel = "DEBUG"
#' myLogger <- createLogger(logName = "com.mycompany.newprocessor", 
#'                          logFileName = "newprocessor.log", 
#'                          logLevel = "DEBUG")
#' 
#' #Now it will log debug and info statements
#' myLogger$debug("a debug statement")
#' myLogger$info("a warn statement")
#' 
createLogger <- function(logName = "com.default.logger", logFileName = "output.log", logDir = racas::applicationSettings$logDir, logLevel = racas::applicationSettings$logLevel) {
  basicConfig(level = logLevel)
  if(is.na(logDir)) {
    logDir <-  getwd()
  }
  logPath <- paste0(logDir,"/",logFileName)
  getLogger(logName)$addHandler(writeToFile, file=logPath, level = logLevel)
  logger <- getLogger(logName)
  setLevel(logLevel, logger)
  
  return(logger)
}
