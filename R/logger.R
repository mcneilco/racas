
Logger <- setRefClass( 
  Class="racasLogger", 
  fields=list( 
    envir = "environment"
  ), 
  contains = "Logger", 
  methods=list( 
    
  ) 
)

#'Creates a new logger
#'
#'Creates a new logger
#'
#'@param logName  Name for the logger output line within the log file (default: "com.default.logger)
#'@param logFileName name of the log file to write to (default: "acas.log")
#'@param logDir name directory to write to (default: racas::applicationSettings$server.log.path)
#'@param logLevel the level at which to write logs (default: racas::applicationSettings$server.log.level, see names(logging::loglevels))
#'@param envir an environment to attach to the object
#'@return object of class logger
#'
#'@examples
#'
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
createLogger <- function(logName = "com.default.logger", logFileName = "racas.log", logDir = racas:::applicationSettings$server.log.path, logLevel = racas:::applicationSettings$server.log.level, envir = environment(), logToConsole = TRUE, ...) {
  if(is.null(logLevel)) logLevel <- "INFO"
  logReset()
  logger <- getLogger(logName)
  setLevel(logLevel, logger)
  
  if(is.na(logDir)) {
    logDir <-  getwd()
  }
  logPath <- paste0(logDir,"/",logFileName)
  logger$addHandler(writeToFile, file=logPath, level = logLevel)
  if(logToConsole) {
    logger$addHandler("basic.stdout", writeToConsole, level = logLevel)
  }
  return(logger)
}

racasLogger <- Logger$new()

#'Creates a new logger object or returns the "racasLogger"
#'
#'Optionally creates a new logger or returns a logger stored in the racas namespace.
#'The default call 'logger()' will compare the field racas::racasLogger$envir with the current list of frames in the call stack.
#'If the environment in the racasLogger matches one of the environments in the current call stack, then the racasLogger is returned.
#'
#'@param racas  Name for the logger output line within the log file (default: "com.default.logger)
#'@param envir name of the log file to write to (default: "acas.log")
#'@param ... further arguments to be passed to \code{\link{createLogger}}
#'@return object of class \code{\link{Logger}}
#'
#'@examples
#' #From a fres
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
logger <- function(racas = TRUE, envir = parent.frame(), ...) {
  if(!racas) {
    return(createLogger(envir = envir, ...))
  } else {
    allEnvironments <- as.list(sys.frames())
    racasLoggerObj <- Filter( function(x) 'Logger' %in% class( get(x) ), ls(pattern = "racasLogger", envir = as.environment("package:racas")) )    
    if(length(racasLoggerObj) > 0) {
      allEnvironments <- as.list(c(sys.frames(),globalenv()))
      loggerObject <- get("racasLogger", envir = as.environment("package:racas"))
      if(any(unlist(lapply(allEnvironments, identical, loggerObject$envir)))) {
        return(loggerObject)
      }
    }
    unlockBinding( "racasLogger", as.environment("package:racas") ) 
    assign("racasLogger", createLogger(envir = envir, ...), as.environment("package:racas"))
    lockBinding( "racasLogger", as.environment("package:racas") ) 
    return(get("racasLogger", envir = as.environment("package:racas")))
  }
  return()
}

