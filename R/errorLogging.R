#' Error and warning trapping
#' 
#' Traps errors and warnings, creating lists of each
#' 
#' @param expr An expression which will have errors caught
#' @return A list with value (either result or error) and warningList
tryCatch.W.E <- function(expr) {
  # This function is taken from the R demo file and edited
  # http://svn.r-project.org/R/trunk/src/library/base/demo/error.catching.R
  # R-help mailing list, Dec 9, 2010
  #
  # It stores the warnings rather than letting them exit as normal for tryCatch
  #
  W <- list()
  
  w.handler <- function(w){ # warning handler
    W <<- c(W,list(w))
    invokeRestart("muffleWarning")
  }
  
  return(list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                          warning = w.handler),
              warningList = W))
}

#'Non-fatal error tracking
#'
#'Add errors to a list of errors to show a user
#'
#'@param errorMessage The message for the user
#'@param errorEnv
#'
#'If the errorEnv is not provided, the search path is used to find an errorList. 
#'Providing an errorEnv is the preferred method to avoid name collsions.
addError <- function(errorMessage, errorEnv = NULL) {
  if (is.null(errorEnv)) {
    if (!exists("errorList")) {
      stopUser("ErrorList has not been defined on the search path")
    }
    errorList <<- c(errorList, errorMessage)
  } else {
    if (!exists("errorList", where = errorEnv)) {
      stopUser(paste0(errorMessage, 
                  "; and internal error in use of addError function: errorList has not been defined in the given environment"))
    }
    assign("errorList", c(errorEnv$errorList, errorMessage), pos = errorEnv)
  }
}

#'Fatal error tracking
#'
#'Declares an error to be of type "userStop", to distinguish between errors we programmed
#'into the system (e.g. "Unrecognized scientist") and errors R gives (e.g. "object not found")
#'
#'@export
#'@param message The error message that the user should see
#'@return Stops the function, and adds the class "userStop" to the error object
#'
#'All helpful errors should be thrown using \code{stopUser}. Any error 
#'thrown using \code{stop} will be treated as an internal error by the simple 
#'experiment loader.
#'
#'When using this function, it is important to give it a single string as an error message. 
#'This means using paste. While \code{stop("text ", variable, " text")} is okay syntax, 
#'you will get an error if you try \code{stopUser("text ", variable, " text")}. Use paste0 
#'instead: \code{stopUser(paste0("text ", variable, " text"))} (note that this syntax is also 
#'perfectly acceptable inside \code{stop})
stopUser <- function(message) {
  e <- simpleError(message)
  class(e) <- c(class(e), "userStop")
  stop(e)
}

#' @rdname stopUser
stopUserWithTime <- function(logFileName) {
  stopUser (paste0("Internal Error: The loader was unable to save your data. Check the log ", 
                   logFileName, " at ", Sys.time()))
}

#' @rdname stopUser
stopUserAndLogInvalidJSON <- function (logName, logFileName, url, response, method = "GET", postfields = NULL) {
  myLogger <- createLogger(logName = logName, logFileName = logFileName)
  if (method != 'GET') {
    errorMessage <- paste0("Request to ", url, " with method '", method, 
    "' responded with invalid JSON when sent \n ", postfields, 
    "\nBody was: \n", response)
  } else {
    errorMessage <- paste0("Request to ", url, " received invalid JSON: \n", response)
  }
  myLogger$error(errorMessage)
  stopUserWithTime(logFileName)
}


#'Warning tracking
#'
#'Declares a warning to be of type "userWarning", to distinguish between warnings we programmed
#'into the system (e.g. "Found extra meta data") and errors R gives (e.g. "argument has length >1")
#'
#'@export
#'@param message The warning message that the user should see
#'@return Adds the class "userWarning" to the warning object
#'
#'All helpful errors should be thrown using \code{warnUser}. Any warning 
#'thrown using \code{warning} will be treated as an internal warning by the simple 
#'experiment loader.
#'
#'When using this function, it is important to give it a single string as a message. 
#'This means using paste. While \code{warning("text ", variable, " text")} is okay syntax, 
#'you will get an error if you try \code{warnUser("text ", variable, " text")}. Use paste0 
#'instead: \code{warnUser(paste0("text ", variable, " text"))} (note that this syntax is also 
#'perfectly acceptable inside \code{warning})
warnUser <- function(message) {
  w <- simpleWarning(message)
  class(w) <- c(class(w), "userWarning")
  warning(w)
}
