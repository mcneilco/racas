#' Error and warning trapping and logging
#' 
#' Traps errors and warnings, logging them out
#' 
#' @param expr An expression which will have errors caught
#' @return A list with value, errorList, and warningList
#'   
#' @details Don't nest this- it uses a global Messenger. \code{value} will be
#' \code{NULL} if there are any caught errors.
#' 
tryCatchLog <- function(expr) {
  output <- NULL
  MyMessenger <- messenger()
  MyMessenger$capture_output({output <- expr})
  return(list(value=output, 
              errorList = MyMessenger$errors, 
              warningList = MyMessenger$warnings))
}



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
#' param errorEnv deprecated
#' 
#' The error is added as class \code{userError} to the racasMessenger.
#' 
#' @aliases userError
addError <- function(errorMessage, errorEnv = NULL) {
  globalMessenger <- messenger()
  newError <- simpleError(errorMessage)
  class(newError) <- c("userError", class(newError))
  globalMessenger$addError(newError)
}

#'Fatal error tracking
#'
#'Declares an error to be of class \code{userStop}, to distinguish between errors we programmed
#'into the system (e.g. "Unrecognized scientist") and errors R gives (e.g. "object not found")
#'
#'@export
#'@param message The error message that the user should see
#'@aliases userStop
#'
#'@details Stops the function, and adds the class "userStop" and "userError" to
#'  the error object
#'  
#'All errors intended for users to view should be thrown using \code{stopUser}. Any error 
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
  class(e) <- c("userStop", "userError", class(e))
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
#'Declares a warning to be of class "userWarning", to distinguish between warnings we programmed
#'into the system (e.g. "Found extra meta data") and errors R gives (e.g. "argument has length >1")
#'
#'@export
#'@param message The warning message that the user should see
#'@aliases userWarning
#'
#'@details Adds the class "userWarning" to the warning object.
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

#' Get error text for users
#' 
#' From a list of error objects, get error messages for users, replacing
#' internal errors with a message to look in the logs.
#' 
#' @param errorList a list of items of class "error"
#'   
#' @details Error messages with class \code{\link{userStop}} pass just their
#'   message, future plans to group errors by class if there are very large
#'   numbers.
getErrorText <- function (errorList) {
#   internalErrors <- Filter(function (x) {!inherits(x, "userError")}, errorList)
#   userErrors <- Filter(function (x) {inherits(x, "userError")}, errorList)
  allTextErrors <- lapply(errorList, getElement, "message")
#   if (length(internalErrors) > 0) {
#     internalTextError <- paste0(
#       "We encountered an internal error. Check the logs at ", Sys.time())
#     allTextErrors <- c(internalTextError, allTextErrors)
#   }
  return(rev(allTextErrors))
}

#' Get warning text for users
#' 
#' From a list of warning objects, get warning messages for users, prefixing
#' internal errors with a message.
#' 
#' @param errorList a list of items of class "warning"
#'   
#' @details Warning messages with class \code{\link{userWarning}} pass just
#'   their message, future plans to group warnings by class if there are very
#'   large numbers.
getWarningText <- function (warningList) {
  lapply(warningList, function(x) {
    if (inherits(x, "userWarning")) {
      x$message
    } else {
      paste0("The system has encountered an internal warning, ", 
             "give this message to your system administrator: ", x$message)
    }
  })
}
