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
#'Adds the class "userStop" to a fatal error, to differentiate between 
#'errors that should be displayed to users and errors that shouldn't 
#'(internal errors)
#'
#'@export
#'@param message The error message that the user should see
#'@return Stops the function, and adds the class "userStop" to the error object
#'
#'All helpful errors should be thrown using \code{stopUser}. Any error 
#'thrown using \code{stop} will be treated as an internal error by the simple 
#'experiment loader
stopUser <- function(message) {
  e <- simpleError(message)
  class(e) <- c(class(e), "userStop")
  stop(e)
}
