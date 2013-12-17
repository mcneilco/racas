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
      stop("ErrorList has not been defined on the search path")
    }
    errorList <<- c(errorList, errorMessage)
  } else {
    if (!exists("errorList", where = errorEnv)) {
      stop("ErrorList has not been defined in the given environment")
    }
    assign("errorList", c(errorEnv$errorList, errorMessage), pos = errorEnv)
  }
}
