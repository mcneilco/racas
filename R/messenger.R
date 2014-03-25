#' Messenger
#'
#' Reference class object to allow passing of messages
#'
#' @return A json object with the sessionid among a number of html divs to display back to the user
#' @name Messenger
#' @include logger.R
#' @export
#' @examples
#' #Basic Messenger
#' myMessenger <- Messenger$new(envir = environment())
#' 
#' #Racas Messenger
#' #This special messenger is stored within the racas environment and can be passed in and out of functions
#' #To retrieve the racas messenger
#' racasMessenger <- messenger()
#' #To reset the messenger
#' racasMessenger <- messenger()$reset()
#' 
#' #Adding messages
#' myMessenger <- Messenger$new(envir = environment())
#' myMessenger$addError("myerror")
#' myMessenger$addWarning("mywarning")
#' myMessenger$addInfo("mywarning")
#' myMessenger$addUserError("my user error")
#' myMessenger$addUserWarning("my user warning")
#' myMessenger$addUserInfo("my user info")
#' 
#' #Running and capture output using messenger
#' myMessenger <- Messenger$new(envir = environment())
#' 
#' #This is run like normal
#' myMessenger$captureOutput("test <- 1+1")
#' test
#' 
#' #This captures the error
#' myMessenger <- Messenger$new(envir = environment())
#' test <- function() stop("there is an error!")
#' myMessenger$captureOutput("test()")
#' myMessenger$errors
#' 
#' #Capturing a user error on run
#' myMessenger <- Messenger$new(envir = environment())
#' test <- function() stop("there is an error!")
#' myMessenger$captureOutput("test()", userError = "There was an error running test function")
#' myMessenger$errors
#' myMessenger$userErrors
#' 
#' #Adding a user error within a captureOutput Call (use racas messenger)
#' racasMessenger <- messenger()$reset()
#' test <- function() {
#'  racasMessenger <- messenger()
#'  e <- try(stop("there is an error!"), silent = TRUE)
#'  if(class(e)=="try-error") { 
#'    racasMessenger$addUserError("Inner error")
#'    #note, you have to throw and error for the outer userError to be added
#'    stop("some error")
#'  }
#'  return(1)
#' }
#' racasMessenger$captureOutput("answer <- test()", userError = "Outer error")
#' racasMessenger$userErrors
#' 
Messenger <- setRefClass(Class = "Messenger", 
                         fields = list(errors = "character",
                                       userErrors = "character",
                                       warnings = "character",
                                       userWarnings = "character",
                                       infos = "character",
                                       userInfos = "character",
                                       logger = "Logger",
                                       envir = "environment",
                                       devMode = "logical"),
                         contains = list("objectUtilities"),
                         methods = list(
                           initialize = function(...) {
                             devMode <<- FALSE
                             logger <<- racas:::createLogger()
                           },
                           addError = function(x) {
                             errors <<- c(errors,as.character(x))
                           },
                           addWarning = function(x) {
                             warnings <<- c(warnings,as.character(x))
                           },
                           addInfo = function(x) {
                             infos <<- c(infos,as.character(x))
                           },
                           addUserError = function(x) {
                             userErrors <<- c(userErrors,as.character(x))
                           },
                           addUserWarning = function(x) {
                             userWarnings <<- c(userWarnings,as.character(x))
                           },
                           addUserInfo = function(x) {
                             userInfos <<- c(userInfos,as.character(x))
                           },
                           reset = function() {
                             errors <<- as.character()
                             infos <<- as.character()
                             warnings <<- as.character()
                             userErrors <<- as.character()
                             userInfos <<- as.character()
                             userWarnings <<- as.character()
                             envir <<- parent.frame()
                             logger <<- Logger$new()
                             devMode <<- FALSE
                             return(.self)
                           },
                           captureOutput = function(expr, userError = NULL, userWarning = NULL, userInfo = NULL, continueOnError = TRUE, envir = parent.frame(), ...) {
                             
                             if(continueOnError == TRUE | devMode == TRUE | (length(errors)==0 & length(userErrors)==0)) {
                               
                               if(!devMode) {
                                 outputHandler <- new_output_handler(error = function(x) {addError(x$message)
                                                                                          logger$error(x$message)
                                 },
                                 warning = function(x) {addWarning(x$message)
                                                        logger$warn(x$message)
                                 },
                                 message = function(x) {addInfo(x$message)
                                                        logger$info(x$message)
                                 },
                                 value = function(x) {logger$error(names(x))
                                 },
                                 )
                                 if(!is.null(userError)) addUserError(userError); errorPos <- length(userErrors)
                                 if(!is.null(userWarning)) addUserWarning(userWarning); warningPos <- length(userWarnings)
                                 if(!is.null(userInfo)) addUserInfo(userInfo); infoPos <- length(userInfos)
                                 evaledExpr <- evaluate(expr, envir = envir, output_handler = outputHandler, new_device = FALSE, ...)
                                   if(any(!c("simpleError","error") %in% unlist(lapply(evaledExpr, class)))) {
                                     if(!is.null(userError)) {
                                       if(length(userErrors) <= errorPos) {
                                         userErrors <<- userErrors[-errorPos]
                                       }
                                     }
                                   }
                               } else {
                                 eval(parse(text = expr), envir = envir)
                               }
                             } else {
                               #Do nothing
                               logger$error(paste0("Not running command because of previous errors: ", expr))
                               invisible(NULL)
                             }
                           },
                           toJSON = function() {
                             return(rjson::toJSON(list("error" = length(errors)!=0,
                                                       "warning" = length(warnings)!=0,
                                                       info = length(infos)!=0,
                                                       errors = errors, 
                                                       warnings = warnings,
                                                       infos = infos,
                                                       userError = length(userErrors)!=0,
                                                       userWarning = length(userWarnings)!=0,
                                                       userInfo = length(userInfos)!=0,
                                                       userErrors = userErrors, 
                                                       userWarnings = userWarnings,
                                                       userInfos = userInfos)))
                           }
                         )
)
racasMessenger <- ""
messenger <- function(racas = TRUE, envir = parent.frame(), ...) {
  if(!racas) {
    return(Messenger$new(envir = envir, ...))
  } else {
    allEnvironments <- as.list(sys.frames())
    racasMessengerObj <- Filter( function(x) 'Messenger' %in% class( get(x) ), ls(pattern = "racasMessenger", envir = as.environment("package:racas")) )    
    if(length(racasMessengerObj) > 0) {
      allEnvironments <- as.list(c(sys.frames(),globalenv()))
      messengerObject <- get("racasMessenger", envir = as.environment("package:racas"))
      if(any(unlist(lapply(allEnvironments, identical, messengerObject$envir)))) {
        return(messengerObject)
      }
    }
    unlockBinding( "racasMessenger", as.environment("package:racas") ) 
    assign("racasMessenger", Messenger$new(envir = envir, ...), as.environment("package:racas"))
    lockBinding( "racasMessenger", as.environment("package:racas") ) 
    return(get("racasMessenger", envir = as.environment("package:racas")))
  }
  return()
}
