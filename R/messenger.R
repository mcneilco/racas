
#o <- objectUtilities$new(test = "me", this = as.integer(10))
#o <- objectUtilities$new()$fromJSON(toJSON(list(test = "me", this = as.integer(10))))
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
                             
                             if(continueOnError == TRUE | length(errors)==0) {
                               
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
                                                                     )
                                 if(!is.null(userError)) addUserError(userError); errorPos <- length(userErrors)
                                 if(!is.null(userWarning)) addUserError(userWarning); warningPos <- length(userWarnings)
                                 if(!is.null(userInfo)) addUserError(userInfo); infoPos <- length(userInfos)
                                 evaledExpr <<- evaluate(expr, envir = envir, output_handler = outputHandler, new_device = FALSE, ...)
                                 if(!is.null(userError)) {
                                   if(length(userErrors) <= errorPos) {
                                     userErrors <<- userErrors[-errorPos]
                                   }
                                 }
                                 
                               } else {
                                 eval(parse(text = expr), envir = envir)
                               }
                             } else {
                               #Do nothing
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

racasMessenger <- Messenger$new(envir = environment())
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
