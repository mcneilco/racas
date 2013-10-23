Messenger <- setRefClass(Class = "Messenger", 
                         fields = list(errors = "character",
                                       warnings = "character",
                                       infos = "character",
                                       envir = "environment"),
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
                           reset = function() {
                             errors <<- as.character()
                             infos <<- as.character()
                             warnings <<- as.character()
                             envir <<- parent.frame()
                             return(.self)
                           },
                           captureOutput = function(expr, envir = parent.frame(), ...) {
                             outputHandler <- new_output_handler(error = function(x) addError(x$message),
                                                                 warning = function(x) addWarning(x$message),
                                                                 message = function(x) addInfo(x$message),
                             )
                             evaledExpr <- evaluate(expr, envir = envir, output_handler = outputHandler)
                           },
                           toJSON = function() {
                             return(rjson::toJSON(list("error" = length(errors)!=0,
                                                       "warning" = length(warnings)!=0,
                                                       info = length(infos)!=0,
                                                       errors = errors, 
                                                       warnings = warnings,
                                                       infos = infos)))
                           }
                         )
)
setMethod ("toJSON", signature="Messenger", definition= function (x) x$toJSON() )

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
