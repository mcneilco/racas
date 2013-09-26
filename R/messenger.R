Messenger <- setRefClass("Messenger", 
                       fields = list(errors = "character",
                                     warnings = "character",
                                     infos = "character"),
                       methods = list(
                         addError = function(x) {
                           errors <<- c(errors,as.character(x))
                         },
                         setErrors = function(x) {
                           errors <<- as.character(x)
                         },
                         addWarning = function(x) {
                           warnings <<- c(warnings,as.character(x))
                         },
                         setWarnings = function(x) {
                           warnings <<- as.character(x)
                         },
                         addInfo = function(x) {
                           infos <<- c(infos,as.character(x))
                         },
                         setInfos = function(x) {
                           infos <<- as.character(x)
                         },
                         reset = function() {
                           errors <<- as.character()
                           infos <<- as.character()
                           warnings <<- as.character()
                         },
                         json = function() {
                           return(toJSON(list(errors = errors, infos = infos, warnings = warnings)))
                         }
                       )
)

bubbleMessenger <- Messenger$new()

messenger <- function(bubble = TRUE) {
  if(!bubble) {
    return(Messenger$new())
  } else {
    bubbleMessengerObj <- Filter( function(x) 'Messenger' %in% class( get(x) ), ls(pattern = "bubbleMessenger", env = as.environment("package:racas")) )    
    if(length(bubbleMessengerObj) > 0) {
      return(get("bubbleMessenger", envir = as.environment("package:racas")))
    } else {
      assignInNamespace("bubbleMessenger", Messenger$new(),  ns="racas")
      return(get("bubbleMessenger", envir = as.environment("package:racas")))
    }
  }
  return()
}