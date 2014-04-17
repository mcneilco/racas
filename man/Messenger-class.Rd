\docType{class}
\name{Messenger-class}
\alias{Messenger-class}
\title{Messenger}
\value{
A json object with the sessionid among a number of html
divs to display back to the user
}
\description{
Reference class object to allow passing of messages
}
\examples{
#Basic Messenger
myMessenger <- Messenger$new(envir = environment())

#Racas Messenger
#This special messenger is stored within the racas environment and can be passed in and out of functions
#To retrieve the racas messenger
racasMessenger <- messenger()
#To reset the messenger
racasMessenger <- messenger()$reset()

#Adding messages
myMessenger <- Messenger$new(envir = environment())
myMessenger$error("myerror")
myMessenger$error("mywarning")
myMessenger$info("mywarning")
myMessenger$userError("my user error")
myMessenger$userWarning("my user warning")
myMessenger$userInfo("my user info")

#Running and capture output using messenger
myMessenger <- Messenger$new(envir = environment())

#This is run like normal
myMessenger$captureOutput("test <- 1+1")
test

#This captures the error
myMessenger <- Messenger$new(envir = environment())
test <- function() stop("there is an error!")
myMessenger$captureOutput("test()")
myMessenger$errors

#Capturing a user error on run
myMessenger <- Messenger$new(envir = environment())
test <- function() stop("there is an error!")
myMessenger$captureOutput("test()", userError = "There was an error running test function")
myMessenger$errors
myMessenger$userErrors

#Adding a user error within a captureOutput Call (use racas messenger)
racasMessenger <- messenger()$reset()
test <- function() {
 racasMessenger <- messenger()
 me <- try(stop("there is an error!"))
 if(class("me")=="try-error") {
   racasMessenger$userError("Inner function error")
 }
 return(1)
}
myMessenger$captureOutput("answer <- test()", userError = "There was an error running test function")
}

