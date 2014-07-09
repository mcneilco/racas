context("messenger")

test_that("userErrors are generated in the correct order",{
  f1 <- function(){
    blah <- messenger()
    blah$addUserError("Error 5")
    f2 <- function() {
      me <- messenger()
      me$addUserError("Error 7")
      stop("ham")
    }
    blah$capture_output("f2()", userError = "Error 6")
    stop("Something")
  }
  myMessenger <- messenger()$reset()
  myMessenger$devMode <- FALSE
  myMessenger$addUserError("Error 1")
  myMessenger$addUserError("Error 2")
  myMessenger$addUserError("Error 3")
  myMessenger$capture_output("f1()", userError = "Error 4")
  myMessenger$capture_output("f1()", userError = "Error 4", continueOnError = FALSE)
  myMessenger$addUserError("Error 8")
  expect_that(myMessenger$userErrors, equals(c("Error 1", "Error 2", "Error 3", "Error 4","Error 5", "Error 6", "Error 7", "Error 8")))
})
