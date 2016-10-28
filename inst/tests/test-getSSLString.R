context("applicationSettings.R")

test_that("getSSLString works", {
  testBlank <- ""
  testNull <- NULL
  testTrue <- TRUE
  testFalse <- FALSE
  
  expect_that(getSSLString(testBlank), equals("http://"))
  expect_that(getSSLString(testNull), equals("http://"))
  expect_that(getSSLString(testTrue), equals("https://"))
  expect_that(getSSLString(testFalse), equals("http://"))
})