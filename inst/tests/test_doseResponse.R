context("doseResponse.R")

test_that("doseResponse.fitData for ec50",{
  file <- system.file("docs", "default-ec50-fitSettings.json", package = "racas")
  fitSettingsJSON <- readChar(file, file.info(file)$size)
  load("data/doseResponse/fitData_EC50.rda")
  expect_that(doseResponse.fitData(fitSettings, fitData),
              equals(fitData_EC50))
})
