context("flattenDeepEntity in JSON_library.R")

test_that("ids are collected correctly",{
  load("data/flattenDeepEntityInput_normal.Rda")
  load("data/flattenDeepEntityOutput_normal.Rda")
  expect_that(flattenDeepEntity(experiment, "subject", "experiment"),
              equals(output))
})

