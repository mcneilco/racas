context("batchAnnotation.R")

test_that("addFileLink works",{
  load("data/addFileLinkOutput_normal.Rda")
  expect_that(addFileLink(c("CMPD01", "CMPD02"), "smeyer", list(id=1,codeName="EXPT-TEST", version=0), 1, "this", "test.txt", testMode=TRUE),
              equals(addFileLinkOutput_normal))
})

test_that("deleteLinkFile works", {
  load("data/deleteLinkFileInput_experiment.Rda")
  expect_that(deleteLinkFile(experiment, testMode=T), equals("test.txt"))
})
