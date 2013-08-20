context("longFormatSave.R")

test_that("meltBatchCodes works",{
  load("data/meltBatchCodesOutput_normal.Rda")
  exampleEntityData <- data.frame(batchCode=c("CMPD-001", "CMPD-003", "CMPD-002"),stateID=c(3,2,1), stateVersion = c(0,1,5), stateGroupIndex=c(5,6,2), publicData=c(T,F,T))
  expect_that(meltBatchCodes(exampleEntityData, c(6,2)), equals(meltBatchCodesOutput_normal))
})
