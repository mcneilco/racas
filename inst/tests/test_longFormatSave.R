context("longFormatSave.R")

test_that("meltBatchCodes works",{
  load("data/meltBatchCodesOutput_normal.Rda")
  exampleEntityData <- data.frame(batchCode=c("CMPD-001", "CMPD-003", "CMPD-002"),stateID=c(3,2,1), stateVersion = c(0,1,5), stateGroupIndex=c(5,6,2), publicData=c(T,F,T))
  expect_that(meltBatchCodes(exampleEntityData, c(6,2)), equals(meltBatchCodesOutput_normal))
})

test_that("linkOldContainers works with old containers", {
  load("data/linkOldContainers_normal.Rda")
  load("data/linkOldContainersOutput_hasOld.Rda")
  testModeData <- data.frame(LABEL_TEXT = c("example CNS 6_1", "example CNS 6_2", "example CNS 6_3"),
             CONTAINER_ID = c(47447, 47448, 47449), stringsAsFactors=F)
  expect_that(linkOldContainers(entityData, stateGroups, labelPrefix, testModeData = testModeData), equals(linkOldContainersOutput))
})

test_that("linkOldContainers works with new containers", {
  load("data/linkOldContainers_normal.Rda")
  load("data/linkOldContainersOutput_hasNew.Rda")
  testModeData <- data.frame(stringsAsFactors=F)
  expect_that(linkOldContainers(entityData, stateGroups, labelPrefix, testModeData = testModeData), equals(linkOldContainersOutput))
})

test_that("linkOldContainers works with a mix of new and old containers", {
  load("data/linkOldContainers_normal.Rda")
  load("data/linkOldContainersOutput_hasMix.Rda")
  testModeData <- data.frame(LABEL_TEXT = c("example CNS 6_2"), CONTAINER_ID = c(47448),stringsAsFactors=F)
  expect_that(linkOldContainers(entityData, stateGroups, labelPrefix, testModeData = testModeData), equals(linkOldContainersOutput))
})

test_that("linkOldContainers works with a mix of prefixed and non-prefixed container labels", {
  load("data/linkOldContainers_normal.Rda")
  load("data/linkOldContainersOutput_hasMixPref.Rda")
  testModeData <- data.frame(LABEL_TEXT = c("2", "example CNS 6_2"), CONTAINER_ID = c(22, 47448),stringsAsFactors=F)
  expect_that(linkOldContainers(entityData, stateGroups, labelPrefix, testModeData = testModeData), equals(linkOldContainersOutput))
})

test_that("linkOldContainers works with non-prefixed labels", {
  load("data/linkOldContainers_normal.Rda")
  load("data/linkOldContainersOutput_nonPref.Rda")
  testModeData <- data.frame(LABEL_TEXT = c("2"), CONTAINER_ID = c(22),stringsAsFactors=F)
  expect_that(linkOldContainers(entityData, stateGroups, labelPrefix, testModeData = testModeData), equals(linkOldContainersOutput))
})

test_that("linkOldContainers works with NULL prefix", {
  load("data/linkOldContainers_normal.Rda")
  load("data/linkOldContainersOutput_nonPref.Rda")
  testModeData <- data.frame(LABEL_TEXT = c("2"), CONTAINER_ID = c(22),stringsAsFactors=F)
  expect_that(linkOldContainers(entityData, stateGroups, labelPrefix=NULL, testModeData = testModeData), equals(linkOldContainersOutput))
})
