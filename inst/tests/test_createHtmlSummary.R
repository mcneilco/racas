context("createHtmlSummary.R")

test_that("createHtmlSummary works",{
  load("data/htmlSummaryNormal_output.Rda")
  expect_that(createHtmlSummary(T,list(1,2,3),T,list(5,4,3),list(wood="tree", this="that"), dryRun=T),
              equals(htmlSummaryNormal_output))
})

test_that("saveAnalysisResults works", {
  load("data/saveAnalysisResults_output.Rda")
  expect_that(saveAnalysisResults(list(),F,"html stuff", testMode=T),
              equals(saveAnalysisResults_output))
})
