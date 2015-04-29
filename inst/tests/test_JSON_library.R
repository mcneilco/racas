context("JSON_library.R")

test_that("pickBestLabel can get a basic label", {
  label <- list(id=6, labelText="myName", preferred = TRUE, lsTypeAndKind="name_protocol name")
  protocol <- list(id=3, lsLabels = list(label))
  expect_equal(pickBestLabel(protocol), label)
})

test_that("pickBestLabel can get a preferred label", {
  label1 <- list(id=6, labelText="otherName", preferred = FALSE, lsTypeAndKind="name_protocol name")
  label2 <- list(id=7, labelText="myName", preferred = TRUE, lsTypeAndKind="name_protocol name")
  protocol <- list(id=3, lsLabels = list(label1, label2))
  expect_equal(pickBestLabel(protocol), label2)
})

test_that("pickBestLabel can get the correct labelTypeAndKind", {
  label1 <- list(id=6, labelText="otherName", preferred = TRUE, lsTypeAndKind="id_important id", recordedDate = 345343)
  label2 <- list(id=7, labelText="myName", preferred = FALSE, lsTypeAndKind="name_protocol name", recordedDate = 345343)
  protocol <- list(id=3, lsLabels = list(label1, label2))
  expect_equal(pickBestLabel(protocol, "name_protocol name"), label2)
})

test_that("getPreferredName can get a name", {
  label <- list(id=6, labelText="myName", preferred = TRUE, lsTypeAndKind="name_protocol name", lsType="name", ignored=0)
  protocol <- list(id=3, lsLabels = list(label))
  expect_equal(getPreferredName(protocol), "myName")
})
