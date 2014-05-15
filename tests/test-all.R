library(testthat)
#To Run tests manually: library(testthat); test_dir("inst/tests/");
#To Test Query Functionality: library(testthat); options("test_query" = TRUE); test_dir("inst/tests/");
# Needed before testing the package:
# Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = FALSE)
updateResults <- FALSE
test_package("racas")
#test_package("racas", reporter="minimal")
