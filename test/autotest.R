library(testthat)

source_dir("../pkg/R", env=parent.frame())
auto_test("../pkg/R", "../pkg/inst/tests")