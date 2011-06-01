library(testthat)

test_that("Tableplot object creation works",{
	setwd("../../")
	require(data.table)
	tab <- tableplot(iris, plot=FALSE)
	expect_that(names(tab), is_identical_to(c("n", "nBins", "binSizes", "isNumber", "rows", "columns")))
	expect_that(length(tab$columns), equals(5))
})