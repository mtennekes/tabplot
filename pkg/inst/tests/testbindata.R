library(testthat)

library(ffbase)
library(grid)

context("bindata")

test_that("bindata (numerical vars) works",{
	data(iris)
	
	p <- prepare(iris)
	bd <- bin_data(p, nbins=50)
	
	test1 <- all(sapply(bd[1:4], function(x)all(x[, 1]==3)))
	test2 <- all(sapply(bd[1:4], function(x)all(x[, 3]==1)))
	expect_true(test1 && test2)
})

test_that("bindata (numerical vars)works",{
	data(iris)
	
	p <- prepare(iris)
	bd <- bin_data(p, nbins=50)
	
	test1 <- all(rowSums(bd[[5]])==1)
	
	expect_true(test1)
})
