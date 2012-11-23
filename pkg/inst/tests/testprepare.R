library(testthat)
 
library(ffbase)
library(grid)

context("prepare")

test_that("prepare works",{
	data(iris)
	p <- prepare(iris)
	iris_data <- p$data[,]
	
	# sort both iris and iris_data
	x <- iris[do.call(order, iris),]
	rownames(x) <- NULL
	
	y <- iris_data[do.call(order, iris_data),]
	rownames(y) <- NULL
	
	expect_identical(x, y)
})

test_that("prepare works",{
	data(iris)
	p <- prepare(iris)
	
	x <- lapply(iris, sort)
	y <- mapply(function(d, o)d[o], as.data.frame(p$data), as.data.frame(p$ordered), SIMPLIFY=FALSE)
	expect_identical(x, y)
})