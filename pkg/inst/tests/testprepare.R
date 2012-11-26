library(testthat)
 
library(ffbase)
library(grid)

context("prepare")

test_that("prepare works",{
	data(iris)
	p <- prepare(iris)
	iris_data <- p$data[,]
	iris_ordered <- p$ordered[,]
	
	# sort both iris and iris_data
	ordered <- lapply(iris, order)
	x <- mapply("[", iris, ordered, SIMPLIFY=FALSE)
	rownames(x) <- NULL
	
	y <- mapply("[", iris_data, iris_ordered, SIMPLIFY=FALSE)
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