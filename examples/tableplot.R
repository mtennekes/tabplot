\dontrun{
	require(ggplot2)

	# load diamondsNA dataset from ggplot2
	data(diamonds)
	tableplot(diamonds)
	
	# customized tableplot
	tableplot(diamonds, colNames=c("carat", "cut", "color", "clarity", "price"), sortCol="price", from=0, to=5)

	# load movies dataset from ggplot2
	data(movies)
	tableplot(movies[,c(3:5,17:24)], sortCol="rating", decreasing=FALSE, scales="lin", nBins=100)
}

tableplot(iris)
tableplot(iris, sortCol="Species")
