\name{tabplot-package}
\alias{tabplot}
\docType{package}
\title{
Tableplot, a visualization of large statistical datasets
}
\description{
A tableplot is a visualisation of a (large) statistical dataset with a dozen of variables, both both numeric and categorical. Each column represents a variable and each row bin is an aggregate of a certain number of records. For numeric variables, a bar chart of the mean values is depicted. For categorical variables, a stacked bar chart is depicted of the proportions of categories. Missing values are taken into account. Also supports large ffdf datasets from the ff package.
}
\details{
\tabular{ll}{
Package: \tab tabplot\cr
Type: \tab Package\cr
Version: \tab 0.9-1\cr
Date: \tab 2011-01-25\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
The function \code{\link{tableplot}} is used to create a tableplot. With the GUI \code{\link{tableGUI}} users are able to easily design the tableplot.
}
\author{
Martijn Tennekes and Edwin de Jonge

Maintainer:  <mtennekes@gmail.com>
}

\keyword{visualization, large datasets}
\examples{
	irisNA <- iris
	# simulate missing data
	is.na(irisNA$Sepal.Width) <- sample(1:nrow(iris), 30)
	is.na(irisNA$Species) <- sample(1:nrow(iris), 15)

	tableplot(irisNA)
}
