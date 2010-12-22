\name{TablePlot-package}
\alias{TablePlot-package}
\alias{TablePlot}
\docType{package}
\title{
Visualization of large statistical datasets.
}
\description{
A tableplot is a visualisation of a (large) statistical dataset with a dozen of variables, both both numeric and categorical. Each column represents a variable and each row bin is an aggregate of a certain number of records. The values of cells are depicted as horizontal bars. Missing values are taken into account. A GUI has been implemented which is used to easily customize the tableplot. Also supports large ffdf datasets from the ff package.
}
\details{
\tabular{ll}{
Package: \tab TablePlot\cr
Type: \tab Package\cr
Version: \tab 0.9\cr
Date: \tab 2010-12-24\cr
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
