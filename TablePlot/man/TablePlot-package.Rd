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
Version: \tab 1.0\cr
Date: \tab 2010-11-24\cr
License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}
The function \code{\link{tableplot}} is used to create a tableplot. With the GUI \code{\link{tableGUI}} users are able to easily design the tableplot.
}
\author{
Martijn Tennekes

Maintainer:  <mtennekes@gmail.com>
}

\keyword{visualization, large datasets}
\examples{
testDf <- data.frame(num_unif=runif(10000, min=120,max=140), num_norm=rnorm(10000), mun_exp=2^(rnorm(100)) * 10000, cat=factor(floor(runif(10000)*5)+1, labels=c("One", "Two", "Three", "Four", "Five")))
tableplot(testDf)
tableGUI()
}
