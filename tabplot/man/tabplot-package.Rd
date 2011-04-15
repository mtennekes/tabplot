\name{tabplot-package}
\alias{tabplot-package}
\alias{tabplot-package}
\title{Tableplot, a visualization of large statistical datasets...}
\description{Tableplot, a visualization of large statistical datasets}
\details{\tabular{ll}{
Package: \tab tabplot\cr
Type: \tab Package\cr
Version: \tab 0.9-2\cr
Date: \tab 2011-04-15\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}

A tableplot is a visualisation of a (large) statistical dataset with a dozen of variables, both both numeric and categorical. Each column represents a variable and each row bin is an aggregate of a certain number of records. For numeric variables, a bar chart of the mean values is depicted. For categorical variables, a stacked bar chart is depicted of the proportions of categories. Missing values are taken into account. Also supports large ffdf datasets from the ff package.

The function \code{\link{tableplot}} is used to create a tableplot. With the GUI \code{\link{tableGUI}} users are able to easily design the tableplot.}
\alias{tabplot}
\docType{package}
\author{Martijn Tennekes \email{mtennekes@gmail.com} and Edwin de Jonge}
\keyword{visualization,}
\keyword{large}
\keyword{datasets}
\examples{irisNA <- iris
# simulate missing data
is.na(irisNA$Sepal.Width) <- sample(1:nrow(iris), 30)
is.na(irisNA$Species) <- sample(1:nrow(iris), 15)
tableplot(irisNA)}

