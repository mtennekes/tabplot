\name{tableGUI}
\alias{tableGUI}
\title{
	A graphical user interface for customizing a tableplot visualization.
}
\description{
	A GUI by which the user can specify tableplots. First, the user selects a data.frame (of ffdf object). Second, the user selects columns from this data.frame that are visualized. Thirds, the user customizes the tableplot by changing the number of row bins, determine which column should be sorted. Further, the user is able to create a factor vector out of a numerical vector (by means of the function \code{\link{num2fac}}).
}
\usage{
tableGUI()
}

\examples{
	testDf <- data.frame(num_unif=runif(10000, min=120,max=140), num_norm=rnorm(10000), mun_exp=2^(rnorm(100)) * 10000, cat=factor(floor(runif(10000)*5)+1, labels=c("One", "Two", "Three", "Four", "Five")))
	tableGUI()
}