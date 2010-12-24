\name{tableplot}
\alias{tableplot}
\title{
Visualization of large statistical datasets.
}
\description{
A tableplot is a visualisation of a (large) statistical dataset with a dozen of variables, both both numeric and categorical. Each column represents a variable and each row bin is an aggregate of a certain number of records. For numeric variables, a bar chart of the mean values is depicted. For categorical variables, a stacked bar chart is depicted of the proportions of categories. Missing values are taken into account. Also supports large ffdf datasets from the ff package. Use \code{\link{tableGUI}} to customize this function with a GUI.
}
\usage{
tableplot(dat, colNames = names(dat), sortCol = 1, decreasing = FALSE, scales = "auto", nBins = 100, from=0, to=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
	a data.frame or an ffdf object (required)
}
  \item{colNames}{
	character vector containing the names of the columns of \code{dat} that are visualized in the tablelplot. If omitted, all columns are visualized. All selected columns should be of class: numeric, integer, factor, or logical.
}
  \item{sortCol}{
	columns that are sorted. \code{sortCol} is either a vector of column names of a vector of indices of \code{colNames}
}
  \item{decreasing}{
	determines whether the columns are sorted decreasingly (TRUE) of increasingly (FALSE). \code{decreasing} can be either a single value that applies to all sorted columns, or a vector of the same length as \code{sortCol}.
}
  \item{scales}{
	determines the horizontal axes of the numeric variables, options: "lin", "log", and "auto" for automatic detection. It should be either one value (applied to all numeric variables) or a vector of values of the same length as \code{colNames}.
}
  \item{nBins}{
	number of row bins
}
  \item{from}{
	percentage from which the data is shown
}
  \item{to}{
	percentage to which the data is shown
}
}

\examples{
\dontrun{
	require(ggplot2)
	
	diamondsNA <- diamonds
	# simulate missing data
	is.na(diamondsNA$price) <- diamondsNA$cut == "Ideal"
	tableplot(diamondsNA)

	data(movies)
	tableplot(movies[,c(3:5,17:24)], sortCol="rating", decreasing=FALSE, scales="lin", nBins=100)
}

irisNA <- iris
# simulate missing data
is.na(irisNA$Sepal.Width) <- sample(1:nrow(iris), 30)
is.na(irisNA$Species) <- sample(1:nrow(iris), 15)

tableplot(irisNA)
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{visualization}
