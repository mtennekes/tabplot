\name{tableplot}
\alias{tableplot}
\title{
Visualization of large statistical datasets.
}
\description{
A tableplot is a visualisation of a (large) statistical dataset with a dozen of variables, both both numeric and categorical. Each column represents a variable and each row bin is an aggregate of a certain number of records. The values of cells are depicted as horizontal bars. Missing values are taken into account. Supports large ffdf datasets from the ff package. Use \code{\link{tableGUI}} to customize this function with a GUI.
}
\usage{
tableplot(dat, colNames = names(dat), sortCol = colNames[1], decreasing = FALSE, scales = "auto", nBins = 100, from=0,to=100)
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
	vector of names (or indices of colNames) of the columns that are sorted
}
  \item{decreasing}{
	logical vector of the same length as \code{sortCol} that determines whether the columns are sorted decreasingly (TRUE) or increasingly (FALSE)
}
  \item{scales}{
	determines the horizontal axes of the numeric variables, options: "lin", "log", and "auto" for automatic detection (may slow down the process). It should be either one value (applied to all numeric variables) or a vector of values of the same length as \code{colNames}.
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
\details{
A tableplot is a spreadsheet-like plot of the data. Each column represents a variable and each row represents an aggregation of a certain number of records. The value of each cell is depicted as a horizontal bar. For numeric variables, its length is determined by the mean value of the aggregation group, and its colour (lightness) is determined by the percentage of missing values in this group; the more data is missing the lighter the colour blue used. When all values are missing, a red bar of full length is shown. For categorical variables, the bar is divided according to the proportion of categories within the group. Each category is shown in a clearly distinctive colour, where red is preserved for missing values.
}

\examples{
\dontrun{
	require{ggplot2}
	data{diamonds}
	tableplot(diamonds)
	
	data{movies}
	tableplot(movies, colNames=c("year","length","budget","rating","votes","Action","Animation","Comedy","Drama","Documentary","Romance","Short"), sortCol=year, decreasing=FALSE, scales="lin", nBins=100, check=TRUE)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{visualization}
