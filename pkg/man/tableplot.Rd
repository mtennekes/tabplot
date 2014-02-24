\name{tableplot}
\alias{tableplot}
\title{Create a tableplot}
\usage{
tableplot(dat, select, subset = NULL, sortCol = 1, decreasing = TRUE,
  nBins = 100, from = 0, to = 100, nCols = ncol(dat), sample = FALSE,
  sampleBinSize = 1000, scales = "auto", max_levels = 50,
  pals = list("Set1", "Set2", "Set3", "Set4"), change_palette_type_at = 20,
  colorNA = "#FF1414", numPals = "Blues", limitsX = NULL,
  bias_brokenX = 0.8, IQR_bias = 5, select_string = NULL,
  subset_string = NULL, colNames = NULL, filter = NULL, plot = TRUE,
  ...)
}
\arguments{
  \item{dat}{a \code{\link{data.frame}}, an
  \code{\link[ff:ffdf]{ffdf}} object, or an object created
  by \code{\link{tablePrepare}} (see details below).
  Required.}

  \item{select}{expression indicating the columns of
  \code{dat} that are visualized in the tablelplot Also
  column indices are supported. By default, all columns are
  visualized. Use \code{select_string} for character
  strings instead of expressions.}

  \item{subset}{logical expression indicing which rows to
  select in \code{dat} (as in \code{\link{subset}}). It is
  also possible to provide the name of a categorical
  variable: then, a tableplot for each category is
  generated. Use \code{subset_string} for character strings
  instead of an expressions.}

  \item{sortCol}{column name on which the dataset is
  sorted. It can be eiter an index or an expression name.
  Also a character string can be used, but this is
  discouraged for programming purposes (use an index
  instead).}

  \item{decreasing}{boolean that determines whether the
  dataset is sorted decreasingly (\code{TRUE}) of
  increasingly (\code{FALSE}).}

  \item{nBins}{number of row bins}

  \item{from}{percentage from which the sorted data is
  shown}

  \item{to}{percentage to which the sorted data is shown}

  \item{nCols}{the maximum number of columns per tableplot.
  If this number is smaller than the number of columns
  selected in \code{datNames}, multiple tableplots are
  generated, where each of them contains the sorted
  column(s).}

  \item{sample}{boolean that determines whether to sample
  or use the whole data. Only useful when
  \code{\link{tablePrepare}} is used.}

  \item{sampleBinSize}{the number of sampled objects per
  bin, if \code{sample} is \code{TRUE}.}

  \item{scales}{determines the horizontal axes of the
  numeric variables in \code{colNames}. Options: "lin",
  "log", and "auto" for automatic detection. Either
  \code{scale} is a named vector, where the names
  correspond to numerical variable names, or \code{scale}
  is unnamed, where the values are applied to all numeric
  variables (recycled if necessary).}

  \item{max_levels}{maximum number of levels for
  categorical variables. Categorical variables with more
  levels will be rebinned into \code{max_levels} levels.
  Either a positive number or -1, which means that
  categorical variables are never rebinned.}

  \item{pals}{list of color palettes. Each list item is on
  of the following: \itemize{ \item a palette name of
  \code{\link{tablePalettes}}, optionally with the starting
  color between brackets. \item a color vector } If the
  list items are unnamed, they are applied to all selected
  categorical variables (recycled if necessary). The list
  items can be assigned to specific categorical variables,
  by naming them accordingly.}

  \item{change_palette_type_at}{number at which the type of
  categorical palettes is changed. For categorical
  variables with less than \code{change_palette_type_at}
  levels, the palette is recycled if necessary. For
  categorical variables with \code{change_palette_type_at}
  levels or more, a new palette of interpolated colors is
  derived (like a rainbow palette).}

  \item{colorNA}{color for missing values}

  \item{numPals}{vector of palette names that are used for
  numeric variables. These names are chosen from the
  sequential palette names in \code{\link{tablePalettes}}.
  Either \code{numPals} is a named vector, where the names
  correspond to the numerical variable names, or an unnamed
  vector (recycled if necessary).}

  \item{limitsX}{a list of vectors of length two, where
  each vector contains a lower and an upper limit value.
  Either the names of \code{limitsX} correspond to
  numerical variable names, or \code{limitsX} is an unnamed
  list (recycled if necessary).}

  \item{bias_brokenX}{parameter between 0 en 1 that
  determines when the x-axis of a numeric variable is
  broken. If minimum value is at least \code{bias_brokenX}
  times the maximum value, then X axis is broken. To turn
  off broken x-axes, set \code{bias_brokenX=1}.}

  \item{IQR_bias}{parameter that determines when a
  logarithmic scale is used when \code{scales} is set to
  "auto". The argument \code{IQR_bias} is multiplied by the
  interquartile range as a test.}

  \item{select_string}{character equivalent of the
  \code{select} argument (particularly useful for
  programming purposes)}

  \item{subset_string}{character equivalent of the
  \code{subset} argument (particularly useful for
  programming purposes)}

  \item{colNames}{deprecated; used in older versions of
  tabplot (prior to 0.12): use \code{select_string}
  instead}

  \item{filter}{deprecated; used in older versions of
  tabplot (prior to 0.12): use \code{subset_string}
  instead}

  \item{plot}{boolean, to plot or not to plot a tableplot}

  \item{...}{layout arguments, such as \code{fontsize} and
  \code{title}, are passed on to
  \code{\link[=plot.tabplot]{plot}}}
}
\value{
\code{\link{tabplot-object}} (silent output). If multiple
tableplots are generated (which can be done by either
setting \code{subset} to a categorical column name, or by
restricting the number of columns with \code{nCols}), then
a list of \code{\link{tabplot-object}s} is silently
returned.
}
\description{
A tableplot is a visualisation of (large) multivariate
datasets. Each column represents a variable and each row
bin is an aggregate of a certain number of records. For
numeric variables, a bar chart of the mean values is
depicted. For categorical variables, a stacked bar chart is
depicted of the proportions of categories. Missing values
are taken into account. Also supports large
\code{\link[ff:ffdf]{ffdf}} datasets from the
\code{\link[ff]{ff}} package. For a quick intro, see
\href{../doc/tabplot-vignette.html}{\code{vignette("tabplot-vignette")}}.
}
\details{
For large dataset, we recommend to use
\code{\link{tablePrepare}} which does all the necessary
preprocessing that are needed to make any tableplot of the
particular dataset. The resulting object of this function
is passed on to \code{tableplot} (argument \code{dat}). Now
tableplotting is very fast, and even faster with sampling
enabled (\code{sample=TRUE}).
}
\note{
In early development versions of \code{tabplot} (prior to
version 1.0) it was possible to sort datasets on multiple
columns. To increase to tableplot creation speed, this
feature is dropped. For multiple sorting purposes, we
recommend to use the \code{subset} parameter instead.
}
\examples{
# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

# default tableplot
tableplot(diamonds)

# most expensive diamonds
tableplot(diamonds, 
		  select=c(carat, cut, color, clarity, price), 
		  sortCol=price, 
		  from=0, 
		  to=5)

# for large datasets, we recommend to preprocess the data with tablePrepare:
p <- tablePrepare(diamonds)

# specific subsetting
tableplot(p, subset=price < 5000 & cut=='Ideal')

# change palettes
tableplot(p, 
		  pals=list(cut="Set4", color="Paired", clarity=grey(seq(0, 1,length.out=7))),
		  numPals=c(carat="Greens", price="Purples"))



# create a tableplot cut category, and fix scale limits of carat, table, and price
tabs <- tableplot(p, subset=cut,
				  limitsX=list(carat=c(0,4), table=c(55, 65), price=c(0, 20000)), plot=FALSE)
plot(tabs[[3]], title="Very good cut diamonds")

}
\seealso{
\code{\link{itableplot}}
}
\keyword{visualization}

