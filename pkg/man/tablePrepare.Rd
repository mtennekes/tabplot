\name{tablePrepare}
\alias{tablePrepare}
\title{Prepares a dataset for tableplotting}
\usage{
  tablePrepare(x, name = NULL, ...)
}
\arguments{
  \item{x}{\code{\link{data.frame}} or
  \code{\link[ff:ffdf]{ffdf}}, will be transformed into an
  \code{\link[ff:ffdf]{ffdf}} object.}

  \item{name}{name of the dataset}

  \item{...}{at the moment not used}
}
\value{
  a prepared object, including the data and order of each
  of the columns
}
\description{
  Tableplots from a large dataset can be generated very
  fast when the preprocessing stage is done only once. This
  function preprocesses the dataset, and returns an object
  that can be passed to \code{\link{tableplot}}. From this
  stage, tableplots are generated very fast, no matter on
  which column the data is sorted or how many row bins are
  chosen.
}
\details{
  The function \code{\link{bin_data}} needs a prepared
  \code{\link{data.frame}} Prepare transforms the supplied
  data into an \code{\link[ff:ffdf]{ffdf}} object and
  calculates the order of each of its columns. Knowing the
  order of the columns speeds up the binning process
  consideratly, For large \code{\link[ff:ffdf]{ffdf}}
  objects this may be a time consuming step so it can be
  wise to call prepare before making a tableplot.
}
\examples{
# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

p <- tablePrepare(diamonds)

tableplot(p, nBins=200, sortCol=depth)
tableplot(p, nBins=50, sortCol=price)

}

