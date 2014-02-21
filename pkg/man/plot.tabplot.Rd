\name{plot.tabplot}
\alias{plot.tabplot}
\title{Plot a \link{tabplot-object}}
\usage{
\method{plot}{tabplot}(x, fontsize = 10, legend.lines = 8,
  max_print_levels = 15, text_NA = "missing", title = NULL,
  showTitle = NULL, fontsize.title = 14, showNumAxes = TRUE,
  relative = FALSE, vp = NULL, ...)
}
\arguments{
  \item{x}{\link{tabplot-object} or
  \link{tabplot_compare-object}}

  \item{fontsize}{the (maximum) fontsize}

  \item{legend.lines}{the number of lines preserved for the
  legend}

  \item{max_print_levels}{maximum number of printed
  category labels in the legend}

  \item{text_NA}{text printed for the missing values
  category in the legend}

  \item{title}{title of the plot (shown if
  \code{showTitle==TRUE})}

  \item{showTitle}{show the title. By default \code{FALSE},
  unless a \code{title} is given.}

  \item{fontsize.title}{the fontsize of the title}

  \item{showNumAxes}{plots an x-axis for each numerical
  variable, along with grid lines (\code{TRUE} by
  default).}

  \item{relative}{boolean that determines whether relative
  scales are used for relative tableplots. If \code{TRUE},
  then \code{mean.diff.rel<-(mean2-mean1)/mean1*100} are
  used. If \code{FALSE}, then the absolute diference is
  taken: \code{mean <- mean2-mean}.}

  \item{vp}{\code{\link[grid:viewport]{viewport}} to draw
  plot in (for instance useful to stack multiple
  tableplots)}

  \item{...}{other arguments are not used}
}
\description{
Plot a \link{tabplot-object}
}
\examples{
# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

tab <- tableplot(diamonds)
plot(tab, title="Shine on you Crazy Diamond!!!",
	 fontsize=12,
	 legend.lines=7,
	 fontsize.title=16)

}

