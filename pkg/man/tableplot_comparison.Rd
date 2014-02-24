\name{-.tabplot}
\alias{-.tabplot}
\title{Compare two tableplots (experimental)}
\usage{
\method{-}{tabplot} (tp1, tp2)
}
\arguments{
  \item{tp1}{the first \link{tabplot-object}}

  \item{tp2}{the second \link{tabplot-object}}
}
\value{
a \link{tabplot_compare-object} that contains information
about the comparison \code{tp1-tp2}
}
\description{
Two tableplots can be compared by substracting two
\link{tabplot-object}s. The result is a
\link{tabplot_compare-object} object in which absolute and
relative differences of mean values are stored, as well as
a comparison of frequency tables for categorical variables.
This object can be plotted with
\code{\link[=plot.tabplot]{plot}}.
}
\examples{
# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

# calculate normalized prices to be used as sample probabilities
price.norm <- with(diamonds, price / max(diamonds$price))

# draw samples
exp.diamonds <- diamonds[sample(1:nrow(diamonds), size=10000, prob=price.norm, replace=TRUE),]
chp.diamonds <- diamonds[sample(1:nrow(diamonds), size=10000, prob=1-price.norm, replace=TRUE),]

tp1 <- tableplot(exp.diamonds)
tp2 <- tableplot(chp.diamonds)

plot(tp2 - tp1)
}

