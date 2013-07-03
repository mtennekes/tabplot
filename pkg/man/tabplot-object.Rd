\name{tabplot-object}
\alias{tabplot-object}
\title{object that contains the information to plot a tableplot}
\description{
  An object of class \code{tabplot} contains the
  information to plot a tableplot without the steps that
  may be time-consuming, such as sorting and aggregating.
  The function \code{\link{tableplot}} silently returns a
  tabplot-object (use \code{plot=FALSE} to supress that the
  tableplot is plot). The function
  \code{\link{tableChange}} can be used to change a
  tabplot-object. The generic functions
  \code{\link[=plot.tabplot]{plot}} and
  \code{\link[=summary.tabplot]{summary}} are used to plot
  and summarize a tabplot-object.
}

