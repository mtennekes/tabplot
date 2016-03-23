#' Object that contains the information to plot a tableplot
#'
#' An object of class \code{tabplot} contains the information to plot a tableplot without the steps that may be time-consuming, such as sorting and aggregating.
#' The function \code{\link{tableplot}} silently returns a tabplot-object (use \code{plot=FALSE} to 
#' supress that the tableplot is plot). The function \code{\link{tableChange}} can be used to change a tabplot-object. The generic functions \code{\link[=plot.tabplot]{plot}} and \code{\link[=summary.tabplot]{summary}} are used to plot and summarize a tabplot-object.
#'
#' @name tabplot-object
#' @rdname tabplot-object
{}

#' Object that contains the information to plot the difference of two tableplots (experimental)
#'
#' A \code{tabplot_compare} is created by substracting two tableplots (see \link{-.tabplot}). For numeric variables, both absolute and relative difference of the mean values are computed. For categorical variables, the freqency tables are compared. 
#'
#' @name tabplot_compare-object
#' @rdname tabplot_compare-object
{}