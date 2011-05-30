#' Tableplot, a visualization of large statistical datasets
#'
#' \tabular{ll}{
#' Package: \tab tabplot\cr
#' Type: \tab Package\cr
#' Version: \tab 0.9-2\cr
#' Date: \tab 2011-04-15\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' A tableplot is a visualisation of a (large) statistical dataset with a dozen of variables, both both numeric and categorical. Each column represents a variable and each row bin is an aggregate of a certain number of records. For numeric variables, a bar chart of the mean values is depicted. For categorical variables, a stacked bar chart is depicted of the proportions of categories. Missing values are taken into account. Also supports large ffdf datasets from the ff package.
#'
#' The function \code{\link{tableplot}} is used to create a tableplot. With the GUI \code{\link{tableGUI}} users are able to easily design the tableplot.
#'
#' @name tabplot-package
#' @aliases tabplot
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com} and Edwin de Jonge
#' @keywords visualization, large datasets
#' @example examples/pkg.R
{}