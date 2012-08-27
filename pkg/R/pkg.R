#' Tableplot, a visualization of large datasets
#'
#' \tabular{ll}{
#' Package: \tab tabplot\cr
#' Type: \tab Package\cr
#' Version: \tab 0.12-1\cr
#' Date: \tab 2012-08-14\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' A tableplot is a visualisation of a (large) dataset. Each column represents a variable and each row bin is an aggregate of a certain number of records. For numeric variables, a bar chart of the mean values is depicted. For categorical variables, a stacked bar chart is depicted of the proportions of categories. Missing values are taken into account. Also supports large \code{\link[ff:ffdf]{ffdf}} datasets from the \code{\link[ff:ff]{ff}} package.
#'
#' The function \code{\link{tableplot}} is used to create a tableplot. Other useful functions are
#' \itemize{
#' \item \code{\link{tablePalettes}} to show all quantitative and qualitative palettes that are included
#' \item \code{\link{tableSave}} to save a tableplot
#' \item \code{\link{tableChange}} to make layout changes to a tableplot
#' }
#' Further, a graphical user interface has been implemented in the package \code{\link[tabplotGTK:tabplotGTK]{tabplotGTK}}
#'
#' @name tabplot-package
#' @aliases tabplot
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com} and Edwin de Jonge
#' @keywords visualization large datasets
#' @example ../examples/pkg.R
{}