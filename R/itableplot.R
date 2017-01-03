#' Graphical User Interface to create tableplots
#'
#' This graphical user interface is developed with the \code{\link[shiny:shiny]{shiny}} package. All datasets that are loaded in the global workspace (\code{\link{data.frame}}, \code{\link[ff:ffdf]{ffdf}}, or \code{\link[tabplot:tablePrepare]{prepared}}) are passed on to the GUI.
#' 
#' This function replaces the old \code{tabplotGTK} package, since it only requires an up-to-date browser (and not software like GTK). Furthermore, maintanance is a lot easier.
#' @aliases itableplot
#' @examples
#' \dontrun{
#' require(ggplot2)
#' data(diamonds)
#' 
#' # load other datasets
#' data(iris)
#' data(cars)
#' 
#' itableplot()
#' }
#' @export
itableplot <- function() {
	if (requireNamespace("shiny")) 
		shiny::runApp(system.file("shinyapp", package="tabplot"))
	invisible()
}