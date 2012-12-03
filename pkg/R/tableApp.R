#' Graphical User Interface to create tableplots
#'
#' This graphical user interface is developped with the \code{shiny} package.
#' 
#' This function replaces the old \code{tabplotGTK} package, since it only requires an up-to-date brower (and not software like GTK). Moreover, maintanance is a lot easier.
#' @aliases tableApp
#' @export
tableApp <- function() {
	if (require(shiny)) 
		shiny::runApp(system.file("shinyapp", package="tabplot"))
	invisible()
}