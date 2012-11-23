tableApp <- function() {
	if (require(shiny)) 
		shiny::runApp(system.file("shinyapp", package="tabplot"))
	invisible()
}