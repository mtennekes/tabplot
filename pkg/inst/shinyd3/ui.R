library(shiny)

tablePlotOutput <- function(outputId, width=800, height=600){
	tagList(
		singleton(tags$script(src="http://d3js.org/d3.v3.js", type="text/javascript")),
		singleton(tags$script(src="js/tableplot.js", type="text/javascript")),
		div(id=outputId, class="tableplot-output", tag("svg", list(width=width, height=height)))
    )
}

shinyUI(pageWithSidebar(
	
	# Application title
	headerPanel("Shiny Tableplot"),
	
	sidebarPanel(
		wellPanel(
			uiOutput("df"),
			uiOutput("selected"),
			uiOutput("sortOn"),
			checkboxInput("decreasing", label="Descending", value=TRUE),
			sliderInput("fromto", label="Range:", min=0, max=1, value=c(0,1), format="#0.0%"),
			numericInput("nBins", label="Number of bins:", value=100, min=2, max=500, step=1),
			checkboxInput("showCode", label="Show code in R console", value=FALSE)),
		checkboxInput("extra", label="Show extra options", value=FALSE),
		conditionalPanel(
			condition = "input.extra == true",
			wellPanel(
				uiOutput("logscale")))
	),
	
	mainPanel(
		tabsetPanel(
			tabPanel("d3", tablePlotOutput("tpplot")),
			tabPanel("Plot", plotOutput("plot"))
		)
	)
))
