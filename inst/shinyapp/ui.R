library(shiny)

shinyUI(pageWithSidebar(
	
	# Application title
	headerPanel("Shiny Tableplot"),
	
	sidebarPanel(
		tabsetPanel(
		tabPanel("Data",
			uiOutput("df"),
			checkboxInput("sampling", label="Sampling", value=TRUE),
			uiOutput("selected"),
			uiOutput("sortOn"),
			checkboxInput("decreasing", label="Decreasing", value=TRUE),
			sliderInput("fromto", label="Range:", min=0, max=1, value=c(0,1), round=-1),
			numericInput("nBins", label="Number of bins:", value=100, min=2, max=500, step=1)),
		tabPanel("Numeric",
			uiOutput("logscale")),
		#tabPanel("Categorical"),
		#	uiOutput("palettes")),
		tabPanel("Output",
			p(textOutput("rcode"))))),
	
	mainPanel(
		plotOutput("plot", height="600px")
	)
))

	#checkboxInput("showCode", label="Show code in R console", value=FALSE),