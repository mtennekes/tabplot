library(shiny)
data(diamonds, package="ggplot2")

dataset <- diamonds
vars <- names(dataset)

shinyUI(pageWithSidebar(
	
	# Application title
	headerPanel("Diamonds tableplot"),
	
	sidebarPanel(
		uiOutput("sortOn"),
		#selectInput("sortCol", label="Sort on:", choices=vars),
		checkboxInput("decreasing", label="Descending", value=FALSE),
		sliderInput("fromto", label="Range:", min=0, max=1, value=c(0,1), format="#0.0%"),
		#uiOutput("selected"),
		checkboxGroupInput("select", label="Select columns:", choices=vars, selected=vars),
		numericInput("nBins", label="# bins", value=100, min=2, max=500, step=1)    
	),
	
	mainPanel(
		plotOutput("plot", height="800px")
	)
))
