library(shiny)
data(diamonds, package="ggplot2")

vars <- names(diamonds)

shinyUI(pageWithSidebar(
	
	# Application title
	headerPanel("Shiny Tableplot"),
	
	sidebarPanel(
		uiOutput("df"),
		uiOutput("sortOn"),
		checkboxInput("decreasing", label="Descending", value=TRUE),
		sliderInput("fromto", label="Range:", min=0, max=1, value=c(0,1), format="#0.0%"),
		uiOutput("selected"),
		numericInput("nBins", label="# bins", value=100, min=2, max=500, step=1)    
	),
	
	mainPanel(
		plotOutput("plot", height="800px")
	)
))
