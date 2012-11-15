library(shiny)
data(diamonds, package="ggplot2")

dataset <- diamonds
vars <- names(dataset)

shinyUI(pageWithSidebar(
	
	# Application title
	headerPanel("Diamonds tableplot"),
	
	sidebarPanel(
		selectInput("sortCol", label="Sort on:", choices=vars),
		checkboxInput("decreasing", label="Descending", value=FALSE),
		sliderInput("fromto", label="Range:", min=0, max=1, value=c(0,1), format="#0.0%"),
# 		numericInput("from", label="from:", value=0, min=0, max=100),
# 		numericInput("to", label="to:", value=100, min=0, max=100),
		checkboxGroupInput("select", label="Select columns:", choices=vars, selected=vars),
		numericInput("nBins", label="# bin", value=100, min=2, max=500, step=1)    
	),
	
	mainPanel(
		plotOutput("plot", height="800px")
	)
))
