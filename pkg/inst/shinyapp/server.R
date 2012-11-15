library(shiny)
library(tabplot)
p <- prepare(diamonds)

shinyServer(function(input, output) {
	
	dataset <- reactive(function(){
		p
	})
	
	output$sortOn <- reactiveUI(function(){
		#vars <- colnames(p$data)
		vars <- input$select
		if (!length(vars)) vars <- colnames(p$data)
		selectInput("sortCol", label="Sort on:", choices=vars)
	})
	
	output$plot <- reactivePlot(function() {
		dat <- dataset()
		sortCol <- input$sortCol
		if (!length(sortCol)) sortCol = 1
		decreasing <- input$decreasing
		select <- input$select
		#from <- input$from
		#to <- input$to
 		fromto <- 100*input$fromto
 		from <- fromto[1]
 		to <- fromto[2]
		nBins <- max(2,as.numeric(input$nBins), na.rm=TRUE)
		tableplot( dat, from=from, to=to
				 , sortCol = sortCol, select_string = select
				 , decreasing = decreasing, nBins=nBins
				 )
	})
})