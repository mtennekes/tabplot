library(shiny)
library(tabplot)

obs <- ls(envir=.GlobalEnv)
dfs <- obs[sapply(obs, function(x)inherits(get(x, envir=.GlobalEnv), "data.frame"))]

ps <- lapply(dfs, function(d)prepare(get(d)))
names(ps) <- dfs


shinyServer(function(input, output) {
	
	dataset <- reactive(function(){
		dfname <- ifelse(is.null(input$dataset), dfs[1], input$dataset)
		ps[[dfname]]
	})
	

	output$df <- reactiveUI(function(){
		p <- dataset()
		vars <- colnames(p$data)
		selectInput("dataset", label="Dataset:", choices=dfs)
	})
	
	output$selected <- reactiveUI(function(){
		p <- dataset()
		vars <- colnames(p$data)
		checkboxGroupInput("select", label="Select columns:", choices=vars, selected=vars)
	})

	output$sortOn <- reactiveUI(function(){
		p <- dataset()
		vars <- colnames(p$data)
		choices <- if (length(input$select)) input$select else vars
		selectInput("sortCol", label="Sort on:", choices=choices)
	})
	
	
	output$plot <- reactivePlot(function() {
		p <- dataset()
		vars <- colnames(p$data)
		select <- input$select
		sortCol <- input$sortCol
		if (length(select) && length(sortCol)) {
			if (sortCol %in% select && all(select %in% vars)) {
				decreasing <- input$decreasing
				fromto <- 100*input$fromto
		 		from <- fromto[1]
		 		to <- fromto[2]
				nBins <- max(2,as.numeric(input$nBins), na.rm=TRUE)
				tableplot( p, from=from, to=to
						 , sortCol = sortCol, select_string = select
						 , decreasing = decreasing, nBins=nBins
						 )
			}
		}
	})
})