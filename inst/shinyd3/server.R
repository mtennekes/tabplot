library(shiny)
library(tabplot)

obs <- ls(envir=.GlobalEnv)
dfs <- obs[sapply(obs, function(x)inherits(get(x, envir=.GlobalEnv), c("data.frame", "ffdf", "prepared")))]

ps <- lapply(dfs, function(d){
	dat <- get(d)
	if (inherits(dat, "prepared")) dat else tablePrepare(dat)
})
names(ps) <- dfs

isNumber <- lapply(ps, function(p){
	sapply(physical(p$data), function(col){
		vmode(col)!="logical" && !is.factor.ff(col)
	})
})


reactiveTablePlot <- function(func){
	reactive(function(){
		val <- func()
		names(val$columns) <- NULL
		val$dataset <- NULL
		val
	})
}

shinyServer(function(input, output) {
	
	dataset <- reactive(function(){
		dfname <- ifelse(is.null(input$dataset), dfs[1], input$dataset)
		ps[[dfname]]
	})
	
	numvars <- reactive(function(){
		if (length(input$select) && length(input$dataset)) {
			input$select[isNumber[[input$dataset]][input$select]]
		} else character(0)
	})
	

	output$df <- reactiveUI(function(){
		p <- dataset()
		vars <- colnames(p$data)
		selectInput("dataset", label="Dataset:", choices=dfs)
	})
	
	output$selected <- reactiveUI(function(){
		p <- dataset()
		vars <- colnames(p$data)
		checkboxGroupInput("select", label="Columns:", choices=vars, selected=vars)
	})

	output$sortOn <- reactiveUI(function(){
		p <- dataset()
		vars <- colnames(p$data)
		choices <- if (length(input$select)) input$select else vars
		selectInput("sortCol", label="Sort on:", choices=choices)
	})
	
	output$logscale <-  reactiveUI(function(){
		if (length(numvars)) {
			checkboxGroupInput("logscale", label="Log scale:", choices=numvars())
		}
	})
		
	output$plot <- reactivePlot(function() {
		p <- dataset()
		vars <- colnames(p$data)
		select <- input$select
		sortCol <- input$sortCol
		logscale <- input$logscale
		if (length(select) && length(sortCol)) {
			if (sortCol %in% select && all(select %in% vars)) {
				decreasing <- input$decreasing
				fromto <- 100*input$fromto
		 		from <- fromto[1]
		 		to <- fromto[2]
				nBins <- max(2,as.numeric(input$nBins), na.rm=TRUE)
				scales <- rep("lin", length(numvars()))
				names(scales) <- numvars()
				if (length(logscale)) {
					scales[logscale] <- "log"
				}
				if (input$showCode)
					cat("tableplot(", input$dataset, ", from=", from, ", to=", to, ", sortCol=",
					sortCol, ",select_string=c(", paste(paste("\"", select, "\"", sep=""), collapse=","), "), decreasing=", decreasing, ", scales=c(", paste(paste(names(scales),"=", paste("\"", scales, "\"", sep="")), collapse=", "), "), nBins=", nBins, ")\n")
				
				tableplot( p, from=from, to=to,
						   sortCol = sortCol, select_string = select,
						   decreasing = decreasing, 
						   scales = scales,
						   nBins=nBins
						 )
				
			}
		}
	})
	
	output$tpplot <- reactiveTablePlot(function(){
		p <- dataset()
		vars <- colnames(p$data)
		select <- input$select
		sortCol <- input$sortCol
		logscale <- input$logscale
		if (length(select) && length(sortCol)) {
			if (sortCol %in% select && all(select %in% vars)) {
				decreasing <- input$decreasing
				fromto <- 100*input$fromto
				from <- fromto[1]
				to <- fromto[2]
				nBins <- max(2,as.numeric(input$nBins), na.rm=TRUE)
				scales <- rep("lin", length(numvars()))
				names(scales) <- numvars()
				if (length(logscale)) {
					scales[logscale] <- "log"
				}
				if (input$showCode)
					cat("tableplot(", input$dataset, ", from=", from, ", to=", to, ", sortCol=",
						sortCol, ",select_string=c(", paste(paste("\"", select, "\"", sep=""), collapse=","), "), decreasing=", decreasing, ", scales=c(", paste(paste(names(scales),"=", paste("\"", scales, "\"", sep="")), collapse=", "), "), nBins=", nBins, ")\n")
				
				tableplot( p, from=from, to=to,
						   sortCol = sortCol, select_string = select,
						   decreasing = decreasing, 
						   scales = scales,
						   nBins=nBins, 
						   plot=FALSE
				)
				
			}
		}		
	})
})