library(shiny)
library(tabplot)

obs <- ls(envir=.GlobalEnv)
dfs <- obs[sapply(obs, function(x)inherits(get(x, envir=.GlobalEnv), c("data.frame", "ffdf", "prepared")))]

cat("Preparing datasets...\n")
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

tablePlot <- function(p, numvars, input, plot=TRUE) {
	vars <- colnames(p$data)
	select <- input$select
	sortCol <- input$sortCol
	logscale <- input$logscale
	sampling <- input$sampling
	
	if (length(select) && length(sortCol)) {
		if (sortCol %in% select && all(select %in% vars)) {
			decreasing <- input$decreasing
			fromto <- 100*input$fromto
			from <- fromto[1]
			to <- fromto[2]
			nBins <- max(2,as.numeric(input$nBins), na.rm=TRUE)
			scales <- rep("lin", length(numvars))
			names(scales) <- numvars
			logscale <- logscale[logscale %in% select]
			if (length(logscale)) {
				scales[logscale] <- "log"
			}
			
			if (plot) {
				tableplot( p, from=from, to=to,
					sortCol = sortCol, select_string = select,
					decreasing = decreasing, 
					scales = scales,
					nBins=nBins, sample=sampling)
			} else {
				fromtoString <- ifelse(from==0 && to==100, "", paste0(", from=", from, ", to=", to))
				selectString <- ifelse(identical(select, vars), "", paste0(", select=c(", paste(select, collapse=","), ")"))
				scalesString <- ifelse(all(scales==scales[1]), paste0(", scales=\"", scales[1], "\""), paste0(", scales=c(", paste(paste(names(scales),"=", paste0("\"", scales, "\"")), collapse=", "), ")"))
				samplingString <- ifelse(sampling, ", sample=TRUE", "")
				
				paste0("tableplot(", input$dataset, 
					   ", sortCol=",	sortCol,
					  fromtoString, 
					  selectString,
					  ", decreasing=", decreasing, 
					  ", nBins=", nBins, 
					  scalesString, samplingString, ")\n")
			}
		}
	}
}


shinyServer(function(input, output) {
	
	dataset <- reactive({
		dfname <- ifelse(is.null(input$dataset), dfs[1], input$dataset)
		ps[[dfname]]
	})
	
	numvars <- reactive({
		if (length(input$select) && length(input$dataset)) {
			input$select[isNumber[[input$dataset]][input$select]]
		} else character(0)
	})
	

	output$df <- renderUI({
		p <- dataset()
		vars <- colnames(p$data)
		selectInput("dataset", label="Dataset:", choices=dfs)
	})
	
	output$selected <- renderUI({
		p <- dataset()
		vars <- colnames(p$data)
		checkboxGroupInput("select", label="Columns:", choices=vars, selected=vars)
	})

	output$sortOn <- renderUI({
		p <- dataset()
		vars <- colnames(p$data)
		choices <- if (length(input$select)) input$select else vars
		selectInput("sortCol", label="Sort on:", choices=choices)
	})
	
	output$logscale <-  renderUI({
		if (length(numvars)) {
			checkboxGroupInput("logscale", label="Log scale:", choices=numvars())
		}
	})
	
	output$rcode <- renderText({
		p <- dataset()
		numvars <- numvars()
		tablePlot(p, numvars, input, plot=FALSE)
	})
		
	output$plot <- renderPlot({
		p <- dataset()
		numvars <- numvars()
		tablePlot(p, numvars, input)
	})
})