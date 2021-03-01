#' Graphical User Interface to create tableplots
#'
#' This graphical user interface is developed with the \code{\link[shiny:shiny]{shiny}} package. All datasets that are loaded in the global workspace (\code{\link{data.frame}}, \code{\link[ff:ffdf]{ffdf}}, or \code{\link[tabplot:tablePrepare]{prepared}}) are passed on to the GUI.
#' 
#' This function replaces the old \code{tabplotGTK} package, since it only requires an up-to-date browser (and not software like GTK). Furthermore, maintanance is a lot easier.
#' @aliases itableplot
#' @examples
#' \dontrun{
#' require(ggplot2)
#' data(diamonds)
#' 
#' # load other datasets
#' data(iris)
#' data(cars)
#' 
#' itableplot()
#' }
#' @export
itableplot <- function() {
	dfs <- ps <- isNumber <- tablePlot <- NULL
	
	
	if (requireNamespace("shiny")) {
		
		shiny::shinyApp(
			ui = shiny::pageWithSidebar(
				
				# Application title
				shiny::headerPanel("Shiny Tableplot"),
				
				shiny::sidebarPanel(
					shiny::tabsetPanel(
						shiny::tabPanel("Data",
										shiny::uiOutput("df"),
										shiny::checkboxInput("sampling", label="Sampling", value=TRUE),
										shiny::uiOutput("selected"),
										shiny::uiOutput("sortOn"),
										shiny::checkboxInput("decreasing", label="Decreasing", value=TRUE),
										shiny::sliderInput("fromto", label="Range:", min=0, max=1, value=c(0,1), round=-1),
										shiny::numericInput("nBins", label="Number of bins:", value=100, min=2, max=500, step=1)),
						shiny::tabPanel("Numeric",
										shiny::uiOutput("logscale")),
						#tabPanel("Categorical"),
						#	uiOutput("palettes")),
						shiny::tabPanel("Output",
										shiny::p(shiny::textOutput("rcode"))))),
				
				shiny::mainPanel(
					shiny::plotOutput("plot", height="600px")
				)
			),
			server = function(input, output){
				dataset <- shiny::reactive({
					dfname <- ifelse(is.null(input$dataset), dfs[1], input$dataset)
					ps[[dfname]]
				})
				
				numvars <- shiny::reactive({
					if (length(input$select) && length(input$dataset)) {
						input$select[isNumber[[input$dataset]][input$select]]
					} else character(0)
				})
				
				
				output$df <- shiny::renderUI({
					# p <- dataset()
					# vars <- colnames(p$data)
					shiny::selectInput("dataset", label="Dataset:", choices=dfs)
				})
				
				output$selected <- shiny::renderUI({
					p <- dataset()
					vars <- colnames(p$data)
					shiny::checkboxGroupInput("select", label="Columns:", choices=vars, selected=vars)
				})
				
				output$sortOn <- shiny::renderUI({
					p <- dataset()
					vars <- colnames(p$data)
					choices <- if (length(input$select)) input$select else vars
					shiny::selectInput("sortCol", label="Sort on:", choices=choices)
				})
				
				output$logscale <-  shiny::renderUI({
					if (length(numvars)) {
						shiny::checkboxGroupInput("logscale", label="Log scale:", choices=numvars())
					}
				})
				
				output$rcode <- shiny::renderText({
					p <- dataset()
					numvars <- numvars()
					tablePlot(p, numvars, input, plot=FALSE)
				})
				
				output$plot <- shiny::renderPlot({
					p <- dataset()
					numvars <- numvars()
					tablePlot(p, numvars, input)
				})
			},
			onStart = function() {
				obs <- ls(envir=.GlobalEnv)
				dfs <- obs[sapply(obs, function(x)inherits(get(x, envir=.GlobalEnv), c("data.frame", "ffdf", "prepared")))]
				
				cat("Preparing datasets...\n")
				
				ps <- lapply(dfs, function(d){
					dat <- get(d)
					if (inherits(dat, "prepared")) {
						attr(dat, "close_this") <- FALSE
					} else {
						dat <- tablePrepare(dat)
						attr(dat, "close_this") <- TRUE
					}
					dat
				})
				names(ps) <- dfs
				
				isNumber <- lapply(ps, function(p){
					sapply(physical(p$data), function(col){
						vmode(col)!="logical" && !ff::is.factor(col)
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
				
				
				shiny::onStop(function() {
					cat("Doing application cleanup\n")
					lapply(ps, function(p) {
						if (attr(p, "close_this")) {
							close(p)
						}
					})
					invisible(NULL)
				})
			}
		)
	}
}