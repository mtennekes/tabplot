tableGUI <-
function() {
    if (!require(gWidgetsRGtk2)){
		stop("This function requires gWidgetsRGtk2")
	}
	
	options("guiToolkit"="RGtk2")

	e <- environment()
	

	# load information about loaded data.frames
	tableGUI_init_data(e)	
	
	# create main GUI
	tableGUI_main_layout(e)
	
	## create window for num2fac  
	tableGUI_n2f_layout(e)
	
	# functions and handlers
    tableGUI_main_functions(e)
    tableGUI_main_handlers(e)
    tableGUI_n2f_handlers(e)
   

	######################################################
	## activate GUI
	######################################################
	tbl2[] <- data.frame(Variable=character(0), Type=character(0), Scale=character(0), Sort=character(0), stringsAsFactors=FALSE)
	svalue(sbr) <- "Ready"
	visible(wdw) <- TRUE
}