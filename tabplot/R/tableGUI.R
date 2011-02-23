tableGUI <-
function(datName=character(0), colNames=character(0), sortCol=1,  decreasing=TRUE, scales="auto", pals=list(1, 9, 3, 10), nBins=100, from=0, to=100) {
    if (!require(gWidgetsRGtk2)){
		stop("This function requires gWidgetsRGtk2")
	}
	
	options("guiToolkit"="RGtk2")

	e <- environment()
	
	#####################################
	## Check arguments
	#####################################
	
	## check datName
	if (length(datName)==1) {
		if (!exists(datName)) stop(paste(datName, "not loaded"))
		if(!(class(get(datName, envir=.GlobalEnv))%in% c("data.frame", "ffdf"))) stop(paste(datName, "not a data.frame or an ffdf object"))
	}
	
	if (length(colNames)!=0) {
		## Check colNames
		if (class(colNames)[1]!="character") stop("<colNames> is not a character(vector)")
		if (!all(colNames %in% names(get(datName, envir=.GlobalEnv)))) stop("<colNames> contains column names that are not found in <datName>")
	
		## Check sortCol, and (if necessary) cast it to column names
		sortCol <- tableplot_checkSortCol(sortCol, colNames, asIndex=TRUE)

		## Check decreasing vector
		decreasing <- tableplot_checkDecreasing(decreasing, sortCol)

		temp <- ifelse(mapply(sortCol, decreasing, FUN=function(x,y,z) {
				
			}, MoreArgs=list(colNames)),)
		
		sortColFull <- rep("", length(colNames))
		sortColFull[sortCol] <- ifelse(decreasing, "\\/", "/\\")
		
		## Check scales
		scales <- tableplot_checkScales(scales, n)

	}

	
	## Check palet indices
	palNames <- lapply(pals, FUN=function(x){
		if (class(x) %in% c("numeric", "integer")) {
			return(paste("default(", x, ")", sep=""))
		} else {
			return("custom")
		})
	pals <- tableplot_checkPals(pals, convertDefault=FALSE)

	# load information about loaded data.frames
	tableGUI_init_data(DF=datName, vars=colNames, sorts=sortColFull, scales=, palettes=pals, e=e)
	
	
	# create main GUI
	tableGUI_main_layout(e)
	
		
	## create window for num2fac  
	
	#tableGUI_n2f_layout(e)
	
	# functions and handlers
    tableGUI_main_handlers(e)
	
	#tableGUI_n2f_handlers(e)
   

	######################################################
	## activate GUI
	######################################################
	#tbl2[] <- data.frame(Variable=character(0), Type=character(0), Scale=character(0), Sort=character(0), stringsAsFactors=FALSE)
	svalue(sbr) <- "Ready"
	visible(wdw) <- TRUE
}