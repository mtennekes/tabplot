tableGUI <-
function(dat=NULL, colNames=names(dat), sortCol=1,  decreasing=TRUE, scales="auto", pals=list(1, 9, 3, 10), nBins=100, from=0, to=100) {
    if (!require(gWidgetsRGtk2)){
		stop("This function requires gWidgetsRGtk2")
	}
	
	options("guiToolkit"="RGtk2")

	e <- environment()
#browser()
	#####################################
	## Check arguments
	#####################################
	## check datName
	if (!is.null(dat)) {
		datName <- deparse(substitute(dat))
		if (!exists(datName)) stop(paste(datName, "not loaded"))
		if(!(class(get(datName, envir=.GlobalEnv))%in% c("data.frame", "ffdf"))) stop(paste(datName, "not a data.frame or an ffdf object"))

		## Check colNames
		if (class(colNames)[1]!="character") stop("<colNames> is not a character(vector)")
		if (!all(colNames %in% names(get(datName, envir=.GlobalEnv)))) stop("<colNames> contains column names that are not found in <datName>")
		
		## get classes
		classes <- getClasses(colNames, datName)	
		whichCat <- which(classes %in% c("factor", "logical"))
		
		## Check sortCol, and (if necessary) cast it to indices
		sortCol <- tableplot_checkSortCol(sortCol, colNames, asIndex=TRUE)

		## Check decreasing vector
		decreasing <- tableplot_checkDecreasing(decreasing, sortCol)

		sortColFull <- rep("", length(colNames))
		sortColFull[sortCol] <- ifelse(decreasing, "\\/", "/\\")
		
		## Check scales
		scales <- tableplot_checkScales(scales, length(colNames))
		scales[classes %in% c("factor", "logical")] <- ""
		
		## Check palet indices
		palList <- tableplot_checkPals(pals)
		
		pal_names <- rep(palList$name, length.out=length(whichCat))
		isCustom <- pal_names=="custom"

		customPals <- rep(palList$palette, length.out=length(whichCat))[isCustom]
		names(customPals) <- colNames[whichCat][isCustom]
		
		palNames <- rep("", length(colNames))
		palNames[whichCat] <- pal_names
	} else {
		datName <- character(0)
		sortColFull <- character(0)
		palNames <- character(0)
	}

	
	# load information about loaded data.frames
	tableGUI_init_data(DF=datName, vars=colNames, sorts=sortColFull, scales=scales, palNames=palNames, customPals=customPals, e=e)
	

	# create main GUI
	tableGUI_main_layout(e)
		
	## create window for num2fac  
	tableGUI_n2f_layout(e)

	## create window for color palettes 
	tableGUI_pal_layout(e)

	
	# functions and handlers
    tableGUI_main_handlers(e)
	
	tableGUI_n2f_handlers(e)
   
	tableGUI_pal_handlers(e)
	
	######################################################
	## activate GUI
	######################################################
	#tbl2[] <- data.frame(Variable=character(0), Type=character(0), Scale=character(0), Sort=character(0), stringsAsFactors=FALSE)
	svalue(sbr) <- "Ready"
	visible(wdw) <- TRUE
}