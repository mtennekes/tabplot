tableGUI_emptyVarTbl <- data.frame(Variable=character(0), Class=character(0), Levels=numeric(0), Type=character(0), 
	Scale=character(0), Sort=character(0), Pal=character(0), PalInitNr=numeric(0), Palette=character(0), Selected=logical(0), New=logical(0), stringsAsFactors=FALSE)

tableGUI_fillVarTbl <- function(DF, vars, sorts, scales, palettes) {
#browser()
	dfNames <- names(get(DF, envir=.GlobalEnv))
	dfClasses <- getClasses(dfNames, currentDF)
	dfLevels <- sapply(dfNames, FUN=function(x){nlevels(get(DF,envir=.GlobalEnv)[[x]])})
	
	dfTypes <- mapply(dfClasses, dfLevels, FUN=function(x,y) {
			ifelse(x=="factor", paste("categorical (", y, ")", sep=""),
				ifelse(x=="logical", "categorical (2)",
					ifelse(x %in% c("numeric", "integer"), "numeric", "unknown")))
		})
	
	dfScale <- sapply(dfClasses, FUN=function(x) {
			ifelse(x %in% c("numeric", "integer"), "auto", "")
		})
	
	dfPalette <- rep("default", length(dfNames))

	dfSort <- rep("", length(dfNames))
	
	if(length(vars)!=0) {
		
		indices <- sapply(vars, FUN=function(x,y){which(x==y)}, dfNames)
		
		dfScales[indices] <- scales
		dfSort[indices] <- sorts
		dfPalette <- palettes
	}
	
	varTbl <- data.frame(Variable=dfNames, Class=dfClasses, Levels=dfLevels, Type=dfTypes, Scale=dfScale, Sort=dfSort, Pal="Default", PalInitNr = 1, Palette = dfPalette, Selected = FALSE, Position=0, New = FALSE, stringsAsFactors=FALSE)
	return(varTbl)
}

	
tableGUI_init_data <- function(DF=character(0), vars=character(0), sorts=character(0), scales=character(0), palettes=character(0), e=e) {
	## receive list of loaded data.frames and its column names
	#browser()
	
	datlist <- lsDF()
	if (length(datlist)==0) stop("No data.frames loaded.")
	
	if(length(DF)==0) DF <- datlist[1]
	
	#allCols <- lsColnames()

	## setup GUI administration
	currentDF <- list(name=DF,
		nrow=nrow(get(DF, envir=.GlobalEnv)),
		class=class(get(DF, envir=.GlobalEnv)))
	varTbl <- tableGUI_fillVarTbl(DF, vars, sorts, scales, palettes)
	
	assign("datlist", datlist, envir=e)
	assign("currentDF", currentDF, envir=e)
	assign("varTbl", varTbl, envir=e)
	
}


tableGUI_getTbl1 <- function(vars=e$varTbl$Variable, cols=c("Variable", "Class"), e) {
	varTbl <- e$varTbl
	if (length(vars)==0) return(varTbl[NULL, cols])
	varTbl <- varTbl[sapply(vars, FUN=function(x,y){which(x==y)}, varTbl$Variable), ]
	return(varTbl[!varTbl$Selected, cols])
}

tableGUI_getTbl2 <- function(vars=e$varTbl$Variable, cols=c("Variable", "Type", "Scale", "Sort", "Palette"), e) {
	varTbl <- e$varTbl
	if (length(vars)==0) return(varTbl[NULL, cols])
	varTbl <- varTbl[sapply(vars, FUN=function(x,y){which(x==y)}, varTbl$Variable), ]
	return(varTbl[varTbl$Selected, cols])
}

tableGUI_getCurrentDFname <- function(e) {
	return(e$currentDF$name)
}

tableGUI_getCurrentDFnrow <- function(e) {
	return(e$currentDF$nrow)
}

tableGUI_getCurrentDFclass <- function(e) {
	return(e$currentDF$class)
}

tableGUI_getNumSel <- function(e) {
	return(sum(e$varTbl$Selected))
}

tableGUI_setVarTbl <- function(vars, cols, value, e) {
	varTbl <- e$varTbl
	varTbl[sapply(vars, FUN=function(x,y){which(x==y)}, varTbl$Variable), cols] <- value
	assign("varTbl", varTbl, envir=e)
}



tableGUI_refreshDF <- function(newDF, parent, e) {
	newvars <- tableGUI_saveVars(parent=parent, e=e)
	tableGUI_init_data(newDF, e)
}


## should newly created variable be kept?
tableGUI_saveVars <- function(vars=e$varTbl$Variable, parent=NULL, e) {
	
	newvars <- intersect(vars, e$varTbl$New)

	savevar <- TRUE
	if (length(newvars) != 0) {
		savevar <- gconfirm(message=paste("Do you want to keep the variable(s)", paste(newvars,collapse=", "), "?"), title="Save?", parent=parent, icon="question")
		if (!savevar) {
			tmpdat <- get(tableGUI_getCurrentDFname(e), envir=.GlobalEnv)
			for (x in newvars) {
				tmpdat[[x]] <- NULL
			}

			varTbl <- e$varTbl[!(e$varTbl$Variable %in% newvars),]
			assign("varTbl", varTbl, envir=e)
			assign(tableGUI_getCurrentDFname(e), tmpdat, envir=.GlobalEnv)
			return(character(0))
		} else {
			return(newvars)
		}
	}
}



## cast selected variable to a categorical variable
tableGUI_castToCat <- function(name, num_scale="", method="", n=0, brks=0, parent=wdw, e) {
	## add temporary column to data.frame
	currentDF <- tableGUI_getCurrentDFname(e)
	tmpdat <- get(currentDF, envir=.GlobalEnv)
	if (class(tmpdat[,name])[1] %in% c("numeric", "integer")) {
		tmpdat$tmptmp <- num2fac(tmpdat[, name], num_scale=num_scale, method=method, n=n, brks=brks)
		CLmethod <- paste("num2fac(", currentDF, "$", name, ", num_scale=\"", num_scale, "\", method=\"", method, "\", n=", n, ", brks=", brks, ")\n", sep="")
	} else {
		tmpdat$tmptmp <- as.factor(tmpdat[, name])
		lvls <- length(levels(tmpdat$tmptmp))
		if (lvls > 30) {
			gmessage(paste("There are too many (", lvls, ") categories.", sep=""), title = "Error", icon = "error", parent=parent)
			tmpdat$tmptmp <- NULL
			return(tableGUI_emptyVarTbl)
		}
		if (lvls > 15) {
			cont <- gconfirm(paste("There are", lvls, "categories. Do you want to continue?"), icon="question")
			if (!cont) {
				tmpdat$tmptmp <- NULL
				return(tableGUI_emptyVarTbl)
			}
		}
		CLmethod <- paste("as.factor(", currentDF, "$", name, ")\n", sep="")
	}
	
	newname <- paste(name, length(levels(tmpdat$tmptmp)),sep="")
	## check whether new name does not exists in data.frame
	tryCatch(
		while (newname %in% colnames(tmpdat)) {
			tempname <- newname
			newname <- ginput(paste("The variable name ", newname, " is already occupied in ", currentDF,". Please enter a new name.", sep=""), text=newname, title="New variable name", icon = "question")
			if (is.na(newname)) newname <- tempname
		}, finally= {
			if (!(newname %in% colnames(tmpdat))) {
				colnames(tmpdat)[ncol(tmpdat)] <- newname
				assign(currentDF, tmpdat, envir=.GlobalEnv)
						
				newRow <- data.frame(Variable=newname, Class="factor", Levels=nlevels(tmpdat[[newname]]), Type=paste("categorical (", nlevels(tmpdat[[newname]]),")", sep=""), Scale="", Sort="", Pal="Default", PalInitNr=1, Palette="default (1)", Selected=TRUE, Position=0, New=TRUE, stringsAsFactors=FALSE)
				
				# print command line
				cat(paste(currentDF, "$", newname, " <- ", CLmethod, sep=""))
				
				return(newRow)
			} else {
				return(tableGUI_emptyVarTbl)
			}
		})
}

## function to transfer variables from table1 to table2
tableGUI_selectVars <- function(vars, e, parent) {
	varTbl <- e$varTbl
	
	varId <- sapply(vars, FUN=function(x,y){which(x==y)}, varTbl$Variable)
	
	unknownId <- varId[which(varTbl$Type[varId]=="unknown")]
	
	## select variables of unknown classes, and ask to convert them (put them in newRows)
	newRows <- tableGUI_emptyVarTbl
	
	if (e$currentDF$class=="ffdf") {
		if (length(unknownId!=0)) gmessage(paste("The variable(s) ", varTbl$Variable[unknownId], " are not recognized as numeric or categorical.", sep=""), icon = "error", parent=parent)
	} else {
		for (i in varTbl$Variable[unknownId]) {
			cast <- gconfirm(paste("The variable ", i, " is not recognized as numeric or categorical. Do you want to cast it to a categorical?", sep=""), icon="question", parent=parent)
			if (cast) {
				newRows <- rbind(newRows, tableGUI_castToCat(i, e=e))
			}
		}
	}
	
	if (length(unknownId)!=0) {
		varId <- setdiff(varId, unknownId)
	}
	posMax <- sum(varTbl$Selected)
	varTbl[varId, "Position"] <- seq(posMax+1, posMax + length(varId))
	varTbl[varId, "Selected"] <- TRUE
	
	if (nrow(newRows)!=0) {
		newRows$Position <- seq(nrow(varTbl) + 1, nrow(varTbl) + nrow(newRows))
		varTbl <- rbind(varTbl, newRows)
	}
	cat("posMax", posMax, "\n")
	cat("n(varId)", length(varId), "\n")
	
	
	if ((length(varId)!=0) && posMax==0) {
		varTbl[varId[1], "Sort"] <- "\\/"
	}
	
	assign("varTbl", varTbl, envir=e)
	
	newVars <- c(varTbl[varId, "Variable"], newRows[, "Variable"])
	print(newVars)
	return(newVars)
}

## function to transfer variables from table2 back to table1
tableGUI_unselectVars <- function(vars, e) {
	varTbl <- e$varTbl
	
	varId <- sapply(vars, FUN=function(x,y){which(x==y)}, varTbl$Variable)

	varTbl$Selected[varId] <- FALSE
	varTbl$Sort[varId] <- ""

	
	assign("varTbl", varTbl, envir=e)
	


	## ask whether new variables should be kept
	## not yet implemented for ffdf
	if (class(get(tableGUI_getCurrentDFname(e),envir=.GlobalEnv))!="ffdf") {
		newvars <- c(vars, tableGUI_saveVars(vars, e=e))
	} else {
		newvars <- vars
	}
	
	return(newvars)
}


tableGUI_run <- function(vars, gui_from, gui_to, gui_nBins, e) {
	currentDFname <- tableGUI_getCurrentDFname(e)
	
	nBins <- min(gui_nBins, tableGUI_getCurrentDFnrow(e))
	
	
	
	# prepare scales
	scales <- tableGUI_getTbl2(vars=vars, cols="Scale", e=e)
	scales[scales==""] <- "auto"
	if (all(scales==scales[1])) {
		scales <- scales[1]
		scalesPrint <- paste("\"", scales, "\"", sep="")
	} else {
		scalesPrint <- paste("c(\"", paste(scales, collapse="\",\""),"\")", sep="")
	}

	# prepare sortCol and decreasing
	sorts <- tableGUI_getTbl2(vars=vars, cols="Sort", e=e)
	sortID <- sorts!=""
	sortColNames <- vars[sortID]
	
	sortCol <- sapply(sortColNames, FUN=function(x, y) which(x==y), vars)
	if (length(sortCol)==1) {
		sortColPrint <- as.character(sortCol)
	} else {
		sortColPrint <- paste("c(", paste(sortCol, collapse=","), ")", sep="")
	}
	
	decreasing <- sorts[sortID]=="\\/"
	if (length(decreasing)==1) {
		decreasingPrint <- decreasing
	} else {
		decreasingPrint <- paste("c(", paste(decreasing,collapse=","),")", sep="")
	}

	
	## print commandline to reproduce tableplot
	cat("tableplot(", currentDFname, ", colNames=c(", paste("\"",paste(vars,collapse="\",\""),"\"", sep=""), "), sortCol=", sortColPrint, ", decreasing=", decreasingPrint, ", scales=", scalesPrint, ", nBins=", nBins, ", from=", gui_from, ", to=", gui_to, ")\n", sep="")
	
	if (dev.cur()==1) {
		dev.new(width=min(11, 2+2*tableGUI_getNumSel(e)), height=7, rescale="fixed")
	}
	
	tableplot(get(currentDFname, envir=.GlobalEnv)[vars], sortCol=sortCol, decreasing=decreasing, scales=scales, nBins=nBins, from=gui_from, to=gui_to)
}





