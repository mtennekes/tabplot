tableGUI <-
function() {
    if (!require(gWidgetsRGtk2)){
		stop("This function requires gWidgetsRGtk2")
	}
	
	options("guiToolkit"="RGtk2")

	e <- environment()
	   
	## Internal function to receive all loaded data.frames
	lsDF <- function(envir=.GlobalEnv) {
		varNames <- ls(envir=envir)
		dfs <- sapply(varNames, function(i) inherits(get(i,envir=envir),c("data.frame", "ffdf"), which=FALSE)[1])
		if (length(dfs)==0) {
			return(character(0))
		} else {
			return(varNames[dfs])
		}
	}
	
	## Internal function to receive the column names from all loaded data.frames
	lsColnames <- function() {
		sapply(lsDF(),FUN=function(i) names(get(i)), simplify=FALSE)
	}

	## function to get classes
	getClasses <- function(vars) {
		n <- length(vars)
		if (class(get(currentDF,envir=.GlobalEnv))=="ffdf") {
			dfTypes <- character(n)
			tmp <- get(currentDF,envir=.GlobalEnv)
			ind <- sapply(vars, FUN=function(x,y){which(x==y)}, names(tmp))
			for (i in 1:n) {
				tmp <- get(currentDF,envir=.GlobalEnv)[[ind[i]]]
				dfTypes[i] <- ifelse(is.null(ramclass(tmp)), "numeric", ramclass(tmp)[1])
			}
		} else {
			dfTypes <- sapply(get(currentDF,envir=.GlobalEnv)[vars], FUN=function(x) class(x)[1])
		}
		return(dfTypes)
	}

	
	

	## receive list of loaded data.frames and its column names
	datlist <- lsDF()
	if (length(datlist)==0) stop("No data.frames loaded.")
	allCols <- lsColnames()

	######################################################
	## create GUI
	######################################################
	
	## create window
	wdw <- gwindow("Tableplot",visible=TRUE)
	sbr <- gstatusbar("Preparing...", cont=wdw)
	g <- gpanedgroup(cont=wdw)

	## create source frame
	ggg <- ggroup(horizontal = TRUE, cont = g, expand=TRUE)
	frm2 <- gframe(text="Source",horizontal = FALSE, cont = ggg) 
	size(frm2) <- c(250,400)
	grp4 <- ggroup(horizontal = FALSE, cont = frm2, expand=TRUE)
	grp9 <- ggroup(horizontal = TRUE, cont = grp4, expand=FALSE)
	lbl3 <- glabel("Data.frame:", cont=grp9)
	cmb <- gcombobox(datlist, cont=grp9)
	addSpring(grp9)
	btnReload <- gbutton("Reload", cont=grp9, expand=FALSE)

	## get selected dataframe and fill table 1
	currentDF <- svalue(cmb)
	dfNames <- names(get(currentDF,envir=.GlobalEnv))
	dfTypes <- getClasses(dfNames)
	tbl1 <- gtable(data.frame(Variable=dfNames, Class=dfTypes, stringsAsFactors=FALSE), multiple=TRUE, cont=grp4, expand=TRUE)

	grp10 <- ggroup(horizontal = TRUE, cont = grp4, expand=FALSE)
	lbl4 <- glabel("Number of Objects:", cont=grp10)
	lbl5 <- glabel(nrow(get(svalue(cmb), envir=.GlobalEnv)), cont=grp10) 

	## create transfer button
	grp8 <- ggroup(horizontal = FALSE, cont = ggg, anchor=c(-1, -1),expand=TRUE)
	addSpring(grp8)
	btnTransfer <- gbutton(">", cont=grp8, expand=TRUE); enabled(btnTransfer) <- FALSE
	addSpace(grp8, 100, horizontal=FALSE)

	## create config frame
	frm <- gframe(text="Tableplot Configuration",horizontal = FALSE, cont = ggg) 
	size(frm) <- c(350,400)
	grp6 <- ggroup(horizontal = FALSE, cont = frm, expand=TRUE) 
	#lbl3 <- glabel("Columns", cont=grp6)

	tbl2 <- gtable(data.frame(Variable=paste(rep(" ", 25), collapse=" "), Type= paste(rep(" ", 13), collapse=" "), Scale="", Sort="", stringsAsFactors=FALSE), multiple=TRUE, cont=grp6, expand=TRUE)

	grp7 <- ggroup(horizontal = TRUE, cont = grp6, expand=FALSE) 
	btn3 <- gbutton("Up", cont=grp7, expand=TRUE); enabled(btn3) <- FALSE
	btn4 <- gbutton("Down", cont=grp7, expand=TRUE); enabled(btn4) <- FALSE
	btnScale <- gbutton("Scale", cont=grp7, expand=TRUE); enabled(btnScale) <- FALSE
	btn5 <- gbutton("Sort", cont=grp7, expand=TRUE); enabled(btn5) <- FALSE
	btnAsCategory <- gbutton("As Categorical", cont=grp7, expand=TRUE); enabled(btnAsCategory) <- FALSE

	#lbl10 <- glabel("Rows", cont=grp6)
	
	grp2 <- ggroup(horizontal = TRUE, cont = grp6) 
	cbx <- gcheckbox(text="Zoom in", checked = FALSE, cont= grp2)
	lbl7 <- glabel("from", cont=grp2)
	spb2 <- gspinbutton(0, 100, by = 10, cont=grp2, expand=FALSE)
	svalue(spb2) <- 0
	
	lbl8 <- glabel("percent to", cont=grp2)
	spb3 <- gspinbutton(0, 100, by = 10, cont=grp2, expand=FALSE)
	svalue(spb3) <- 100
	lbl9 <- glabel("percent", cont=grp2)
	enabled(lbl7) <- FALSE
	enabled(spb2) <- FALSE
	enabled(lbl8) <- FALSE
	enabled(spb3) <- FALSE
	enabled(lbl9) <- FALSE
		
	grp1 <- ggroup(horizontal = TRUE, cont = grp6) 
	lbl1 <- glabel("Number of Row Bins:", cont=grp1)
	spb <- gspinbutton(0, 1000, by = 10, cont=grp1, expand=TRUE)
	svalue(spb) <- 100
	btn2 <- gbutton("Run", cont=grp1, expand=TRUE); enabled(btn2) <- FALSE

	
	
	## Create window for num2fac  
	wdw2 <- gwindow("As Categorical", parent=wdw, width=200, height=100, visible=FALSE)
	g2 <- gpanedgroup(cont=wdw2)
	
	grp16 <- ggroup(horizontal = TRUE, cont=g2)

	grp11 <- ggroup(horizontal = FALSE, cont=grp16)

	grp15 <- ggroup(horizontal = TRUE, cont=grp11)
	lbl4 <- glabel("Variable name:", cont=grp15)
	lbl6 <- glabel(text="", width=13, cont=grp15)
	
	grp12 <- ggroup(horizontal = TRUE, cont=grp11)

	n2f.method <- c("fixed", "pretty", "kmeans", "discrete")
	frm4 <- gframe(text="Method", cont=grp12)
	rad2 <- gradio(n2f.method, cont=frm4)

	n2f.scale <- c("lineair", "logarithmic", "automatic")
	frm3 <- gframe(text="Scale", cont=grp12)
	rad <- gradio(n2f.scale, cont=frm3)

	grp17 <- ggroup(horizontal = FALSE, cont=grp16)

	grp13 <- ggroup(horizontal = TRUE, cont=grp17)
	lbl2 <- glabel(text="Number of Categories:", cont=grp13)
	cmb2 <- gcombobox(c("auto", 2:9), cont=grp13)
	#edt <- gedit(text="", width=3, cont=grp13)

	grp14 <- ggroup(horizontal = TRUE, cont=grp17)
	frm6 <- gframe("Breaks", cont=grp14)
	lay2 <- glayout(container=frm6, spacing=0)
	for (i in 1:3) {
		for (j in 1:3) {
			lay2[j, i] <- gedit(text="", width=7)
		}
	}
	enabled(lay2) <- FALSE
	grp18 <- ggroup(horizontal = TRUE, cont=grp17)
	btnOK <- gbutton("OK", cont=grp18, expand=TRUE)
	btnCancel <- gbutton("Cancel", cont=grp18, expand=TRUE)
	name <- character(0)
	
	######################################################
	## internal functions
	######################################################
	
	## should newly created variable be kept?
	saveVars <- function(indices=rep(TRUE, nrow(tbl2)), parent=wdw) {

		vars <- tbl2[indices,1]
		newvarsID <- !(vars %in% allCols[[currentDF]])
		newvars <- vars[newvarsID]

		savevar <- TRUE
		if (length(newvars) != 0) {
			savevar <- gconfirm(message=paste("Do you want to keep the variable(s)", paste(newvars,collapse=", "), "?"), title="Save?", parent=parent, icon="question")
			if (savevar) {
				allCols[[currentDF]] <- c(allCols[[currentDF]], newvars)
				assign("allCols", allCols, envir=e)
			} else {
				tmpdat <- get(currentDF, envir=.GlobalEnv)
				for (x in newvars) {
					tmpdat[[x]] <- NULL
				}
				assign(currentDF, tmpdat, envir=.GlobalEnv)
			}
		}
		return(savevar)
	}
	
	## function to transfer variables from table1 to table2
	transLR <- function(indices) {
		
		classes <- getClasses(tbl1[indices,1])
		
		## select variables of unknown classes, and ask to convert them (put them in newRows)
		unknown <- which(!(classes %in% c("numeric", "integer", "factor", "logical")))
		newRows <- data.frame(Variable=character(0), Type=character(0), Scale=character(0), Sort=character(0),stringsAsFactors=FALSE)
		if (class(get(currentDF,envir=.GlobalEnv))=="ffdf") {
			if (length(unknown!=0)) gmessage(paste("The variable(s) ", tbl1[indices[unknown],1], " are not recognized as numeric or categorical.", sep=""), icon = "error", parent=wdw)
		} else {
			for (i in unknown) {
				cast <- gconfirm(paste("The variable ", tbl1[indices[i],1], " is not recognized as numeric or categorical. Do you want to cast it to a categorical?", sep=""), icon="question", parent=wdw)
				if (cast) {
					newRows <- rbind(newRows, castToCat(tbl1[indices[i],1]))
				}
			}
		}
		if (length(unknown)!=0) {
			indices <- indices[-unknown]
			classes <- classes[-unknown]
		}
		## determine whether variables of known classes can be transferred (put them in existingRows)
		if (length(indices)==0) {
			rows <- newRows
		} else {
			existingRows <- data.frame(Variable=tbl1[indices,1],stringsAsFactors=FALSE)
			
			existingRows$Type <- mapply(existingRows$Variable, classes, FUN=function(x, classX){
				if (classX=="factor") {
					lvls <- length(levels(get(currentDF, envir=.GlobalEnv)[[x]]))
					if (lvls > 30) {
						gmessage(paste("There are too many (", lvls, ") categories.", sep=""), title = "Error", icon = "error")
						return("error")
					} else return(paste("categorical (", lvls, ")", sep=""))
				} else if (classX=="logical") {
					return("categorical (2)")
				} else if (classX %in% c("numeric", "integer")) {
					return("numeric")
				} else {
					gmessage("Unknown class", title = "Error", icon = "error")
					return("error")
				}
			})
			errors <- existingRows$Variable[existingRows$Type =="error"]

			indices <- indices[which(!(tbl1[indices,1 ] %in% errors))]
			if (length(indices)==0) {
				rows <- newRows
			} else {
				existingRows <- existingRows[existingRows$Type !="error", ]
				existingRows$Scale <- ""
				existingRows$Scale[existingRows$Type=="numeric"] <- ifelse(class(get(currentDF,envir=.GlobalEnv))=="ffdf", "lin", "auto")
				existingRows$Sort <- ""
				if (nrow(newRows)==0) {
					rows <- existingRows
				} else {
					rows <- rbind(existingRows, newRows)
				}
				
				## remove objects from table1
				tbl1[] <- tbl1[-indices,,drop=FALSE]
			}
		}
		## add objects from table2
		if (nrow(rows)!=0) {
			if (nrow(tbl2)==0) {
				tbl2[] <- rows
				tbl2[1,4] <- "/\\"
			} else {
				index <- svalue(tbl2, index=TRUE)
				tbl2[] <- rbind(tbl2[], rows)
				svalue(tbl2, index=TRUE) <- index
				if (length(index) != 0) enabled(btn4) <- TRUE
			}
		}
	}

	
	## function to transfer variables from table2 back to table1
	transRL <- function(indices) {
		vars <- tbl2[indices,1]
		newvarsID <- !(vars %in% allCols[[currentDF]])
		newvars <- vars[newvarsID]
		indicesAdd <- indices
		
		## ask whether new variables should be kept
		## not yet implemented for ffdf
		if (class(get(currentDF,envir=.GlobalEnv))!="ffdf") {
			savevar <- saveVars(indices)
			if (!savevar) indicesAdd <- indices[!newvarsID]
		}
		
		## determine added content of table1
		newRows <- data.frame(Variable=tbl2[indicesAdd,1], stringsAsFactors=FALSE)
		newRows$Class <- getClasses(newRows$Variable)
		
		## add objects to table1
		if (nrow(tbl1)==0) {
			tbl1[] <- newRows
		} else {
			tbl1[] <- rbind(tbl1[], newRows)
		}
		
		## remove objects from table1
		tbl2[] <- tbl2[-indices,,drop=FALSE]
		if ((nrow(tbl2)!=0) && (all(tbl2[,4]==""))) {
			tbl2[1,4] <- "/\\"
		}
	}
			
	## cast selected variable to a categorical variable
	castToCat <- function(name, num_scale="", method="", n=0, brks=0) {
		## add temporary column to data.frame
		tmpdat <- get(currentDF, envir=.GlobalEnv)
		if (class(tmpdat[,name])[1] %in% c("numeric", "integer")) {
			tmpdat$tmptmp <- num2fac(tmpdat[, name], num_scale=num_scale, method=method, n=n, brks=brks)
		} else {
			tmpdat$tmptmp <- as.factor(tmpdat[, name])
			lvls <- length(levels(tmpdat$tmptmp))
			if (lvls > 30) {
				gmessage(paste("There are too many (", lvls, ") categories.", sep=""), title = "Error", icon = "error")
				tmpdat$tmptmp <- NULL
				return(data.frame(Variable=character(0), Type=character(0), Scale=character(0), Sort=character(0), stringsAsFactors=FALSE))
			}
			if (lvls > 15) {
				cont <- gconfirm(paste("There are", lvls, "categories. Do you want to continue?"), icon="question")
				if (!cont) {
					tmpdat$tmptmp <- NULL
					return(data.frame(Variable=character(0), Type=character(0), Scale=character(0), Sort=character(0), stringsAsFactors=FALSE))
				}
			}
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
							
					newRow <- data.frame(Variable=newname, Type=paste("categorical (", length(levels(tmpdat[[newname]])),")", sep=""), Scale="", Sort="", stringsAsFactors=FALSE)
					return(newRow)
				} else {
					return(data.frame(Variable=character(0), Type=character(0), Scale=character(0), Sort=character(0), stringsAsFactors=FALSE))
				}
			})
	}

	
	asCategoricalDialog <- function() {
		name <- get("name", envir=e)
		if (length(name)!=0) {
			varname <- name[1]
			svalue(sbr) <- paste("Transforming", varname, "to a categorical variable...")
			varID <- which(tbl2[,1]==varname)
			svalue(rad) <- ifelse(tbl2[varID,3]=="auto", "automatic", ifelse(tbl2[varID,3]=="log", "logarithmic", "lineair"))
			svalue(rad2) <- "pretty"
			svalue(lbl6) <- varname
			svalue(cmb2) <- 5
			
			visible(wdw2) <- TRUE
			enabled(wdw) <- FALSE
		} else {
			# check if GUI is not destroyed
			if (class(wdw@widget@widget)[1]!="<invalid>") {
				svalue(sbr) <- "Ready"	
				enabled(wdw) <- TRUE
				visible(wdw2) <- FALSE
			}
		}
	}
	
	changeTbl2 <- function() {
		if (nrow(tbl2)==0) {
			# disable run button
			enabled(btn2) <- FALSE
			# disable number of bins
			enabled(lbl1) <- FALSE
			enabled(spb) <- FALSE
			
			# disable button row under tbl2
			enabled(btn3) <- FALSE
			enabled(btn4) <- FALSE
			enabled(btn5) <- FALSE
			enabled(btnAsCategory) <- FALSE
			enabled(btnScale) <- FALSE
			
			# disable zoom line
			svalue(cbx) <- FALSE
			enabled(cbx) <- FALSE

		} else {
			# enable zoom
			enabled(cbx) <- TRUE
			# enable number of bins
			enabled(lbl1) <- TRUE
			enabled(spb) <- TRUE
			
			# check selected rows
			index <- svalue(tbl2, index=TRUE)
			if (length(index)!=0) {
				# enable buttons
				enabled(btn3) <- all(index > 1)
				enabled(btn4) <- all(index < nrow(tbl2))
				enabled(btn5) <- TRUE
				enabled(btnAsCategory) <- (any(substr(tbl2[index, 2],1,3)=="num") && class(get(currentDF,envir=.GlobalEnv))!="ffdf")
				enabled(btnScale) <- TRUE
				enabled(btnTransfer) <- TRUE
				svalue(btnTransfer) <- "<"
			} else {
				# disable button row
				enabled(btn3) <- FALSE
				enabled(btn4) <- FALSE
				enabled(btn5) <- FALSE
				enabled(btnAsCategory) <- FALSE
				enabled(btnScale) <- FALSE
			}
		}
	}
	
	
	######################################################
	## add handlers
	######################################################
	
	## select data.frame
	addHandlerChanged(cmb, handler = function(h,...) {
		saveVars()

		if (is.null(svalue(h$obj))) {
			svalue(h$obj) <- currentDF
		} else {
			dfName <- svalue(h$obj)
			assign("currentDF", dfName, envir=e)
			dfNames <- names(get(currentDF, envir=.GlobalEnv))
			dfTypes <- getClasses(dfNames)
			
			tbl1[] <- data.frame(Variable=dfNames, Class=dfTypes, stringsAsFactors=FALSE)
			tbl2[] <- data.frame(Variable=character(0), Type=character(0), Scale=character(0), Sort=character(0), stringsAsFactors=FALSE)
			nr <- nrow(get(dfName,envir=.GlobalEnv))
			svalue(lbl5) <- nr
			if (nr<2) {
				svalue(sbr) <- ifelse(nr==0, "Warning: no objects available.", "Warning: only one object available.")
				enabled(btnTransfer) <- FALSE
				svalue(spb) <- nr
				enabled(spb) <- FALSE
			} else if (nr < 10) {
				svalue(sbr) <- "Warning: only a few objects available. Number of row bins will be ignored."
				svalue(spb) <- nr
				enabled(spb) <- FALSE
			} else {
				svalue(sbr) <- ""
				svalue(spb) <- min(nr, 100)
				enabled(spb) <- TRUE
			}
			enabled(btn2) <- FALSE
		}
	})

	## refresh table1
	addHandlerClicked(btnReload, function(h,...) {
		datlist_new <- lsDF()
		if (length(datlist_new)==0) stop("No data.frames loaded.")
		allCols_new <- lsColnames()
		
		blockHandler(cmb)
		cmb[] <- datlist_new
		unblockHandler(cmb)
		
		if (!(currentDF %in% datlist_new)) {
			assign("currentDF", datlist_new[1], envir=e)
		}			
		
		#  bypass saveVars
		if (nrow(tbl2)!=0) {
			allCols[[currentDF]] <- tbl2[,1]
			assign("allCols", allCols, envir=e)
		}
			
		svalue(cmb) <- currentDF
		
		assign("datlist", datlist_new, envir=e)
		assign("allCols", allCols_new, envir=e)
		#changeTbl2()
	})
	
	## transfer variables
	addHandlerClicked(btnTransfer, function(h,...) {
		enabled(btnTransfer) <- FALSE
		svalue(sbr) <- "Transferring variable(s)..."
		if (svalue(btnTransfer)==">") {
			indices <- svalue(tbl1, index=TRUE)
			transLR(indices)
			svalue(btnTransfer)<-">"
			enabled(btn2) <- TRUE
			enabled(btnTransfer) <- FALSE
		} else {
			indices <- svalue(tbl2, index=TRUE)
			transRL(indices)
			svalue(btnTransfer)<-"<"
		}
		svalue(sbr) <- "Ready"
	})

	## change number of bins
	addHandlerKeystroke(spb, function(h,...) {
		enabled(btn2) <- (svalue(spb) > 0 && nrow(tbl2)!=0)
	})
	addHandlerChanged(spb, function(h,...) {
		mx <- nrow(get(currentDF,envir=.GlobalEnv))
		if (svalue(h$obj) >  mx) {
			svalue(spb) <- mx
		}
		if (svalue(h$obj) <2 && svalue(lbl5)>=2) {
			svalue(spb) <- 2
		}
	})

	## run!
	addHandlerClicked(btn2, function(h,...) {
		enabled(btn2) <- FALSE
		svalue(sbr) <- "Preparing tableplot..."
		
		from <- svalue(spb2)
		to <- svalue(spb3)
		nBins <- min(svalue(spb), nrow(get(currentDF, envir=.GlobalEnv)))
		
		colNames <- tbl2[,1]
		
		
		# prepare scales
		scales <- tbl2[,3]
		if (all(scales==scales[1])) {
			scales <- scales[1]
			if (scales=="") scales <- "auto"
			scalesPrint <- paste("\"", scales, "\"", sep="")
		} else {
			scales[scales==""] <- "auto"
			scalesPrint <- paste("c(\"", paste(scales,collapse="\",\""),"\")", sep="")
		}

		# prepare sortCol and decreasing
		sortID <- which(tbl2[,4]!="")
		sortColNames <- tbl2[sortID,1]
		sortCol <- sapply(sortColNames, FUN=function(x, y) which(x==y), colNames)
		sortColPrint <- paste("c(", paste(sapply(sortColNames, FUN=function(x, y) which(x==y), allCols[[currentDF]]), collapse=","), ")", sep="")
		
		decreasing <- tbl2[sortID,4]=="/\\"
		if (length(decreasing)==1) {
			decreasingPrint <- decreasing
		} else {
			decreasingPrint <- paste("c(", paste(decreasing,collapse=","),")", sep="")
		}

		
		## print commandline to reproduce tableplot
		cat("tableplot(", currentDF, ", colNames=c(", paste("\"",paste(colNames,collapse="\",\""),"\"", sep=""), "), sortCol=\"", sortColPrint, "\", decreasing=", decreasingPrint, ", scales=", scalesPrint, ", nBins=", nBins, ", from=", from, ", to=", to, ")\n", sep="")
		
		if (dev.cur()==1) {
			dev.new(width=min(11, 2+2*nrow(tbl2)), height=7, rescale="fixed")
		}
		
		tableplot(get(currentDF, envir=.GlobalEnv)[colNames], sortCol=sortCol, decreasing=decreasing, scales=scales, nBins=nBins, from=from, to=to)
		svalue(sbr) <- "Ready"
	})

	## move up
	addHandlerClicked(btn3, function(h,...) {
		index <- svalue(tbl2, index=TRUE)
		tbl2temp <- tbl2[]
		for (i in index) {
			tbl2temp[c(i-1,i), ] <- tbl2temp[c(i,i-1), ]
		}
		tbl2[] <- tbl2temp
		svalue(tbl2,index=TRUE) <- index - 1
		enabled(btn2) <- TRUE
	})

	## move down
	addHandlerClicked(btn4, function(h,...) {
		index <- svalue(tbl2, index=TRUE)
		tbl2temp <- tbl2[]
		for (i in index[length(index):1]) {
			tbl2temp[c(i,i+1), ] <- tbl2temp[c(i+1,i), ]
		}
		tbl2[] <- tbl2temp
		svalue(tbl2,index=TRUE) <- index + 1
		enabled(btn2) <- TRUE
	})
	  
	  
	## change scale
	addHandlerClicked(btnScale, function(h,...) {
		index <- svalue(tbl2, index=TRUE)
		oldValues <- tbl2[index, 3]
		
		newValues <- oldValues
		newValues[oldValues=="auto"] <- "lin"
		newValues[oldValues=="lin"] <- "log"
		newValues[oldValues=="log"] <- ifelse(class(get(currentDF,envir=.GlobalEnv))=="ffdf", "lin", "auto")
		tbl2[index, 3] <- newValues
		enabled(btn2) <- TRUE
	})
	  
	## sort
	addHandlerClicked(btn5, function(h,...) {
		index <- svalue(tbl2, index=TRUE)
		oldValues <- tbl2[index, 4]
		
		newValues <- oldValues
		newValues[oldValues== ""] <- "/\\"
		newValues[oldValues=="/\\"] <- "\\/"
		newValues[oldValues=="\\/"] <- ""
		tbl2[index, 4] <- newValues
		
		if (all(tbl2[,4]=="")) {
			svalue(sbr) <- "Warning: no colums are sorted"
			enabled(btn2) <- FALSE
		} else {
			svalue(sbr) <- "Ready"
			enabled(btn2) <- TRUE
		}
	})

	## as categorical
	addHandlerClicked(btnAsCategory, function(h,...) {
		enabled(btnAsCategory) <- FALSE
		nameID <- svalue(tbl2, index=TRUE)
		nameID <- nameID[which(tbl2[nameID,2]=="numeric")]
		name <- tbl2[nameID,1]
		
		nr <- nrow(tbl2)
		assign("name", name, envir=e)
		asCategoricalDialog()
	})
	
	## zoom in
	addHandlerChanged(cbx, function(h,...) {
		if (svalue(h$obj)) {
			enabled(lbl7) <- TRUE
			enabled(spb2) <- TRUE
			enabled(lbl8) <- TRUE
			enabled(spb3) <- TRUE
			enabled(lbl9) <- TRUE
		} else {
			enabled(lbl7) <- FALSE
			enabled(spb2) <- FALSE
			enabled(lbl8) <- FALSE
			enabled(spb3) <- FALSE
			enabled(lbl9) <- FALSE
			if (!((svalue(spb2)==0) && (svalue(spb3)==100)))  enabled(btn2) <- TRUE

			svalue(spb2) <- 0
			svalue(spb3) <- 100
		}
	})

	## change <from>	
	addHandlerKeystroke(spb2, function(h,...) {
		enabled(btn2) <- TRUE
	})
	addHandlerChanged(spb2, function(h,...) {
		if (svalue(h$obj) >= svalue(spb3)) {
			svalue(h$obj) <- max(0, (svalue(spb3)-1))
		}
	})
	
	
	## change <to>	
	addHandlerKeystroke(spb3, function(h,...) {
		enabled(btn2) <- TRUE
	})
	addHandlerChanged(spb3, function(h,...) {
		if (svalue(h$obj) <= svalue(spb2)) {
			svalue(h$obj) <- min(100, (svalue(spb2)+1))
		}
	})
	

	  
	## click on table1
	addHandlerClicked(tbl1, function(h,...) {
		enabled(btnTransfer) <- svalue(lbl5)>=2
		svalue(btnTransfer) <- ">"
	})
	
	# click on table2 
	addHandlerClicked(tbl2, function(h,...) {
		changeTbl2()
	})
	
	## quit GUI window
	addHandlerDestroy(wdw, function(h,...) {
		#dispose(wdw2)
		saveVars(parent=NULL)
	})

	################### handlers asCategoricalDialog
	
	## click on OK
	addHandlerClicked(btnOK, function(h,...) {
		varname <- get("name", envir=e)[1]
		
		num_scale <- c("lin", "log", "auto")[svalue(rad, index=TRUE)]
		method <- svalue(rad2)
		if (svalue(cmb2)=="auto") {
			n <- 0
		} else {
			n <- svalue(cmb2)
		}
		exit <- FALSE
		brks <- 0
		if (method=="fixed") {
			brks_char <- character(n+1)
			i <- 1
			j <- 1
			for (k in 1:(n+1)) {
				brks_char[k] <- svalue(lay2[j,i])
				i <- i+1
				if (i==4) {i<-1; j<-j+1}
			}
			nanCount <- sum(is.na(as.numeric(brks_char)))
			if (nanCount!=0) {
				exit <- TRUE
			} else {
				brks <- as.numeric(brks_char)
				for (k in 1:(length(brks)-1)) {
					if (brks[k] >= brks[k+1]) {
						exit <- TRUE
					}
				}
			}
		}
		if (!exit) {
			newRow <- castToCat(varname, num_scale=num_scale, method=method, n=n, brks=brks)
			tbl2[] <- rbind(tbl2[], newRow)

			name <- get("name", envir=e)
			name <- name[-1]
			assign("name", name, envir=e)
			enabled(btn2) <- TRUE
			asCategoricalDialog()
		} else {
			gmessage("The breaks are not correct", title="Breaks",icon = "warning")
		}
	})
	


	## close window
	addHandlerDestroy(wdw, function(h,...) {
		svalue(sbr) <- "Cancelled"
		name <- get("name", envir=e)
		name <- name[-1]
		assign("name", name, envir=e)
		asCategoricalDialog()
	})

	## click on cancel
	addHandlerClicked(btnCancel, function(h,...) {
		svalue(sbr) <- "Cancelled"
		name <- get("name", envir=e)
		name <- name[-1]
		assign("name", name, envir=e)
		asCategoricalDialog()
	})

	## change scale
	addHandlerChanged(rad, function(h,...) {
	})

	## change method
	addHandlerChanged(rad2, function(h,...) {
		if (svalue(h$obj)=="fixed") {
			tmp <- svalue(cmb2)
			cmb2[] <- 2:8
			if (tmp != "auto") svalue(cmb2) <- tmp else svalue(cmb2) <- 5
			enabled(lay2) <- TRUE
			for (i in 1:3) {
				enabled(lay2[1,i]) <- TRUE
				enabled(lay2[2,i]) <- TRUE
				enabled(lay2[3,i]) <- FALSE
			}
			enabled(rad) <- FALSE
			enabled(cmb2) <- TRUE
		} else if (svalue(h$obj)=="pretty") {
			tmp <- svalue(cmb2)
			cmb2[] <- c("auto", 2:8)
			svalue(cmb2) <- tmp
			for (i in 1:3) for (j in 1:3) svalue(lay2[j,i]) <- ""
			enabled(lay2) <- FALSE
			enabled(rad) <- TRUE
			enabled(cmb2) <- TRUE
		} else if (svalue(h$obj)=="kmeans") {
			tmp <- svalue(cmb2)
			cmb2[] <- 2:8
			if (tmp != "auto") svalue(cmb2) <- tmp else svalue(cmb2) <- 5
			for (i in 1:3) for (j in 1:3) svalue(lay2[j,i]) <- ""
			enabled(lay2) <- FALSE
			enabled(rad) <- TRUE
			enabled(cmb2) <- TRUE
		} else {
			enabled(lay2) <- FALSE
			enabled(rad) <- FALSE
			enabled(cmb2) <- FALSE
		}
	})

	## change number of categories
	addHandlerChanged(cmb2, function(h,...) {
		if (svalue(rad2)=="fixed") {
			if (!is.null(svalue(h$obj))) {
				n <- as.numeric(svalue(h$obj))
				i <- 1
				j <- 1
				fill <- TRUE
				for (k in 1:9) {
					if (fill) {
						enabled(lay2[j,i]) <- TRUE
						if (k==(n+1)) fill <- FALSE
					} else {
						svalue(lay2[j,i]) <- ""
						enabled(lay2[j,i]) <- FALSE
						
					}
					i <- i+1
					if (i==4) {i<-1; j<-j+1}
				}
			}
		}

	})


	
	######################################################
	## activate GUI
	######################################################
	tbl2[] <- data.frame(Variable=character(0), Type=character(0), Scale=character(0), Sort=character(0), stringsAsFactors=FALSE)
	svalue(sbr) <- "Ready"
	visible(wdw) <- TRUE
}

