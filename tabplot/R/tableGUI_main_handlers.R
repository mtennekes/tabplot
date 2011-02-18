    ######################################################
	## add handlers
	######################################################
	
tableGUI_main_handlers <- function(e) {
#	browser()
	with(e, {	
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
					svalue(spbBins) <- nr
				} else if (nr < 10) {
					svalue(sbr) <- "Warning: only a few objects available. Number of row bins will be ignored."
					svalue(spbBins) <- nr
				} else {
					svalue(sbr) <- ""
					svalue(spbBins) <- min(nr, 100)
				}
				enabled(btnTransfer) <- FALSE
				#enabled(spbBins) <- TRUE
				#enabled(btnRun) <- FALSE
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
				enabled(btnRun) <- TRUE
				enabled(btnTransfer) <- FALSE
			} else {
				indices <- svalue(tbl2, index=TRUE)
				transRL(indices)
				svalue(btnTransfer)<-"<"
			}
			svalue(sbr) <- "Ready"
		})

		## change number of bins
		addHandlerKeystroke(spbBins, function(h,...) {
			enabled(btnRun) <- (svalue(spbBins) > 0 && nrow(tbl2)!=0)
		})
		addHandlerChanged(spbBins, function(h,...) {
			mx <- nrow(get(currentDF,envir=.GlobalEnv))
			if (svalue(h$obj) >  mx) {
				svalue(spbBins) <- mx
			}
			if (svalue(h$obj) <2 && as.integer(svalue(lbl5))>=2) {
				svalue(spbBins) <- 2
			}
		})

		## run!
		addHandlerClicked(btnRun, function(h,...) {
			enabled(btnRun) <- FALSE
			svalue(sbr) <- "Preparing tableplot..."
			
			from <- svalue(spbBinsFrom)
			to <- svalue(spbBinsTo)
			nBins <- min(svalue(spbBins), nrow(get(currentDF, envir=.GlobalEnv)))
			
			colNames <- tbl2[,1]
			
			
			# prepare scales
			scales <- tbl2[,3]
			scales[scales==""] <- "auto"
			if (all(scales==scales[1])) {
				scales <- scales[1]
				scalesPrint <- paste("\"", scales, "\"", sep="")
			} else {
				scalesPrint <- paste("c(\"", paste(scales,collapse="\",\""),"\")", sep="")
			}

			# prepare sortCol and decreasing
			sortID <- which(tbl2[,4]!="")
			sortColNames <- tbl2[sortID,1]
			sortCol <- sapply(sortColNames, FUN=function(x, y) which(x==y), colNames)
			if (length(sortCol)==1) {
				sortColPrint <- as.character(sortCol)
			} else {
				sortColPrint <- paste("c(", paste(sortCol, collapse=","), ")", sep="")
			}
			
			decreasing <- tbl2[sortID,4]=="\\/"
			if (length(decreasing)==1) {
				decreasingPrint <- decreasing
			} else {
				decreasingPrint <- paste("c(", paste(decreasing,collapse=","),")", sep="")
			}

			
			## print commandline to reproduce tableplot
			cat("tableplot(", currentDF, ", colNames=c(", paste("\"",paste(colNames,collapse="\",\""),"\"", sep=""), "), sortCol=", sortColPrint, ", decreasing=", decreasingPrint, ", scales=", scalesPrint, ", nBins=", nBins, ", from=", from, ", to=", to, ")\n", sep="")
			
			if (dev.cur()==1) {
				dev.new(width=min(11, 2+2*nrow(tbl2)), height=7, rescale="fixed")
			}
			
			tableplot(get(currentDF, envir=.GlobalEnv)[colNames], sortCol=sortCol, decreasing=decreasing, scales=scales, nBins=nBins, from=from, to=to)
			svalue(sbr) <- "Ready"
		})

		## move up
		addHandlerClicked(btnUp, function(h,...) {
			index <- svalue(tbl2, index=TRUE)
			tbl2temp <- tbl2[]
			for (i in index) {
				tbl2temp[c(i-1,i), ] <- tbl2temp[c(i,i-1), ]
			}
			tbl2[] <- tbl2temp
			svalue(tbl2,index=TRUE) <- index - 1
			enabled(btnRun) <- TRUE
		})

		## move down
		addHandlerClicked(btnDown, function(h,...) {
			index <- svalue(tbl2, index=TRUE)
			tbl2temp <- tbl2[]
			for (i in index[length(index):1]) {
				tbl2temp[c(i,i+1), ] <- tbl2temp[c(i+1,i), ]
			}
			tbl2[] <- tbl2temp
			svalue(tbl2,index=TRUE) <- index + 1
			enabled(btnRun) <- TRUE
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
			enabled(btnRun) <- TRUE
		})
		  
		## sort
		addHandlerClicked(btnSort, function(h,...) {
			index <- svalue(tbl2, index=TRUE)
			oldValues <- tbl2[index, 4]
			
			newValues <- oldValues
			newValues[oldValues== ""] <- "\\/"
			newValues[oldValues=="\\/"] <- "/\\"
			newValues[oldValues=="/\\"] <- ""
			tbl2[index, 4] <- newValues
			
			if (all(tbl2[,4]=="")) {
				svalue(sbr) <- "Warning: no colums are sorted"
				enabled(btnRun) <- FALSE
			} else {
				svalue(sbr) <- "Ready"
				enabled(btnRun) <- TRUE
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
				enabled(spbBinsFrom) <- TRUE
				enabled(lbl8) <- TRUE
				enabled(spbBinsTo) <- TRUE
				enabled(lbl9) <- TRUE
			} else {
				enabled(lbl7) <- FALSE
				enabled(spbBinsFrom) <- FALSE
				enabled(lbl8) <- FALSE
				enabled(spbBinsTo) <- FALSE
				enabled(lbl9) <- FALSE
				if (!((svalue(spbBinsFrom)==0) && (svalue(spbBinsTo)==100)))  enabled(btnRun) <- TRUE

				svalue(spbBinsFrom) <- 0
				svalue(spbBinsTo) <- 100
			}
		})

		## change <from>	
		addHandlerKeystroke(spbBinsFrom, function(h,...) {
			enabled(btnRun) <- TRUE
		})
		addHandlerChanged(spbBinsFrom, function(h,...) {
			if (svalue(h$obj) >= svalue(spbBinsTo)) {
				svalue(h$obj) <- max(0, (svalue(spbBinsTo)-1))
			}
		})
		
		
		## change <to>	
		addHandlerKeystroke(spbBinsTo, function(h,...) {
			enabled(btnRun) <- TRUE
		})
		addHandlerChanged(spbBinsTo, function(h,...) {
			if (svalue(h$obj) <= svalue(spbBinsFrom)) {
				svalue(h$obj) <- min(100, (svalue(spbBinsFrom)+1))
			}
		})
		

		  
		## click on table1
		addHandlerClicked(tbl1, function(h,...) {
			enabled(btnTransfer) <- as.integer(svalue(lbl5))>=2
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
	})
}
