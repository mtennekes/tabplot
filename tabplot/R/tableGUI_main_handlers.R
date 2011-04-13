    ######################################################
	## add handlers
	######################################################
	
tableGUI_main_handlers <- function(e) {

	with(e, {	
		## select data.frame
		addHandlerChanged(cmb, handler = function(h,...) {
			if (is.null(svalue(h$obj))) {
				svalue(h$obj) <- tableGUI_getCurrentDFname(e)
			}
			
			tableGUI_refreshDF(newDF=svalue(h$obj), wdw, e)
			
			tbl1[] <- tableGUI_getTbl1(e=e)
			tbl2[] <- tableGUI_getTbl2(e=e)
			
			nr <- tableGUI_getCurrentDFnrow(e)
			
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
			
		})

		## refresh table1
		addHandlerClicked(btnReload, function(h,...) {
			tableGUI_refreshDF(parent=wdw, e=e)

			cmb[] <- e$datlist
			
			tbl1[] <- tableGUI_getTbl1(e=e)
			tbl2[] <- tableGUI_getTbl2(e=e)

			svalue(cmb) <- tableGUI_getCurrentDFname(e)
		})
		
		## transfer variables
		addHandlerClicked(btnTransfer, function(h,...) {

			enabled(btnTransfer) <- FALSE
			svalue(sbr) <- "Transferring variable(s)..."
			if (svalue(btnTransfer)==">") {
				indices <- svalue(tbl1, index=TRUE)
				vars2 <- tableGUI_selectVars(tbl1[indices, 1], parent=wdw, e=e)
				if(nrow(tbl2)!=0) vars2 <- c(tbl2[,1], vars2)
				tbl2[] <- tableGUI_getTbl2(vars=vars2, e=e)

				vars1 <- setdiff(tbl1[,1], vars2)
				tbl1[] <- tableGUI_getTbl1(vars=vars1, e=e)
				svalue(btnTransfer)<-">"
				enabled(btnRun) <- TRUE
				enabled(btnTransfer) <- FALSE
			} else {
				indices <- svalue(tbl2, index=TRUE)
				vars <- tableGUI_unselectVars(tbl2[indices, 1], parent=wdw, e=e)
				
				if (nrow(tbl1)!=0) {
					vars1 <- c(tbl1[,1], vars)
				} else {
					vars1 <- vars
				}
				tbl1[] <- tableGUI_getTbl1(vars=vars1, e=e)
				
				vars2 <- setdiff(tbl2[,1], vars)
				tbl2[] <- tableGUI_getTbl2(vars=vars2, e=e)
				svalue(btnTransfer)<-"<"
			}
			svalue(sbr) <- "Ready"
		})

		## change number of bins
		addHandlerKeystroke(spbBins, function(h,...) {
			enabled(btnRun) <- (svalue(spbBins) > 0 && nrow(tbl2)!=0)
		})
		addHandlerChanged(spbBins, function(h,...) {
			mx <- tableGUI_getCurrentDFnrow(e)
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
			
			gui_from <- svalue(spbBinsFrom)
			gui_to <- svalue(spbBinsTo)
			gui_nBins <- svalue(spbBins)
			
			tableGUI_run(tbl2[,1], gui_from, gui_to, gui_nBins, e) 
			
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
			newValues[oldValues=="log"] <- ifelse(tableGUI_getCurrentDFclass(e)=="ffdf", "lin", "auto")
			tbl2[index, 3] <- newValues
			
			tableGUI_setVarTbl(vars=tbl2[index, 1], cols="Scale", value=newValues, e=e)
			
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
			
			tableGUI_setVarTbl(vars=tbl2[index, 1], cols="Sort", value=newValues, e=e)
			
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
		

		## colour palette
		addHandlerClicked(btnPal, function(h,...) {
			enabled(btnPal) <- FALSE
			svalue(sbr) <- "Configurating color palettes..."

			nameID <- svalue(tbl2, index=TRUE)
			nameID <- nameID[which(tbl2[nameID,2]!="numeric")]

					
			if (length(nameID)==0) nameID <- which(tbl2[,2]!="numeric")
			
			palVarName <- tbl2[nameID[1], 1]
			assign("palVarName", palVarName, envir=e)
			
			tableGUI_initPal(e) 
			enabled(btnRun) <- TRUE
	
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
			newvars <- tableGUI_saveVars(parent=NULL, e=e)
		})
		
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
				enabled(btnRun) <- FALSE
				# disable number of bins
				enabled(lbl1) <- FALSE
				enabled(spbBins) <- FALSE
				
				# disable button row under tbl2
				enabled(btnUp) <- FALSE
				enabled(btnDown) <- FALSE
				enabled(btnSort) <- FALSE
				enabled(btnAsCategory) <- FALSE
				enabled(btnPal) <- FALSE
				enabled(btnScale) <- FALSE
				
				# disable zoom line
				svalue(cbx) <- FALSE
				enabled(cbx) <- FALSE

			} else {
				# enable zoom
				enabled(cbx) <- TRUE
				# enable number of bins
				enabled(lbl1) <- TRUE
				enabled(spbBins) <- TRUE
				
				# check selected rows
				index <- svalue(tbl2, index=TRUE)
				if (length(index)!=0) {
					# enable buttons
					enabled(btnUp) <- all(index > 1)
					enabled(btnDown) <- all(index < nrow(tbl2))
					enabled(btnSort) <- TRUE
					
					enabled(btnAsCategory) <- (any(substr(tbl2[index, 2],1,3)=="num") && tableGUI_getCurrentDFclass(e)!="ffdf")
					enabled(btnPal) <- (any(substr(tbl2[1:nrow(tbl2), 2],1,3)=="cat"))

					enabled(btnScale) <- TRUE
					enabled(btnTransfer) <- TRUE
					svalue(btnTransfer) <- "<"
				} else {
					# disable button row
					enabled(btnUp) <- FALSE
					enabled(btnDown) <- FALSE
					enabled(btnSort) <- FALSE
					enabled(btnAsCategory) <- FALSE
					enabled(btnPal) <- FALSE
					enabled(btnScale) <- FALSE
				}
			}
		}
	})
}
