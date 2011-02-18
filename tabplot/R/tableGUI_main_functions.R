	######################################################
	## internal GUI functions
	######################################################
	
tableGUI_main_functions <- function(e) {
	with(e, {
		
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
					enabled(btnAsCategory) <- (any(substr(tbl2[index, 2],1,3)=="num") && class(get(currentDF,envir=.GlobalEnv))!="ffdf")
					enabled(btnScale) <- TRUE
					enabled(btnTransfer) <- TRUE
					svalue(btnTransfer) <- "<"
				} else {
					# disable button row
					enabled(btnUp) <- FALSE
					enabled(btnDown) <- FALSE
					enabled(btnSort) <- FALSE
					enabled(btnAsCategory) <- FALSE
					enabled(btnScale) <- FALSE
				}
			}
		}
	})
}
