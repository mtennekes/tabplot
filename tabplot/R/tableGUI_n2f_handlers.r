    ######################################################
	## add handlers
	######################################################
	
tableGUI_n2f_handlers <- function(e) {
#	browser()
	with(e, {	
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
				newVars <- tableGUI_castToCat(varname, num_scale=num_scale, method=method, n=n, brks=brks, e=e)

				tbl2[] <- tableGUI_getTbl2(vars=c(tbl2[,1], newVars), e=e)

				name <- get("name", envir=e)
				name <- name[-1]
				assign("name", name, envir=e)
				enabled(btnRun) <- TRUE
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
	})
}
