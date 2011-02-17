	######################################################
	## internal GUI functions
	######################################################
	
tableGUIfunctions <- function(e) {
	with(e, {
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
					tbl2[1,4] <- "\\/"
				} else {
					index <- svalue(tbl2, index=TRUE)
					tbl2[] <- rbind(tbl2[], rows)
					svalue(tbl2, index=TRUE) <- index
					if (length(index) != 0) enabled(btnDown) <- TRUE
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
				tbl2[1,4] <- "\\/"
			}
		}
				
		## cast selected variable to a categorical variable
		castToCat <- function(name, num_scale="", method="", n=0, brks=0) {
			## add temporary column to data.frame
			tmpdat <- get(currentDF, envir=.GlobalEnv)
			if (class(tmpdat[,name])[1] %in% c("numeric", "integer")) {
				tmpdat$tmptmp <- num2fac(tmpdat[, name], num_scale=num_scale, method=method, n=n, brks=brks)
				CLmethod <- paste("num2fac(", currentDF, "$", name, ", num_scale=\"", num_scale, "\", method=\"", method, "\", n=", n, ", brks=", brks, ")\n", sep="")
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
								
						newRow <- data.frame(Variable=newname, Type=paste("categorical (", length(levels(tmpdat[[newname]])),")", sep=""), Scale="", Sort="", stringsAsFactors=FALSE)
						
						# print command line
						cat(paste(currentDF, "$", newname, " <- ", CLmethod, sep=""))
						
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
