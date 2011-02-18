## cast selected variable to a categorical variable
tableGUI_castToCat <- function(name, num_scale="", method="", n=0, brks=0, parent=wdw) {
	## add temporary column to data.frame
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
			return(data.frame(Variable=character(0), Class=character(0), Levels=numeric(0), Type=character(0), Scale=character(0), Sort=character(0), Pal=character(0), PalInitNr=numeric(0), Selected=logical(0), New=logical(0), stringsAsFactors=FALSE))
		}
		if (lvls > 15) {
			cont <- gconfirm(paste("There are", lvls, "categories. Do you want to continue?"), icon="question")
			if (!cont) {
				tmpdat$tmptmp <- NULL
				return(data.frame(Variable=character(0), Class=character(0), Levels=numeric(0), Type=character(0), Scale=character(0), Sort=character(0), Pal=character(0), PalInitNr=numeric(0), Selected=logical(0), New=logical(0), stringsAsFactors=FALSE))
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
						
				newRow <- data.frame(Variable=newname, Class="factor", Levels=nlevels(tmpdat[[newname]]), Type=paste("categorical (", nlevels(tmpdat[[newname]]),")", sep=""), Scale="", Sort="", Pal="Default", PalInitNr=1, Selected=TRUE, New=TRUE, stringsAsFactors=FALSE)
				
				# print command line
				cat(paste(currentDF, "$", newname, " <- ", CLmethod, sep=""))
				
				return(newRow)
			} else {
				return(data.frame(Variable=character(0), Class=character(0), Levels=numeric(0), Type=character(0), Scale=character(0), Sort=character(0), Pal=character(0), PalInitNr=numeric(0), Selected=logical(0), New=logical(0), stringsAsFactors=FALSE))
			}
		})
}
