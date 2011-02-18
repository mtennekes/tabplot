## function to transfer variables from table1 to table2
tableGUI_selectVars <- function(indices, parent=wdw) {
	varId <- which(!varTbl$Selected)[indices]


	unknownId <- varInd[which(varTbl$Type[varId]=="unknown")]
	
	## select variables of unknown classes, and ask to convert them (put them in newRows)
	newRows <- data.frame(Variable=character(0), Class=character(0), Levels=numeric(0), Type=character(0), Scale=character(0), Sort=character(0), Pal=character(0), PalInitNr=numeric(0), Selected=logical(0), New=logical(0), stringsAsFactors=FALSE)
	
	if (class(get(currentDF,envir=.GlobalEnv))=="ffdf") {
		if (length(unknownId!=0)) gmessage(paste("The variable(s) ", varTbl$Variable[unknownId], " are not recognized as numeric or categorical.", sep=""), icon = "error", parent=parent)
	} else {
		for (i in varTbl$Variable[unknownId]) {
			cast <- gconfirm(paste("The variable ", i, " is not recognized as numeric or categorical. Do you want to cast it to a categorical?", sep=""), icon="question", parent=parent)
			if (cast) {
				newRows <- rbind(newRows, castToCat(i))
			}
		}
	}
	
	if (length(unknown)!=0) {
		varId <- setdiff(varId, unknown)
		varTbl[varId, "Selected"] <- TRUE
	}
	
	if (length(newRows)!=) {
		varTbl <- rbind(varTbl, newRows)
	}
	
}
