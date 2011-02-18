## function to transfer variables from table2 back to table1
tableGUI_unselectVars <- function(indices) {
	varId <- which(varTbl$Selected)[indices]

	## ask whether new variables should be kept
	## not yet implemented for ffdf
	if (class(get(currentDF,envir=.GlobalEnv))!="ffdf") {
		saveVars()
	}
	
}
