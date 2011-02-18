## should newly created variable be kept?
tableGUI_saveVars <- function(parent=wdw) {
	newvars <- varTbl[varTbl$New, "Variable"]

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
}
