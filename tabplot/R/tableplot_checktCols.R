tableplot_checkCols <- function(col, colNames, asIndex=TRUE) {	
	colFunction <- deparse(substitute(col))

	if (class(col)[1]=="character") {
		if (!all(col %in% colNames)) stop(paste("invalid", colFunction))
		if (asIndex) {
			col <- sapply(col, FUN=function(x) which(x==colNames))
		}
	} else if (class(col)[1] %in% c("numeric", "integer")) {
		if (any(col > length(colNames)) || any(col < 1)) {
			stop(paste(colFunction, "has an invalid value"))
		}
		if (!asIndex) {
			col <- colNames[col]
		}
	} else {
		stop(paste(colFunction, "is not a character or numeric value or vector"))
	}
	return(col)
}
