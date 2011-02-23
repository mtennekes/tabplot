tableplot_checkSortCol <- function(sortCol, colNames, asIndex=TRUE) {	
	if (class(sortCol)[1]=="character") {
		if (!all(sortCol %in% colNames)) stop("invalid <sortCol>")
		if (asIndex) {
			sortCol <- sapply(sortCol, FUN=function(x) which(x==colNames))
		}
	} else if (class(sortCol)[1] %in% c("numeric", "integer")) {
		if (any(sortCol > length(colNames)) || any(sortCol < 1)) {
			stop("<sortCol> has an invalid value")
		}
		if (!asIndex) {
			sortCol <- colNames[sortCol]
		}
	} else {
		stop("<sortCol> is not a character or numeric value or vector")
	}
	return(sortCol)
}
