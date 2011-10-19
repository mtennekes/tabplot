#' Function to check the tableplot argument: sortCol
#'
#' @aliases tableplot_checkCols
#' @param sortCol sortCol
#' @param colNames colNames
#' @return (possibly corrected) sortCol
#' @export
tableplot_checkCols <- function(sortCol, colNames) {	
	colFunction <- deparse(substitute(sortCol))

	if (class(sortCol)[1]=="character") {
		if (!all(sortCol %in% colNames)) stop(paste("invalid", colFunction))
		sortCol <- sapply(sortCol, FUN=function(x) which(x==colNames))
	} else if (class(sortCol)[1] %in% c("numeric", "integer")) {
		if (any(sortCol > length(colNames)) || any(sortCol < 1)) {
			stop(paste(colFunction, "has an invalid value"))
		}
	} else {
		stop(paste(colFunction, "is not a character or numeric value or vector"))
	}
	return(sortCol)
}
