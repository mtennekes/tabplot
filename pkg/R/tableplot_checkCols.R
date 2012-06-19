#' Function to check the tableplot argument: sortCol
#'
#' @aliases tableplot_checkCols
#' @param sortCol sortCol
#' @param colNames colNames
#' @return (possibly corrected) sortCol
#' @export
tableplot_checkCols <- function(sortCol, colNames) {	
	if (!missing(sortCol)) {
		nl <- as.list(seq_along(colNames))
		names(nl) <- colNames
		sortCol <- eval(sortCol, nl, parent.frame())
		if (is.character(sortCol)) sortCol <- which(sortCol==colNames)
	}
	
	tryCatch({
		if (any(sortCol > length(colNames))) stop("Incorrect sortCol indices")
	}, error=function(e) {
		if (getOption("show.error.messages")) cat("Incorrect sortCol argument\n")
	})
	
	
	return(sortCol)
}
