tableplot_checkNcols <- function(nCols, colNames, sortCol) {
	if (!is.numeric(nCols)) stop("<ncolums> is not numeric")
	if (nCols < length(sortCol)) stop("<nCols> less than number of sorted columns")
	if (nCols == length(sortCol) && length(sortCol) < length(colNames)) 
		stop("<nCols> equal to number of sorted columns while number of selected columns is larger")
	
	ifelse(nCols > length(colNames), length(colNames), nCols)
}