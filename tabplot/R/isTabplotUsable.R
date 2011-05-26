## function that checks whether first argument, the tabplot object "tab", can be reused with the specifications according to the other arguments. Reusing a tabplot object is done by the function changeTabplot
isTabplotUsable <- function(tab, colNames, sortCol, decreasing, scales, nBins, from, to) {

	## check number of row bins
	if (nBins!=tab$nBins) return(FALSE)

	tabColNames <- sapply(tab$columns, function(col)col$name)
	colID <- match(colNames, tabColNames)
	
	## check is all column names are in tab
	if (any(is.na(colID))) return(FALSE)

	## check if the scales correspond
	if (any(scales!=tab$scales[colID])) return(FALSE)

	
	sortColNames <- colNames[sortCol]

	tabSort <- sapply(1:length(tab$columns), function(id) tab$columns[[id]]$sort)
	tabSortCols <- which(tabSort!="")

	## check if sort columns correspond
	if (!identical(sortColNames, tabColNames[tabSortCols])) return(FALSE)
	
	tabDecreasing <- tabSort[tabSortCols]=="decreasing"

	## check if sorting order corresponds (or to the flipped case)
	if (!(identical(tabDecreasing, decreasing) || identical(!tabDecreasing, decreasing))) return(FALSE)
	
	return(TRUE)
}