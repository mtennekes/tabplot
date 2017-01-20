splitTab <- function(tab, nCols) {
	sortCols <- tab$sortCol
	otherCols <- setdiff(1L:tab$m, sortCols)
	
	nsort <- length(sortCols)
	nother <- length(otherCols)
	ntabs <- ceiling(nother / (nCols - nsort))
	
	otherColsID <- rep(1:ntabs, each=nCols-nsort, length.out=length(otherCols))
	colList <- split(otherCols, otherColsID)
	colList <- lapply(colList, function(x,y)c(y,x), sortCols)
	
	lapply(colList, function(cols) tableChange(tab, select=cols))
}
