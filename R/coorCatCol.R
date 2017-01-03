coorCatCol <- function(tabCol, nBins) {
	ncategories <- ncol(tabCol$freq)
	widths <- tabCol$freq
	
	tabCol$x <- cbind(0,(matrix(apply(widths, 1, cumsum), 
								nrow=nBins,byrow=TRUE)[, -ncategories]))
	#tabCol$categories <- categories
	tabCol$widths <- widths
	tabCol
}