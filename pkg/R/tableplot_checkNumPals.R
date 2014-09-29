tableplot_checkNumPals <- function(numPals, colNames, isNumber) {

	## check palette names
	signs <- substr(numPals, 1, 1)=="-"
	numPalsID <- numPals
	numPalsID[signs] <- substr(numPalsID[signs], 2, nchar(numPalsID[signs]))
	if ((class(numPalsID)!="character") || !all(numPalsID %in% names(tabplotPalettes$div))) stop("<numPals> is not correct")
	
	## check assignment to column names
	if (is.null(names(numPals))) {
		if (length(numPals) != length(colNames)) {
			numPals2 <- structure(rep(NA, length.out=length(colNames)), names=colNames)
			numPals2[isNumber] <- rep(numPals, length.out=sum(isNumber))
		} else {
			numPals2 <- structure(numPals, names=colNames)
		}
	} else {
		if (!all(names(numPals) %in% colNames)) stop("<numPals> is not correct")
		numPals2 <- structure(rep(NA, length(colNames)), names=colNames)
		numPals2[isNumber] <- "RdYlBu"
		numPals2[names(numPals)] <- numPals
	}
	numPals2
}