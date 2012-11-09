tableplot_checkNumPals <- function(numPals, colNames, isNumber) {
	if ((class(numPals)!="character") || !all(numPals %in% c("Blues", "Greens", "Greys"))) stop("<numPals> is not correct")
	
	if (is.null(names(numPals))) {
		if (length(numPals) != length(colNames)) {
			numPals2 <- structure(rep(NA, length.out=length(colNames)), names=colNames)
			numPals2[isNumber] <- structure(rep(numPals, length.out=sum(isNumber)), names=colNames)
		} else {
			numPals2 <- structure(numPals, names=colNames)
		}
	} else {
		if (!all(names(numPals) %in% colNames)) stop("<numPals> is not correct")
		numPals2 <- structure(rep(NA, length(colNames)), names=colNames)
		numPals2[isNumber] <- "Blues"
		numPals2[names(numPals)] <- numPals
	}
	numPals2
}