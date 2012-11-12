tableplot_checkLimitsX <- function(limitsX, colNames, isNumber) {
	if (any(sapply(limitsX, function(l) !is.na(l) && (!is.numeric(l) || length(l)!=2 || l[2]<=l[1])))) stop("<limitsX> is not correct")

	if (is.null(names(limitsX))) {
		if (length(limitsX) != length(colNames)) {
			limitsX2 <- structure(as.list(rep(NA, length(colNames))), names=colNames)
			limitsX2[isNumber] <- rep(limitsX, length.out=sum(isNumber))
		} else limitsX2 <- structure(limitsX, names=colNames)
	} else {
		if (!all(names(limitsX) %in% colNames)) stop("<limitsX> is not correct")
		limitsX2 <- structure(as.list(rep(NA, length(colNames))), names=colNames)
		limitsX2[names(limitsX)] <- limitsX
	}
	
	limitsX2	
}