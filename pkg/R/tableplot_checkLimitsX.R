tableplot_checkLimitsX <- function(limitsX, colNames) {
	if (is.null(names(limitsX))) {
		if (length(limitsX) != length(colNames)) {
			limits <- limitsX[rep(1:length(limitsX), length.out=length(colNames))]
		}
		names(limits) <- colNames
	} else {
		if (!all(names(limitsX) %in% colNames)) stop("<limitsX> is not correct")
		limits <- structure(as.list(rep(NA, length(colNames))), names=colNames)
		limits[names(limitsX)] <- limitsX
	}
	if (any(sapply(limits, function(l) !is.na(l) && (!is.numeric(l) || length(l)!=2 || l[2]<=l[1])))) stop("<limitsX> is not correct")
	
	limits	
}