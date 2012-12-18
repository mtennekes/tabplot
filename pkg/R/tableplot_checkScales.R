tableplot_checkScales <- function(scales, colNames, isNumber) {
	if (length(setdiff(scales, c("auto", "lin", "log")))>0) stop("<scales> should consist of auto, lin and log")

	if (is.null(names(scales))) {
		if (length(scales) != length(colNames)) {
			scales2 <- structure(rep(NA, length(colNames)), names=colNames)
			scales2[isNumber] <- rep(scales, length.out=sum(isNumber))
		} else scales2 <- structure(scales, names=colNames)
	} else {
		if (!all(names(scales) %in% colNames)) stop("<scales> is not correct")
		scales2 <- structure(rep(NA, length(colNames)), names=colNames)
		scales2[isNumber] <- "auto"
		scales2[names(scales)] <- scales
	}
	scales2	
}