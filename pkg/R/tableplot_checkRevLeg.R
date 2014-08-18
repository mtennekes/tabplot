tableplot_checkRevLeg <- function(rev_legend, colNames) {
	nCols <- length(colNames)
	if (!is.logical(rev_legend)) stop("<rev_legend> should be logical")
	if (is.null(names(rev_legend))) {
		rev_legend <- rep(rev_legend, length.out=nCols)
		names(rev_legend) <- colNames
	} else {
		if (!all(names(rev_legend) %in% colNames)) stop("Not all <rev_legend> names are selected columns.")
	}
	
	rev_legend2 <- rep(FALSE, nCols)
	names(rev_legend2) <- colNames
	rev_legend2[names(rev_legend)] <- rev_legend
	rev_legend2
}