tableplot_checkBins <- function(nBins, nDat) {
	if (!is.numeric(nBins)) stop("<nBins> is not numeric")
	if (nBins > nDat) { 
		if (nDat > 1000) {
			warning("Setting nBins (",nBins,") to 1000")
			nBins <- 1000
		} else {
			warning("Setting nBins (",nBins,") to number of rows (", nDat, ")")
			nBins <- nDat
		}
	}
	return(nBins)
}