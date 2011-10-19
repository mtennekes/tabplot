#' Function to check the tableplot argument: number of bins.
#'
#' @aliases tableplot_checkBins
#' @param nBins the number of bins
#' @param nDat the number of data observations
#' @return the (possibly corrected) number of bins
#' @export
tableplot_checkBins <- function(nBins, nDat) {
	if (class(nBins)[1]!="numeric") stop("<nBins> is not numeric")
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