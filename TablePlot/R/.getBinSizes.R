.getBinSizes <- function(n, nBins) {
	#############################
	## Calculate bin sizes
	#############################

	normalBinSize <- n %/% nBins
	binSizes <- rep(normalBinSize, nBins)
	if ( (rest <- n %% nBins) > 0) {
		binSizes[1:rest] <- binSizes[1:rest] + 1
	}
	return(binSizes)
}