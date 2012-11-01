getBinSizes <- function(n, nBins, decreasing = FALSE) {

# 	normalBinSize <- n %/% nBins
# 	binSizes <- rep(normalBinSize, nBins)
# 	if ( (rest <- n %% nBins) > 0) {
# 		binSizes[1:rest] <- binSizes[1:rest] + 1
# 	}
# 	return(binSizes)
	
	# consistent with (improved) binRanges:
	r <- as.integer(seq(1, n+1, length.out=nBins+1))

	f <- r[-(nBins+1)]
	t <- r[-1] - 1L
	
	bins <- (t-f)+1
	if (decreasing) bins <- rev(bins)
	bins
}