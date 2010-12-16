.getFreqTable <- function(aggIndex, fac) {
	## determine categories and frequencies
	categories <- levels(fac)
	freqTable <- table( aggIndex
					  , fac
					  , useNA = "ifany"
					  )[1:nBins,]
				  
	if (ncol(freqTable) > length(categories)) {
		categories <- c(categories, "missing")
	}
	names(freqTable) <- categories
}
