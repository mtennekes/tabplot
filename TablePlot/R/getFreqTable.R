getFreqTable <- function(fac, aggIndex) {
	## determine categories and frequencies
	categories <- levels(fac)
	freqTable <- as.data.frame(table( aggIndex
					  , fac
					  , useNA = "ifany"
					  )[1:nBins,])
				  
	if (ncol(freqTable) > length(categories)) {
		categories <- c(categories, "missing")
	}
	names(freqTable) <- categories
	return(freqTable)
}
