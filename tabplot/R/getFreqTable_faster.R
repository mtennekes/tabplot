getFreqTable <- function(fac, aggIndex, nBins) {
	## determine categories and frequencies

	system.time({
		categories <- levels(fac)
		freqTable <- as.matrix(table( aggIndex
						  , fac
						  , useNA = "ifany"
						  )[1:nBins,])
					  
		if (ncol(freqTable) > length(categories)) {
			categories <- c(categories, "missing")
		}
	})
	
	tabulate(fac, nbins=nlevels(fac))
	
	return(list(freqTable=freqTable, categories=categories))
}