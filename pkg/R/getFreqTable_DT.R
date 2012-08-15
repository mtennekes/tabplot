getFreqTable_DT <- function(DT, col) {
	V1 <- aggIndex <-NULL;  rm(V1, aggIndex)
	categories <- levels(DT[, get(col)])
	nlev <- length(categories)
	
	freqTable <- matrix(DT[, tabulate(get(col), nbins=nlev),
						   by=aggIndex][,V1],
						ncol=nlev, byrow=TRUE)
	
	
	missings <- DT[, sum(is.na(get(col))), by=aggIndex][,V1]

	if (any(is.na(DT[, aggIndex]))) {
		freqTable <- freqTable[-1,]
		missings <- missings[-1]
	}
	
	if (any(missings!=0)) {
		freqTable <- cbind(freqTable, missings)
		categories <- c(categories, "missing")
	}
	
	colnames(freqTable) <- categories
	
	return(list(freqTable=freqTable, categories=categories))
}