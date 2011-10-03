getFreqTable_DT <- function(fac, aggIndex, useNA="ifany") {

	DT <- data.table(fac=fac, aggIndex=aggIndex)
	setkey(DT, aggIndex)
	
	DT_freq <- DT[, tabulate(fac, nbins=nlevels(fac)), by=aggIndex]
	
	freqTable <- matrix(DT_freq[!is.na(aggIndex), "V1", with=FALSE]$V1, ncol=nlevels(fac), byrow=TRUE)
	
	DT_miss <- DT[, sum(is.na(fac)), by=aggIndex]
	missings <- DT_miss[!is.na(aggIndex), "V1", with=FALSE]$V1
	
	if (any(missings!=0) | useNA=="always") {
		freqTable <- cbind(freqTable, missings)
		categories <- c(levels(fac), "missing")
	} else {
		categories <- levels(fac)
	}
	colnames(freqTable) <- categories
	
	
	return(list(freqTable=freqTable, categories=categories))
}