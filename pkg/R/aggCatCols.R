aggCatCols <- 
function(dat, colNames, max_levels) {
	datFreq <- list()
	
	for (col in colNames) {
		col_orig <- col
		if (nlevels(dat[[col]]) > max_levels) {
			temp <- cut(1:nlevels(dat[[col]]), breaks=max_levels)
			
			
			lbinSizes <-	getBinSizes(nlevels(dat[[col]]), max_levels)
			lbrks <- c(0, cumsum(lbinSizes))
			brks_ind1 <- lbrks[1:max_levels]+1
			brks_ind2 <- lbrks[(1:max_levels)+1]
			
			brks_lab <- ifelse(brks_ind1==brks_ind2, 
				   levels(dat[[col]])[brks_ind1], 
				   paste0(levels(dat[[col]])[brks_ind1], "...",
				   	   levels(dat[[col]])[brks_ind2])
				   )
			
			tempCol <- factor(cut(as.numeric(dat[[col]]), lbrks, right=TRUE, labels=FALSE),
							  levels=1:max_levels,
							  labels=brks_lab)
			
			dat[, tempCol:=tempCol]
			col <- "tempCol"
		}
		datFreq[[col_orig]] <- getFreqTable_DT(dat[, c("aggIndex", col), with=FALSE], col)
		if (nlevels(dat[[col_orig]]) > max_levels) {
			dat[, tempCol:=NULL]
			if (tail(datFreq[[col_orig]]$categories, 1)=="missing") {
				datFreq[[col_orig]]$categories <- c(levels(dat[[col_orig]]), "missing")
			} else {
				datFreq[[col_orig]]$categories <- levels(dat[[col_orig]])
			}
		}
	}
	datFreq
}