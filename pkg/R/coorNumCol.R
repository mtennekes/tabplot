coorNumCol <- function(tabCol, bias_brokenX) {
	num <- tabCol$mean.scaled
	
	## determine whether broken X-axis are applied, and transform values accordingly
	minmax <- range(num, na.rm=TRUE)
	if ((minmax[2]) > 0 && minmax[1] > (bias_brokenX * minmax[2])) {
		## broken x-axis has positive values
		brokenX <- 1
		values <- num - minmax[1]
	} else if ((minmax[1]) < 0 && minmax[2] < (bias_brokenX * minmax[1])) {
		## broken x-axis has negative values
		brokenX <- -1
		values <- num - minmax[2]
	} else {
		## x-axis not broken
		brokenX <- 0
		values <- num
	}
	
	## scale values to 0-1, and determine 0-1 value of the y-axis
	minV <- min(values, na.rm=TRUE)
	maxV <- max(values, na.rm=TRUE)
	if (minV < 0 && maxV > 0) {
		xline <- -minV / (maxV - minV)
		widths <- (values) / (maxV - minV)
	} else if (brokenX==1) {
		xline <- 0
		widths <- 0.3 + (values) * 0.7 / (maxV - minV)
	} else if (brokenX==-1) {
		xline <- 1
		widths <- -0.3 + (values) * 0.7 / (maxV - minV)
	} else {
		xline <- ifelse(maxV > 0, 0, 1)
		widths <- (values) / max(abs(minV), abs(maxV))
	}
	widths[is.nan(widths)] <- minV
	
	tabCol$brokenX <- brokenX
	tabCol$mean.brokenX <- values
	tabCol$xline <- xline
	tabCol$widths <- widths
	
	tabCol
}
