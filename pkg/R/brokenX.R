brokenX <- function(num, bias_brokenX) {
## bias_brokenX: parameter between 0 en 1 that determines when x-axis is broken
##               if minimum value is at least <bias_brokenX> times the maximum value, then X axis is broken

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
	return(list(brokenX=brokenX, values=values))
}