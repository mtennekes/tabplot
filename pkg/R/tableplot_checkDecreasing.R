tableplot_checkDecreasing <- function(decreasing, sortCol){
	if (class(decreasing)[1]!="logical") stop("<decreasing> is not a logical")
	if (length(decreasing)==1) {
		decreasing <- rep(decreasing, length(sortCol))
	} else if (length(decreasing) != length(sortCol)) stop("<sortCol> and <decreasing> have different lengths")
	decreasing
}