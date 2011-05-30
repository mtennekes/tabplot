tableplot_checkScales <- function(scales, n) {
	if (length(scales)==1) scales <- rep(scales, n)
	if (length(scales)!=n) stop(paste("<scales> should be of length ", n))
	if (length(setdiff(scales, c("auto", "lin", "log")))>0) stop("<scales> should consist of auto, lin and log")
	return(scales)
}