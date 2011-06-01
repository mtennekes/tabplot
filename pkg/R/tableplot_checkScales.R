tableplot_checkScales <- function(scales) {
	if (length(setdiff(scales, c("auto", "lin", "log")))>0) stop("<scales> should consist of auto, lin and log")
	return(scales)
}