#' Function to check the tableplot argument: scales
#'
#' @aliases tableplot_checkScales
#' @param scales scales
#' @return scales
#' @export
tableplot_checkScales <- function(scales) {
	if (length(setdiff(scales, c("auto", "lin", "log")))>0) stop("<scales> should consist of auto, lin and log")
	scales
}