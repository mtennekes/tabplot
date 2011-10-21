#' Print a \link{tabplot-object}
#'
#' @param x tabplot object
#' @param ... arguments passed to other methods
#' @export
#' @method print tabplot
print.tabplot <- function(x, ...) {
	str(x, max.level=2)
	invisible(x)
}