#' Show all palettes of the tabplot package
#'
#' All palettes in \code{\link{tabplotPalettes}} are shown in a new device.
#'
#' @aliases tableGUI_showAllPals
#' @export
#' @keywords color palettes
tableGUI_showAllPals <- function() {
	dev.new(width=6, height=4, rescale="fixed")

	## for package building, uncomment these rules
	# data("tabplotPalettes")
	# tabplotPalettes <- get("tabplotPalettes", pos=globalenv())
	
	k <- length(tabplotPalettes)
	ncols <- max(sapply(tabplotPalettes,FUN=length))

	pushViewport(viewport(layout=grid.layout(k+1, ncols+6)))
	for (i in 1:ncols) {
		pushViewport(viewport(layout.pos.col=i+6, layout.pos.row=1))		
		grid.text(i)
		popViewport()
	}
	for (j in 1:k) {
		pushViewport(viewport(layout.pos.col=1, layout.pos.row=j+1))		
		grid.text(names(tabplotPalettes)[j], just="left")
		popViewport()

		for (i in 1:ncols) {
			pushViewport(viewport(layout.pos.col=i+6, layout.pos.row=j+1))		
			grid.rect(height=0.66, gp=gpar(col=NA,fill=tabplotPalettes[[j]][i]))
			popViewport()
		}
	}
}