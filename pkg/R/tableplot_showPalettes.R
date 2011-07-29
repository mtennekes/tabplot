#' Show all palettes of the tabplot package
#'
#' All palettes in \code{\link{tabplotPalettes}} are shown in a new device.
#'
#' @aliases tableplot_showPalettes
#' @export
#' @keywords color palettes
tableplot_showPalettes <- function() {

	tabplotPalettes <- NULL; rm(tabplotPalettes); #trick R CMD check
	data("tabplotPalettes")
	
	k <- length(tabplotPalettes)
	ncols <- max(sapply(tabplotPalettes,FUN=length))
	names(tabplotPalettes)
	
	grid.newpage()
	mx <- 0.05 + max(convertWidth(stringWidth(names(tabplotPalettes)), "npc", valueOnly=TRUE))
	pushViewport(viewport(layout=grid.layout(k+1, ncols+1, 
		widths=c(mx, rep((1-mx)/ncols, ncols)),
		heights=unit(1, "lines")
		)))
	
	
	for (i in 1:ncols) {
		pushViewport(viewport(layout.pos.col=i+1, layout.pos.row=1))		
		grid.text(i)
		popViewport()
	}
	for (j in 1:k) {
		pushViewport(viewport(layout.pos.col=1, layout.pos.row=j+1))		
		grid.text(names(tabplotPalettes)[j], just="right", x=0.95)
		popViewport()

		for (i in 1:ncols) {
			pushViewport(viewport(layout.pos.col=i+1, layout.pos.row=j+1))		
			grid.rect(height=0.66, gp=gpar(col=NA,fill=tabplotPalettes[[j]][i]))
			popViewport()
		}
	}
}