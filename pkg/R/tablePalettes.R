#' Show all palettes of the tabplot package
#'
#' All palettes in \code{\link{tabplotPalettes}} are shown.
#'
#' @export
#' @keywords color palettes
tablePalettes <- function() {

	#tabplotPalettes <- NULL; rm(tabplotPalettes); #trick R CMD check
	#data("tabplotPalettes")
	
	tpal <- c(list(qualitative=NA), tabplotPalettes$qual, list(sequential=NA), tabplotPalettes$seq)
	
	k <- length(tpal)
	ncols <- max(sapply(tpal,FUN=length))
	
	grid.newpage()
	mx <- 0.05 + max(convertWidth(stringWidth(names(tpal)), "npc", valueOnly=TRUE))
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
		grid.text(names(tpal)[j], just="right", x=0.95)
		popViewport()

		for (i in 1:ncols) {
			pushViewport(viewport(layout.pos.col=i+1, layout.pos.row=j+1))		
			grid.rect(height=0.66, gp=gpar(col=NA,fill=tpal[[j]][i]))
			popViewport()
		}
	}
	invisible(tabplotPalettes)
}