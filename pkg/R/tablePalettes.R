#' Show / get all palettes of the tabplot package
#'
#' All color palettes are shown and/or returned that can be used for tableplots.
#'
#' Sequential palettes (for numeric variables):
#' \itemize{
#' \item Blues
#' \item Greens
#' \item Greys
#' }
#' These palettes are taken from ColorBrewer (Brewer et al., 2003).
#' 
#' Qualitative palattes (for categorical variables):
#' \itemize{
#' \item Set1
#' \item Set2
#' \item Set3
#' \item Set4
#' \item Set5
#' \item Set6
#' \item Set7
#' \item Paired
#' \item HCL1
#' \item HCL2
#' \item HCL3
#' }
#' The default palette, "Set1", is a colorblind-friendly palette (see Okabe and Ito, 2002). Palettes "Set2" to "Set6" and "Paired" are based on ColorBrewer palettes (Brewer et al., 2003). Palette "Set7", is a colorblind-friedly palette from the dichromat package (see Thomas Lumley , 2012). The HCL Palettes are based on the Hue-Chroma-Luminance color space model (see Zeileis et al., 2009). The color red has been removed from the orignal palettes, since it is occupied by missing values.
#'
#' @param plot Boolean that determines whether the palettes are plot.
#' @return list with palettes (silent output)
#' @export
#' @keywords color palettes
#' @references 
#' \href{http://jfly.iam.u-tokyo.ac.jp/color/}{Okabe, M. and Ito, K. Color Universal Design (CUD) - How to make figures and presentations that are friendly to Colorblind people, 2002}
#'
#' \href{http://colorbrewer2.org/}{Brewer, Cynthia A., Geoffrey W. Hatchard and Mark A. Harrower, 2003, ColorBrewer in Print: A Catalog of Color Schemes for Maps, Cartography and Geographic Information Science 30(1): 5-32.}
#'
#' \href{http://http://CRAN.R-project.org/package=dichromat/}{Thomas Lumley (2012). dichromat: Color schemes for dichromats. R package version 1.2-4. http://CRAN.R-project.org/package=dichromat}
#' 
#' \href{http://statmath.wu.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf}{Zeileis, A., Hornik, K.,  and Murrell, P.  Escaping RGBland: Selecting colors for statistical graphics.  In Proceedings of Computational Statistics & Data Analysis. 2009, 3259-3270.}
#' @export
#' @keywords color palettes
tablePalettes <- function(plot = TRUE) {
	
	if (plot) {
		tpal <- c(list(sequential=NA), tabplotPalettes$seq, list(qualitative=NA), tabplotPalettes$qual)
		
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
	}
	invisible(tabplotPalettes)
}