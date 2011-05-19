#' Function to change the palettes of an tabplot object.
#'
#' Changes the palettes of a tabplot object and returns a modified tabplot object.
#'
#' @aliases changePalettes
#' @param tab tabplot object
#' @param pals list of color palettes. Each list item is on of the following:
#' \itemize{
#' \item a index number between 1 and 16. In this case, the default palette is used with the index number being the first color that is used.
#' \item a palette name in \code{\link{tabplotPalettes}}, optionally with the starting color between brackets.
#' \item a palette vector
#' }
#' @return tabplot object
#' @export
changePalettes <- 
function(tab, pals) {
	## Check palet indices
	pals <- tableplot_checkPals(pals)$palette

	whichCategorical <- which(sapply(tab$columns, FUN=function(col)!col$isnumeric))

	paletNr <- 1
	for (i in whichCategorical) {
		tab$columns[[i]]$palet <- pals[[paletNr]]
		paletNr <- ifelse(paletNr==length(pals), 1, paletNr + 1)
	}

	return(tab)
}