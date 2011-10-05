tableplot_checkPals <- function(pals) {

	if (class(pals)!="list") stop("<pals> is not a list")

	tabplotPalettes <- NULL; rm(tabplotPalettes); #trick R CMD check
	data("tabplotPalettes")
	
	palNames <- names(tabplotPalettes)
	palLengths <- nchar(palNames)
	
	getPal <- function(palName, startCol=1) {
		originalPal <- tabplotPalettes[[palName]]
		pal <- originalPal[startCol:length(originalPal)]
		if (startCol!=1) pal <- c(pal, originalPal[1:(startCol-1)])
		palList <- list(palette=pal, name=paste(palName, "(", startCol, ")", sep=""))
		return(palList)
	}

	
	palList <- lapply(pals, FUN=function(x, palN, palL){
		if (class(x) %in% c("numeric", "integer")) {
			if (x<1 || x>16) stop("<pals> number(s) should be between 1 and 16")
			return(getPal("default", x))
		} else {
			if (class(x)=="character" && length(x)==1) {
				checkPals <- mapply(palN, palL, FUN=function(palN, palL, x)substr(x, 1, palL)==palN, MoreArgs=list(x))
				if (sum(checkPals)==1) {
					whichPal <- which(checkPals)
					maxCol <- length(tabplotPalettes[[whichPal]])
					startCol <- as.integer(substr(x, palL[whichPal]+2, nchar(x)-1))
					if (is.na(startCol) || startCol<1 || startCol>maxCol) startCol <- 1
					return(getPal(palN[whichPal], startCol))
				}
			}
		
			if (class(try(col2rgb(x), silent=TRUE))=="try-error") {
				stop("<pals> color palette(s) are not correct")
			}
			return(list(palette=x, name="custom"))
		}
	}, palNames, palLengths)
	
	palN <- sapply(palList, FUN=function(x)x$name)
	palP <- lapply(palList, FUN=function(x)x$palette)
	
	return(list(name=palN, palette=palP))
}