tableplot_checkPals <- function(pals, convertDefault=TRUE) {

	if (class(pals)!="list") stop("<pals> is not a list")

	defaultPal <- c(brewer.pal(9,"Set1")[2:9], brewer.pal(8,"Set2"))

	pals <- lapply(pals, FUN=function(x, defaultPal){
		if (class(x) %in% c("numeric", "integer")) {
			if (x<1 || x>16) stop("<pals> number(s) should be between 1 and 16")
			if(convertDefault) {
				pal <- defaultPal[x:length(defaultPal)]
				if (x!=1) pal <- c(pal, defaultPal[1:(x-1)])
			} else {
				pal <- x
			}
			return(pal)
		} else {
			if (class(try(col2rgb(x), silent=TRUE))=="try-error") {
				stop("<pals> color palette(s) are not correct")
			}
			return(x)
		}
	}, defaultPal)
	return(pals)
}