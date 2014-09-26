tableplot_checkPals <- function(pals, colNames, isCat) {
	if (class(pals)!="list") stop("<pals> is not a list")

	catCols <- colNames[isCat]
	
	if (is.null(names(pals))) {
		pals2 <- rep(pals, length.out=length(catCols))
		names(pals2) <- catCols
	} else {
		if (!all(names(pals) %in% catCols)) stop("<pals> is not correct")
		pals2 <- structure(as.list(rep("Set1", length(catCols))), names=catCols)
		pals2[names(pals)] <- pals
	}
	
	allpals <- c(tabplotPalettes$qual, tabplotPalettes$div)
	
	getPal <- function(name, s) {
		pal <- allpals[[name]]
		list(palette=if (s==1) pal else rep(pal, length.out=length(pal)+s-1)[-(1:(s-1))],
			 name=paste(name, "(", s, ")", sep=""))
	}
	
	palList <- lapply(pals2, function(p){
		if (class(p)=="character" && length(p)==1) {
			s <- ifelse(substr(p, nchar(p),nchar(p))==")", 
						as.integer(substr(p, nchar(p)-1,nchar(p)-1)), NA)
			name <- ifelse(is.na(s), p, substr(p, 1, nchar(p)-3))
			if (is.na(s)) s <- 1
			if (!name %in% names(allpals)) stop("<pals> is not correct")
			getPal(name, s)
		} else {
			if (class(try(col2rgb(p), silent=TRUE))=="try-error") {
				stop("<pals> color palette(s) are not correct")
			}
			list(palette=p, name="custom")
		}
	})
	
	l <- as.list(rep(NA, length(colNames)))
	names(l) <- colNames
	l[isCat] <- palList
	l
}