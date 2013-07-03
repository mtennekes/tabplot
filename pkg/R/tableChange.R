#' Change a \link{tabplot-object}
#'
#' Make layout changes in a \link{tabplot-object}, such as the order of columns, and color palettes.
#'
#' @aliases tableChange
#' @param tab \link{tabplot-object}
#' @param select index vector of the desired columns (column names are not supported)
#' @param select_string vector of names of the desired columns
#' @param decreasing determines whether the dataset is sorted decreasingly (\code{TRUE}) of increasingly (\code{FALSE}).
#' @param pals list of color palettes. Each list item is on of the following:
#' \itemize{
#' \item a palette name in \code{\link{tablePalettes}}, optionally with the starting color between brackets.
#' \item a palette vector
#' }
#' If the list items are unnamed, they are applied to all selected categorical variables (recycled if necessary). The list items can be assigned to specific categorical variables,
#' by naming them accordingly.
#' @param colorNA color for missing values
#' @param numPals name(s) of the palette(s) that is(are) used for numeric variables ("Blues", "Greys", or "Greens"). Recycled if necessary.
#' @return \link{tabplot-object}
#' @export
#' @example ../examples/tableChange.R

tableChange <- function(tab, select=NULL, select_string=tab$select, decreasing=NULL, pals=list(), colorNA = NULL, numPals = NULL) {

	## change order of columns
	currentColNames <- tab$select

	colID <- if (missing(select)) {
		 match(select_string, currentColNames)
	} else select
	
	if (any(is.na(colID))) stop("Unknown columns selected")

	tab2 <- tab
	tab2$m <- length(colID)
	tab2$select <- tab2$select[colID]
	tab2$isNumber <- tab2$isNumber[colID]
	tab2$columns <- tab2$columns[colID]
	
	## flip tabplot
	if (!missing(decreasing)) {
		if (decreasing!=tab2$decreasing) {
			tab2$binSizes <- rev(tab$binSizes)
			tab2$rows$heights <- tab$binSizes/tab$n
		    tab2$rows$y <- 1- c(0,cumsum(tab$binSizes/tab$n)[-tab$nBins])
			tab2$rows$marks <- rev(tab$rows$marks)
			
			tab2$decreasing <- !tab2$decreasing
			
			tab2$columns <- lapply(tab2$columns, function(col) {
				if (col$isnumeric) {
					col$mean <- rev(col$mean)
					col$compl <- rev(col$compl)
					#col$lower <- rev(col$lower)
					#col$upper <- rev(col$upper)
					col$mean.scaled <- rev(col$mean.scaled)
					col$mean.brokenX <- rev(col$mean.brokenX)
					col$widths <- rev(col$widths)
				} else {
					col$freq <- col$freq[nrow(col$freq):1,]		
					col$x <- col$x[nrow(col$x):1,]		
					col$widths <- col$widths[nrow(col$widths):1,]		
				}
				col
			})
		}
	}
	
	## change palettes
	if (length(pals)) {
		isChanged <- !tab2$isNumber
		if (length(pnames <- names(pals))) isChanged <- isChanged & (tab2$select %in% pnames)
		pals <- tableplot_checkPals(pals, colNames=tab2$select, isCat=isChanged)

		tab2$columns[isChanged] <- mapply(function(col, pal){
			col$paletname <- pal$name
			col$palet <- pal$palette
			col
		}, tab2$columns[isChanged], pals[isChanged], SIMPLIFY=FALSE)
	}

	## change colorNA
	if (!missing(colorNA)) {
		## Check colorNA
		if (class(try(col2rgb(colorNA), silent=TRUE))=="try-error") {
			stop("<colorNA> is not correct")
		}
		whichCategorical <- which(sapply(tab2$columns, FUN=function(col)!col$isnumeric))

		for (i in whichCategorical) {
			tab2$columns[[i]]$colorNA <- colorNA
		}
	}
	
	## change numeric palettes
	if (!missing(numPals)) {
		## Check numPals
		if ((class(numPals)!="character") || !all(numPals %in% c("Blues", "Greens", "Greys"))) stop("<numPals> is not correct")

		whichNumeric <- which(sapply(tab2$columns, FUN=function(col)col$isnumeric))	
		numPals <- rep(numPals, length.out=length(whichNumeric))
		paletNr <- 1
		for (i in whichNumeric) {
			tab2$columns[[i]]$paletname <- numPals[paletNr]
			paletNr <- paletNr + 1
		}
		
	}
	
	tab2
}