#' Changes a \link{tabplot-object}
#'
#' Function to change the order of columns, to flip, and to change the palettes of a \link{tabplot-object}.
#'
#' @aliases tableChange
#' @param tab \link{tabplot-object}
#' @param colNames vector of names of the desired columns
#' @param flip logical, if TRUE then the plot is flipped vertically, i.e.\ the row bins are reversed
#' @param pals list of color palettes. Each list item is on of the following:
#' \itemize{
#' \item a palette name in \code{\link{tablePalettes}}, optionally with the starting color between brackets.
#' \item a palette vector
#' }
#' @param colorNA color for missing values
#' @param numPals name(s) of the palette(s) that is(are) used for numeric variables ("Blues", "Greys", or "Greens"). Recycled if necessary.
#' @return \link{tabplot-object}
#' @export
#' @example ../examples/tableChange.R

tableChange <- function(tab, colNames=sapply(tab$columns, function(col)col$name), flip=FALSE, pals=list(), colorNA = NULL, numPals = NULL) {

	## change order of columns
	currentColNames <- sapply(tab$columns, function(col)col$name)

	colID <- match(colNames, currentColNames)
	
	## check if each column in colNames exist in tab
	if (any(is.na(colID))) stop(paste("Column(s) ", paste(colNames[is.na(colID)], collapse=", "), " does(do) not exist."  , sep=""))

	tab2 <- list(dataset=tab$dataset,
			n=length(colNames),
			nBins=tab$nBins,
			binSizes=tab$binSizes,
			isNumber=tab$isNumber[colID],
			rows=tab$rows,
			columns=lapply(colID, function(id) tab$column[[id]])
		)

	## flip tabplot
	if (flip) {
		tab2$rows$heights <- rev(tab$rows$heights)
		
		tab2$rows$heights <- -(tab$binSizes/tab$rows$m)
	    tab2$rows$y <- 1- c(0,cumsum(tab$binSizes/tab$rows$m)[-tab$nBins])
	
		tab2$rows$marks <- rev(tab$rows$marks)
		
	
		flipCol <- function(col) {
			col$sort <- ifelse(col$sort=="", "", ifelse(col$sort=="decreasing", "increasing", "decreasing"))
			if (col$isnumeric) {
				col$mean <- rev(col$mean)
				col$compl <- rev(col$compl)
				col$lower <- rev(col$lower)
				col$upper <- rev(col$upper)
				col$mean.scaled <- rev(col$mean.scaled)
				col$mean.brokenX <- rev(col$mean.brokenX)
				col$widths <- rev(col$widths)
			} else {
				col$freq <- col$freq[nrow(col$freq):1,]		
				col$x <- col$x[nrow(col$x):1,]		
				col$widths <- col$widths[nrow(col$widths):1,]		
			}
			return(col)
		}
		
		tab2$columns <- lapply(tab2$columns, flipCol)
	}
	
	## change palettes
	if (length(pals)!=0) {
		pals <- tableplot_checkPals(pals)

		whichCategorical <- which(sapply(tab2$columns, FUN=function(col)!col$isnumeric))

		paletNr <- 1
		for (i in whichCategorical) {
			tab2$columns[[i]]$paletname <- pals$name[paletNr]
			tab2$columns[[i]]$palet <- pals$palette[[paletNr]]
			paletNr <- ifelse(paletNr==length(pals$name), 1, paletNr + 1)
		}
	}

	## change colorNA
	if (!is.null(colorNA)) {
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
	if (!is.null(numPals)) {
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
	
	class(tab2) <- "tabplot"
	return(tab2)
}