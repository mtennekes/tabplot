columnsPlot <- function( dat
                       , colNames=names(dat)
                       , sortCol=1
                       ,  decreasing=TRUE
                       , scales="auto"
                       , palet=c(1, 9, 3, 10)
                       , nBins=100
                       , from=0
                       , to=100
                       ) {


	#####################################
	## Check arguments and cast dat-columns to numeric or factor
	#####################################
	
	## Check dat
	if (nrow(dat)==0) stop("<dat> doesn't have any rows")
	if (nrow(dat)==1) stop("<dat> has only one row")
	
	## Check colNames
	if (class(colNames)[1]!="character") stop("<colNames> is not a character(vector)")
	if (!all(colNames %in% names(dat))) stop("<colNames> contains column names that are not found in <dat>")

	## Only select the columns of colNames
	dat <- dat[colNames]
	n <- length(colNames)

	## Check sortCol, and (if necessary) cast it to indices
	if (class(sortCol)[1]=="character") {
		if (!all(sortCol %in% colNames)) stop("invalid <sortCol>")
		sortCol <- sapply(sortCol, FUN=function(x) which(x==colNames))
	} else if (class(sortCol)[1] %in% c("numeric", "integer")) {
		if (any(sortCol > ncol(dat)) || any(sortCol < 1)) {
			stop("<sortCol> has an invalid value")
		}
	} else {
		stop("<sortCol> is not a character or numeric value or vector")
	}

	## Check decreasing vector
	if (class(decreasing)[1]!="logical") stop("<decreasing> is not a logical")
	if (length(decreasing)==1) {
		decreasing <- rep(decreasing, length(sortCol))
	} else if (length(decreasing) != length(sortCol)) stop("<sortCol> and <decreasing> have different lengths")
	
	## Check scales
	if (length(scales)==1) scales <- rep(scales, n)
	if (length(scales)!=length(colNames)) stop(paste("<scales> should be of length ", length(colNames)))
	if (length(setdiff(scales, c("auto", "lin", "log")))>0) stop("<scales> should consist of auto, lin and log")

	## Check palet indices
	if (!(class(palet) %in% c("numeric", "integer"))) stop("<palet> is not an integer vector")
	if (any(palet<1) || any(palet>16)) stop("<palet> number(s) should be between 1 and 16")
	
	## Check nBins
	if (class(nBins)[1]!="numeric") stop("<nBins> is not numeric")
	if (nBins > nrow(dat)) { 
		warning("Setting nBins (",nBins,") to number of rows (", nrow(dat), ")")
		nBins <- nrow(dat)
	}
	
	## Check from and to
	if (class(from)[1]!="numeric") stop("<from> is not numeric")
	if (class(to)[1]!="numeric") stop("<to> is not numeric")
	if (from < 0 && from > 100) stop("<from> is not a number in [0, 100]")
	if (to < 0 && to > 100) stop("<to> is not a number in [0, 100]")
	if (from >= to) stop("<from> is not smaller than <to>")




	##########################
	#### Preprocess
	##########################

	tab <- preprocess(dat, colNames, sortCol,  decreasing, scales, palet, nBins, from,to)

	scales <- tab$scales
	isNumber <- tab$isNumber
	
	###########################
	##### Function to determine logarithmic scale
	###########################
	getLog <- function(x) {
		logx <- numeric(length(x))
		neg <- x < 0		
		logx[!neg] <- log10(x[!neg]+1)
		logx[neg] <- -log10(abs(x[neg])+1)
		return(logx)
	}

	#####################################
	#####################################
	## Grammar of Graphics: Scales
	##
	## Scale operations
	#####################################
	#####################################
	
	## Determine scales of numeric variables in case they are set to "auto". IQR is used.
	IQR_bias <- 3
	for (i in which(isNumber & scales=="auto")) {
		quant <- quantile(tab$columns[[i]]$mean, na.rm=TRUE)
		IQR <- quant[4] - quant[2]
		
		## Simple test to determine whether scale is lin or log
		if ( (quant[5] > quant[4] + IQR_bias * IQR)
        || (quant[1] < quant[2] - IQR_bias * IQR)
         ){
			scales[i] <- "log" 
		} else {
			scales[i] <- "lin" 
		}
	}
	
	## Assign scale information to list, and apply scale transformation
	for (i in which(isNumber)) {
		tab$columns[[i]]$scale <- scales[i]
		if (scales[i]=="log") {
			tab$columns[[i]]$mean.scaled <- getLog(tab$columns[[i]]$mean)
		} else {
			tab$columns[[i]]$mean.scaled <- tab$columns[[i]]$mean
		}
	}
	
	#####################################
	#####################################
	## Grammar of Graphics: Coordinates
	##
	## Coordinate transformations
	#####################################
	#####################################

	#############################
	## Categorical variables
	#############################

	## determine widths and x positions of the categorical variables
	for (i in which(!isNumber)) {
		categories <- tab$columns[[i]]$categories
		widths <- tab$columns[[i]]$freq / rep(tab$binSizes, length(categories))
		
		x <- cbind(0,(t(apply(widths, 1, cumsum)))[, -length(categories)])
		tab$columns[[i]]$categories <- categories
		tab$columns[[i]]$x <- x
		tab$columns[[i]]$widths <- widths
	}


	#############################
	## Numeric variables
	#############################
	
	#### Normalization
	for (i in which(isNumber)){
      numcol <- tab$columns[[i]]
      ## scale values to 0-1, and determine 0-1 value of the y-axis
      numcol$range <- range(numcol$lower, numcol$upper, na.rm=TRUE)
      tab$columns[[i]] <- numcol
	}
	## plot
	plotColumns(tab)

}