#' Visualization of large multivariate datasets.
#'
#' A tableplot is a visualisation of a (large) multivariate dataset. Each column represents a variable and each row bin is an aggregate of a certain number of records. For numeric variables, a bar chart of the mean values is depicted. For categorical variables, a stacked bar chart is depicted of the proportions of categories. Missing values are taken into account. Also supports large ffdf datasets from the ff package. Use \code{\link{tableGUI}} to customize this function with a GUI.
#'
#' @aliases tableplot
#' @param dat a \code{\link{data.frame}}, \code{\link{data.table}}, or an \code{\link[ff:ffdf]{ffdf}} object (required)
#' @param colNames character vector containing the names of the columns of \code{dat} that are visualized in the tablelplot. If omitted, all columns are visualized. All selected columns should be of class: numeric, integer, factor, or logical.
#' @param sortCol columns that are sorted. \code{sortCol} is either a vector of column names of a vector of indices of \code{colNames}
#' @param decreasing determines whether the columns are sorted decreasingly (TRUE) of increasingly (FALSE). \code{decreasing} can be either a single value that applies to all sorted columns, or a vector of the same length as \code{sortCol}.
#' @param scales determines the horizontal axes of the numeric variables in \code{colNames}, options: "lin", "log", and "auto" for automatic detection. If necessary, \code{scales} is recycled.
#' @param pals list of color palettes. Each list item is on of the following:
#' \itemize{
#' \item a index number between 1 and 16. In this case, the default palette is used with the index number being the first color that is used.
#' \item a palette name in \code{\link{tabplotPalettes}}, optionally with the starting color between brackets.
#' \item a palette vector
#' }
#' The items of \code{pals} are applied to the categorical variables of \code{colNames}. If necessary, \code{pals} is recycled.
#' @param nBins number of row bins
#' @param from percentage from which the data is shown
#' @param to percentage to which the data is shown
#' @param bias_brokenX parameter between 0 en 1 that determines when the x-axis of a numeric variable is broken. If minimum value is at least \code{bias_brokenX} times the maximum value, then X axis is broken. To turn off broken x-axes, set \code{bias_brokenX=1}.
#' @param IQR_bias parameter that determines when a logarithmic scale is used when \code{scales} is set to "auto". The argument \code{IQR_bias} is multiplied by the interquartile range as a test.
#' @param plot boolean, to plot or not to plot a tableplot
#' @param ... arguments passed to \code{\link{plot.tabplot}}
#' @return \link{tabplot-object} (silent output)
#' @export
#' @keywords visualization
#' @example examples/tableplot.R


# TO DO:
# @param filter variable name(s) on which the tableplot is filtered


tableplot <- function(dat, colNames=names(dat), sortCol=1,  decreasing=TRUE, scales="auto", pals=list(1, 9, 3, 10), nBins=100, from=0, to=100, filter=NULL, bias_brokenX=0.8, IQR_bias=5, plot=TRUE, ...) {

	datName <- deparse(substitute(dat))
	if (class(dat)[1]=="data.frame") dat <- data.table(dat)
	
	#####################################
	## Filter data
	#####################################
	if (!is.null(filter)) {
		if (class(filter)[1]!="expression") stop("<filter> is not an expression")
		
		if (class(dat)[1]=="ffdf") {
			sel <- bit(nrow(dat))
			for (i in chunk(dat)) {
				sel[i] <- eval(filter, dat[i,])
			}
			dat <- subset(dat, sel)
		} else {
			sel <- eval(filter, dat)
			dat <- dat[sel,]
		}
		
	}
	
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
	if (class(dat)[1]=="data.table") {
		dat <- dat[, colNames, with=FALSE] 
	} else {
		dat <- dat[colNames]
	}
	
	n <- length(colNames)

	## Check sortCol, and (if necessary) cast it to indices
	sortCol <- tableplot_checkCols(sortCol, colNames)

	## Check decreasing vector
	decreasing <- tableplot_checkDecreasing(decreasing, sortCol)

	## Check scales
	scales <- tableplot_checkScales(scales)

	## Check palet indices
	pals <- tableplot_checkPals(pals)
	
	## Check nBins
	nBins <- tableplot_checkBins(nBins, nrow(dat))
	
	## Check from and to
	tableplot_checkFromTo(from, to)
	

	## Check filter variables
	# if (!is.null(filter)) filter <- tableplot_checkCols(filter, colNames)

	######## TO DO: implement filter variable(s)

	##########################
	#### Preprocess
	##########################

	tab <- preprocess(dat, datName, colNames, sortCol,  decreasing, scales, pals, nBins, from,to)
	
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
	for (i in which(isNumber)) {
		if (tab$columns[[i]]$scale_init=="auto") {
			quant <- quantile(tab$columns[[i]]$mean, na.rm=TRUE)
			IQR <- quant[4] - quant[2]
			
			## Simple test to determine whether scale is lin or log
			if ((quant[5] > quant[4] + IQR_bias * IQR) || 
				(quant[1] < quant[2] - IQR_bias * IQR)) {
				tab$columns[[i]]$scale_final <- "log" 
			} else {
				tab$columns[[i]]$scale_final <- "lin" 
			}
		} else {
			tab$columns[[i]]$scale_final <- tab$columns[[i]]$scale_init
			
		}
	}
	
	## Apply scale transformation
	for (i in which(isNumber)) {
		if (tab$columns[[i]]$scale_final=="log") {
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

	#### Broken X-axis
	temp <- lapply(tab$columns[isNumber], FUN=function(x){brokenX(x$mean.scaled, bias_brokenX)})
	j <- 1
	for (i in which(isNumber)) {
		tab$columns[[i]]$brokenX <- temp[[j]]$brokenX
		tab$columns[[i]]$mean.brokenX <- temp[[j]]$values
		j <- j + 1
	}
	## make this code prettier
	
	#### Normalization
	for (i in which(isNumber)) {
		brokenX <- tab$columns[[i]]$brokenX
		values <- tab$columns[[i]]$mean.brokenX
		## scale values to 0-1, and determine 0-1 value of the y-axis
		minV <- min(values, na.rm=TRUE)
		maxV <- max(values, na.rm=TRUE)
		if (minV < 0 && maxV > 0) {
			xline <- -minV / (maxV - minV)
			widths <- (values) / (maxV - minV)
		} else if (brokenX==1) {
			xline <- 0
			widths <- 0.3 + (values) * 0.7 / (maxV - minV)
		} else if (brokenX==-1) {
			xline <- 1
			widths <- -0.3 + (values) * 0.7 / (maxV - minV)
		} else {
			xline <- ifelse(maxV > 0, 0, 1)
			widths <- (values) / max(abs(minV), abs(maxV))
		}
		widths[is.nan(widths)] <- minV
		## assign to tab object
		tab$columns[[i]]$xline <- xline
		tab$columns[[i]]$widths <- widths
	}
	
	## plot
	class(tab) <- "tabplot"
	if (plot) plot(tab, ...)
	invisible(tab)
}