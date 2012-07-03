#' Create a tableplot
#'
#' A tableplot is a visualisation of (large) multivariate datasets. Each column represents a variable and each row bin is an aggregate of a certain number of records. For numeric variables, a bar chart of the mean values is depicted. For categorical variables, a stacked bar chart is depicted of the proportions of categories. Missing values are taken into account. Also supports large ffdf datasets from the ff package.
#'
#' @param dat a \code{\link{data.frame}}, \code{\link{data.table}}, or an \code{\link[ff:ffdf]{ffdf}} object (required)
#' @param select expression indicating the columns of \code{dat} that are visualized in the tablelplot. By default, all columns are visualized. Use \code{select_string} to provide a character string instead of an expression. 
#' @param subset filter that condition to subset the observations in \code{dat} (expression). It is also possible to provide the name of a categorical variable: then, a tableplot for each category is generated. Use \code{subset_string} to provide a character string instead of an expression.
#' @param sortCol columns that are sorted. \code{sortCol} is either a vector of column names of a vector of indices of \code{colNames}
#' @param decreasing determines whether the columns are sorted decreasingly (TRUE) of increasingly (FALSE). \code{decreasing} can be either a single value that applies to all sorted columns, or a vector of the same length as \code{sortCol}.
#' @param nBins number of row bins
#' @param from percentage from which the data is shown
#' @param to percentage to which the data is shown
#' @param nCols the maximum number of columns per tableplot. If this number is smaller than the number of columns selected in \code{datNames}, multiple tableplots are generated, where each of them contains the sorted column(s).
#' @param scales determines the horizontal axes of the numeric variables in \code{colNames}, options: "lin", "log", and "auto" for automatic detection. If necessary, \code{scales} is recycled.
#' @param pals list of color palettes. Each list item is on of the following:
#' \itemize{
#' \item a palette name in \code{\link{tablePalettes}}, optionally with the starting color between brackets.
#' \item a palette vector
#' }
#' The items of \code{pals} are applied to the categorical variables of \code{colNames}. If necessary, \code{pals} is recycled.
#' @param colorNA color for missing values
#' @param numPals name(s) of the palette(s) that is(are) used for numeric variables ("Blues", "Greys", or "Greens"). Recycled if necessary.
#' @param bias_brokenX parameter between 0 en 1 that determines when the x-axis of a numeric variable is broken. If minimum value is at least \code{bias_brokenX} times the maximum value, then X axis is broken. To turn off broken x-axes, set \code{bias_brokenX=1}.
#' @param IQR_bias parameter that determines when a logarithmic scale is used when \code{scales} is set to "auto". The argument \code{IQR_bias} is multiplied by the interquartile range as a test.
#' @param select_string character equivalent of the \code{select} argument (particularly useful when writing functions)
#' @param subset_string character equivalent of the \code{subset} argument (particularly useful when writing functions) 
#' @param colNames used in older versions of tabplot (<= 0.11-2): use \code{select(\_string)} instead
#' @param filter used in older versions oftabplot (<= 0.11-2): use \code{subset(\_string)} instead
#' @param plot boolean, to plot or not to plot a tableplot
#' @param ... arguments passed to \code{\link{plot.tabplot}}
#' @return \link{tabplot-object} (silent output)
#' @export
#' @keywords visualization
#' @example ../examples/tableplot.R
tableplot <- function(dat, select, subset=NULL, sortCol=1,  decreasing=TRUE, 
					  nBins=100, from=0, to=100, nCols=ncol(dat), 
					  scales="auto", pals=list("Set1", "Set2", "Set3", "Set4"), colorNA = "#FF1414", 
					  numPals = "Blues", bias_brokenX=0.8, IQR_bias=5, select_string = NULL,
					  subset_string=NULL, colNames=NULL, filter=NULL, 
					  plot=TRUE, ...) {

	datName <- deparse(substitute(dat))
	if (class(dat)[1]=="data.frame") dat <- data.table(dat)


	
	## discourage colNames and filter arguments
	if (!missing(colNames)) {
		warning("The argument colNames will not be supported anymore in the future versions 
				of tabplot. Use select or select_string instead")
		select_string <- colNames
	}

	if (!missing(filter)) {
		warning("The argument filter will not be supported anymore in the future versions of tabplot. 
				Use subset or subset_string instead")  
		subset_string <- filter
	}
	
	
	#####################################
	## Filter data: subset(string)
	#####################################
	# complement subset and subset_string
	if (!missing(subset)) {
		subset_string <- deparse(substitute(subset))
	} else if (!missing(subset_string)) {
		subset <- parse(text=subset_string)
	}

	
	if (!is.null(subset_string)) {

		# split by one variable
		if (subset_string %in% names(dat)) {
			lvls <- levels(dat[[subset_string]])
			
			if ((class(dat[[subset_string]])[1]=="logical") || (class(dat)[1]=="ffdf" &&
				vmode(dat[[subset_string]]) %in% c("boolean", "logical"))) {
				isLogical <- TRUE
				lvls <- c("TRUE", "FALSE")
			} else {
				isLogical <- FALSE
			}
			if (is.null(lvls)) stop("subset variable is not categorical")
			
			subsets_string <- paste(subset_string, " == ", ifelse(isLogical, "", "\""), lvls,
									ifelse(isLogical, "", "\""), sep="")
			tabs <- lapply(subsets_string, FUN=function(subs_string){
				tab <- tableplot(dat, select_string=select_string, sortCol=sortCol, 
								 decreasing=decreasing, scales=scales, pals=pals, nBins=nBins,
								 from=from, to=to, subset_string=subs_string, 
								 bias_brokenX=bias_brokenX, IQR_bias=IQR_bias, plot=plot, ...)
				tab
			})
			return(invisible(tabs))
		}
		e <- substitute(subset)
		# other filters
		if (class(dat)[1]=="ffdf") {
			r <- bit(nrow(dat))
			for (i in chunk(dat)) {
				r[i] <- eval(e, dat[i,])
				r <- r & !is.na(r)
			}
			dat <- subset(dat, r)
		} else {
			r <- eval(e, dat, parent.frame())
			r <- r & !is.na(r)
			dat <- dat[r, ]
		}
		
	}
	
	#####################################
	## Check arguments and cast dat-columns to numeric or factor
	#####################################
	
	## Check dat
	if (nrow(dat)==0) stop("<dat> doesn't have any rows")
	if (nrow(dat)==1) stop("<dat> has only one row")
	
	## Check select(_string)
	if (!missing(select)) {
		nl <- as.list(seq_along(dat))
		names(nl) <- names(dat)
		colNames <- eval(substitute(select), nl, parent.frame())
		colNames <- names(dat)[colNames]
	} else if (!is.null(select_string)) {
		if (!all(select_string %in% names(dat))) stop("select_string contains wrong column names")
		colNames <- select_string
	} else {
		colNames <- names(dat)
	}
	
	## Only select the columns of colNames
	if (class(dat)[1]=="data.table") {
		
		ignoreNames <- setdiff(names(dat), colNames)
		if (length(ignoreNames)!=0) 
			dat[, ignoreNames:=NULL, with=FALSE]
		if (!identical(colNames, names(dat)))
			setcolorder(dat, colNames)
			#dat <- subset(dat, select=colNames)
	} else {
		dat <- dat[colNames]
	}
	
	n <- length(colNames)

	## Check sortCol, and (if necessary) cast it to indices

	sortCol <- tableplot_checkCols(substitute(sortCol), colNames)

	## Check decreasing vector
	decreasing <- tableplot_checkDecreasing(decreasing, sortCol)

	
	## Check number of columns
	if (!is.numeric(nCols)) stop("<ncolums> is not numeric")
	if (nCols < length(sortCol)) stop("<nCols> less than number of sorted columns")
	if (nCols == length(sortCol) && length(sortCol) < length(colNames)) 
		stop("<nCols> equal to number of sorted columns while number of selected columns is larger")
	
	if (nCols > length(colNames)) nCols <- length(colNames)
	
	## Check scales
	scales <- tableplot_checkScales(scales)

	## Check palet indices
	pals <- tableplot_checkPals(pals)
	
	## Check colorNA
	if (class(try(col2rgb(colorNA), silent=TRUE))=="try-error") {
		stop("<colorNA> is not correct")
	}
	
	## Check numPals
	if ((class(numPals)!="character") || !all(numPals %in% c("Blues", "Greens", "Greys")))
		stop("<numPals> is not correct")
	
	## Check nBins
	nBins <- tableplot_checkBins(nBins, nrow(dat))
	
	## Check from and to
	tableplot_checkFromTo(from, to)
	

	##########################
	#### Preprocess
	##########################

	
	tab <- preprocess(dat, datName, subset_string, colNames, sortCol,  
					  decreasing, scales, pals, colorNA, numPals, nBins, from,to)
	
	#dat[, agg Index:=NULL]
	
	# delete cloned ffdf (those with filter)
	if (!missing(subset_string) && class(dat)[1]=="ffdf") delete(dat)

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

	### multiple tableplots if nCols < length(colNames)
	if (nCols < length(colNames)) {
		nOtherCol <- nCols - length(sortCol)
		
		otherCols <- setdiff(seq.int(length(colNames)), sortCol)
		
		nOtherCols <- length(otherCols)
		
		ntab <- ceiling(nOtherCols / nOtherCol)
		tabs <- list()
		j <- 1
		for (i in seq.int(ntab))	{
			id <- unique(c(sortCol,
				otherCols[j:(min(j-1+nOtherCol, nOtherCols))]))
			tab_sec <- tab
			tab_sec$n <- length(id)
			tab_sec$isNumber <- tab$isNumber[id]
			tab_sec$columns <- tab$columns[id]
			class(tab_sec) <- "tabplot"
			tabs[[i]] <- tab_sec
			if (plot) plot(tab_sec, ...)
			j <- j + nOtherCol
		}
		invisible(tabs)
	} else {		
		## plot
		class(tab) <- "tabplot"
		if (plot) plot(tab, ...)
		invisible(tab)
	}
}