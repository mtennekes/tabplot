#' Create a tableplot
#'
#' A tableplot is a visualisation of (large) multivariate datasets. Each column represents a variable and each row bin is an aggregate of a certain number of records. For numeric variables, a bar chart of the mean values is depicted. For categorical variables, a stacked bar chart is depicted of the proportions of categories. Missing values are taken into account. Also supports large \code{\link[ff:ffdf]{ffdf}} datasets from the \code{\link[ff]{ff}} package. For a quick intro, see \href{../doc/tabplot-vignette.pdf}{\code{vignette("tabplot-vignette")}}.
#'
#' For large dataset, we recommend to use \code{\link{tablePrepare}}. Tableplotting is much faster
#' when the resulting object of \code{\link{tablePrepare}} is passed on to \code{tableplot} (argument \code{dat}) rather than the dataset itself.
#' @param dat a \code{\link{data.frame}}, an \code{\link[ff:ffdf]{ffdf}} object, or an object created by \code{\link{tablePrepare}} (see details below). Required.
#' @param select expression indicating the columns of \code{dat} that are visualized in the tablelplot Also column indices are supported. By default, all columns are visualized. Use \code{select_string} for character strings instead of expressions. 
#' @param subset logical expression indicing which rows to select in \code{dat} (as in \code{\link{subset}}). It is also possible to provide the name of a categorical variable: then, a tableplot for each category is generated. Use \code{subset_string} for character strings instead of an expressions.
#' @param sortCol column name on which the dataset is sorted. It can be eiter an index or an expression name. Also a character string can be used, but this is discouraged for programming purposes (use an index instead).
#' @param decreasing determines whether the dataset is sorted decreasingly (\code{TRUE}) of increasingly (\code{FALSE}).
#' @param nBins number of row bins
#' @param from percentage from which the sorted data is shown
#' @param to percentage to which the sorted data is shown
#' @param nCols the maximum number of columns per tableplot. If this number is smaller than the number of columns selected in \code{datNames}, multiple tableplots are generated, where each of them contains the sorted column(s).
#' @param sample determines whether to sample or use the whole data. Note that this does not involve the preprocessing stage. Therefore, it is only useful for large datasets where \code{\link{tablePrepare}} is used.
#' @param sampleBinSize the number of sampled objects per bin, if \code{sample} is \code{TRUE}.
#' @param scales determines the horizontal axes of the numeric variables in \code{colNames}. Options: "lin", "log", and "auto" for automatic detection. Either \code{scale} is a named vector, where the names correspond to numerical variable names, or \code{scale} is unnamed, where the values are applied to all numeric variables (recycled if necessary).
#' @param max_levels maximum number of levels for categorical variables. Categorical variables with more levels will be rebinned into \code{max_levels} levels. Either a positive number or -1, which means that categorical variables are never rebinned.
#' @param pals list of color palettes. Each list item is on of the following:
#' \itemize{
#' \item a palette name of \code{\link{tablePalettes}}, optionally with the starting color between brackets.
#' \item a color vector
#' }
#' If the list items are unnamed, they are applied to all selected categorical variables (recycled if necessary). The list items can be assigned to specific categorical variables,
#' by naming them accordingly.
#' @param change_palette_type_at number at which the type of categorical palettes is changed. For categorical variables with less than \code{change_palette_type_at} levels, the palette is recycled if necessary. For categorical variables with \code{change_palette_type_at} levels or more, a new palette of interpolated colors is derived (like a rainbow palette).
#' @param colorNA color for missing values
#' @param numPals vector of palette names that are used for numeric variables. These names are chosen from the sequential palette names in \code{\link{tablePalettes}}. Either \code{numPals} is a named vector, where the names correspond to the numerical variable names, or an unnamed vector (recycled if necessary).
#' @param limitsX a list of vectors of length two, where each vector contains a lower and an upper limit value. Either the names of \code{limitsX} correspond to numerical variable names, or \code{limitsX} is an unnamed list (recycled if necessary).
#' @param bias_brokenX parameter between 0 en 1 that determines when the x-axis of a numeric variable is broken. If minimum value is at least \code{bias_brokenX} times the maximum value, then X axis is broken. To turn off broken x-axes, set \code{bias_brokenX=1}.
#' @param IQR_bias parameter that determines when a logarithmic scale is used when \code{scales} is set to "auto". The argument \code{IQR_bias} is multiplied by the interquartile range as a test.
#' @param select_string character equivalent of the \code{select} argument (particularly useful for programming purposes)
#' @param subset_string character equivalent of the \code{subset} argument (particularly useful for programming purposes) 
#' @param colNames deprecated; used in older versions of tabplot (prior to 0.12): use \code{select_string} instead
#' @param filter deprecated; used in older versions of tabplot (prior to 0.12): use \code{subset_string} instead
#' @param plot boolean, to plot or not to plot a tableplot
#' @param ... layout arguments, such as \code{fontsize} and \code{title}, are passed on to \code{\link{plot.tabplot}}
#' @return \code{\link{tabplot-object}} (silent output). If multiple tableplots are generated (which can be done by either setting \code{subset} to a categorical column name, or by restricting the number of columns with \code{nCols}), then a list of \code{\link{tabplot-object}s} is silently returned.
#' @export
#' @import grid
#' @keywords visualization
#' @example ../examples/tableplot.R
#' @seealso \code{\link{itableplot}}
#' @note In early development versions of \code{tabplot} (prior to version 1.0) it was possible to sort datasets on multiple columns. To increase to tableplot creation speed, this feature is dropped. For multiple sorting purposes, we recommend to use the \code{subset} parameter instead.
tableplot <- function(dat, select, subset=NULL, sortCol=1,  decreasing=TRUE, 
					  nBins=100, from=0, to=100, nCols=ncol(dat),
					  sample=FALSE,
					  sampleBinSize=1e3,
					  scales="auto", max_levels=50, 
					  pals=list("Set1", "Set2", "Set3", "Set4"), 
					  change_palette_type_at = 20,
					  colorNA = "#FF1414", 
					  numPals = "Blues", 
					  limitsX = NULL,
					  bias_brokenX=0.8, IQR_bias=5, 
					  select_string = NULL,
					  subset_string=NULL, colNames=NULL, filter=NULL, 
					  plot=TRUE, ...) {


	#require(ffbase)

	##################################
	## prepare data if necessary
	##################################
	
	p <- dat
	if (!inherits(dat, "prepared")){
		datName <- deparse(substitute(dat))
		p <- tablePrepare(dat, name=datName)
	} else datName <- attr(p, "name")
	
	dat <- p$data
	
	##################################
	## check select and subset arguments
	##################################
	
	## discourage colNames and filter arguments
	if (!missing(colNames)) {
		warning("The argument colNames will not be supported 
				anymore in the future versions of tabplot. 
				Use select or select_string instead")
		select_string <- colNames
	}
	
	if (!missing(filter)) {
		warning("The argument filter will not be supported 
				anymore in the future versions of tabplot. 
				Use subset or subset_string instead")  
		subset_string <- filter
	}
	
	## argument subset(string): complement subset and subset_string
	if (!missing(subset)) {
		subset_string <- deparse(substitute(subset))
	} else if (!missing(subset_string)) {
		subset <- parse(text=subset_string)
	}
	
	## argument select(_string) argument
	if (!missing(select)) {
		nl <- as.list(seq_along(dat))
		names(nl) <- names(dat)
		colNames <- eval(substitute(select), nl, parent.frame())
		colNames <- names(dat)[colNames]
	} else if (!is.null(select_string)) {
		if (!all(select_string %in% names(dat))) 
			stop("select_string contains wrong column names")
		colNames <- select_string
	} else {
		colNames <- names(dat)
	}
	
	## argument sortCol
	sortCol <- tableplot_checkCols(substitute(sortCol), colNames)
	
	
	##################################
	## subset data
	##################################
	if (!missing(subset_string)) {
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
			
			subsets_string <- paste(subset_string, " == ", 
									ifelse(isLogical, "", "\""), lvls,
									ifelse(isLogical, "", "\""), sep="")
			tabs <- lapply(subsets_string, FUN=function(subs_string){
                tableplot(p, select_string=colNames, sortCol=sortCol, 
								 decreasing=decreasing, scales=scales, max_levels=max_levels, 
								 pals=pals, nBins=nBins,
								 from=from, to=to, subset_string=subs_string, 
                		  		 colorNA = colorNA, numPals = numPals, limitsX=limitsX,
								 bias_brokenX=bias_brokenX, IQR_bias=IQR_bias, plot=plot, ...)
			})
			return(invisible(tabs))
		}		
		p <- subset_data(p, cols=colNames, subset_string=subset_string, sortCol=sortCol)
		dat <- p$data
	}	
	
	##################################
	## other checks
	##################################

	isNumber <- sapply(physical(dat)[colNames], function(col)!is.factor.ff(col) && !vmode(col)=="logical")
	
	if (nrow(dat)==0) stop("<dat> doesn't have any rows")
	if (nrow(dat)==1) stop("<dat> has only one row")
	decreasing <- tableplot_checkDecreasing(decreasing, sortCol)
	nCols <- tableplot_checkNcols(nCols, colNames, sortCol)
	scales <- tableplot_checkScales(scales, colNames, isNumber)
	pals <- tableplot_checkPals(pals, colNames, !isNumber)
	if (any(!isNumber))
		change_palette_type_at <- tableplot_checkChangePalType(change_palette_type_at, 
														   max(sapply(pals[!isNumber], function(pal)length(pal$palette))))
	if (class(try(col2rgb(colorNA), silent=TRUE))=="try-error") 
		stop("<colorNA> is not correct")
	numPals <- tableplot_checkNumPals(numPals, colNames, isNumber)
	limitsX <- if (missing(limitsX)) list() else tableplot_checkLimitsX(limitsX, colNames, isNumber)
	tableplot_checkFromTo(from, to)
	
	#browser()
	N <- as.integer(length(p$ordered[[1]]) * (to-from)/100)
	nBins <- tableplot_checkBins(nBins, max(N,2))
	n <- ifelse(sample, min(nBins * sampleBinSize, N), N)
	
	
	##################################
	## bin data
	##################################
	
	bd <- bin_data( p, sortCol=sortCol, cols=colNames, from=from/100, to=to/100
    			  , nbins=nBins, decreasing=decreasing, sample, sampleBinSize=sampleBinSize)
		
	bd <- bin_hcc_data(bd, max_levels)
	
	tab <- columnTable( bd, datName, colNames=colNames, subset_string=subset_string, 
						sortCol=sortCol, decreasing=decreasing, scales=scales, 
						pals=pals, change_palette_type_at=change_palette_type_at,
						colorNA=colorNA, numPals=numPals, nBins=nBins, from=from, 
						to=to, N=N, n=n)
	
	
														   
	##################################
	## Grammar of Graphics
	##################################

	## scales
	tab$columns[isNumber] <- lapply(tab$columns[isNumber], scaleNumCol, IQR_bias)
	
	## coordinates
	tab$columns[!isNumber] <- lapply(tab$columns[!isNumber], coorCatCol, nBins)
	tab$columns[isNumber] <- mapply(coorNumCol, tab$columns[isNumber], limitsX[isNumber], MoreArgs=list(bias_brokenX=bias_brokenX), SIMPLIFY=FALSE)
	
	
	##################################
	## output
	##################################
	
	### multiple tableplots if nCols < length(colNames)
	if (nCols < length(colNames)) {
		tabs <- splitTab(tab, nCols)
		if (plot) sapply(tabs, plot, ...)
		invisible(tabs)
	} else {
		if (plot) plot(tab, ...)
		invisible(tab)
	}
}