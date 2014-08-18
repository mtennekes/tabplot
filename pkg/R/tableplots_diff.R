#' Compare two tableplots (experimental)
#'
#' Two tableplots can be compared by substracting two \link{tabplot-object}s. The result is a \link{tabplot_compare-object} object in which absolute and relative differences of mean values are stored, as well as a comparison of frequency tables for categorical variables. This object can be plotted with \code{\link[=plot.tabplot]{plot}}. 
#' 
#' @rdname tableplot_comparison
#' @aliases -.tabplot
#' @usage \method{-}{tabplot} (tp1, tp2)
#' @param tp1 the first \link{tabplot-object}
#' @param tp2 the second \link{tabplot-object}
#' @return a \link{tabplot_compare-object} that contains information about the comparison \code{tp1-tp2}
#' @example ../examples/tableplots_diff.R
#' @export
"-.tabplot" <- function(tp1, tp2) {
	stopifnot(tp1$nBins==tp2$nBins,
			  tp1$sortCol==tp2$sortCol)
	
	if (!all(tp1$select==tp2$select)) warning("Column names are not equal")
	
	tp <- tp1
	midspace <- .05
	tp$columns <- mapply(function(col1, col2) {
		col <- col1
		if (col1$isnumeric) {
			col$mean1 <- col1$mean
			col$mean2 <- col2$mean
			col$mean.diff <- col$mean <- col1$mean - col2$mean
			col$mean.diff.rel <- col$mean <- ((col1$mean - col2$mean) / col1$mean)*100
			col$scale_init <- "lin"
			col$compl <- pmin(col1$compl, col2$compl)
			col[c("mean", "scale_final", "mean.scaled", "brokenX", "mean.diff.coor", "marks.labels", "marks.x", "xline", "widths")] <- NULL
		} else {
			
# 			col <- tp$columns[[4]]
# 			col1 <- tp1$columns[[4]]
# 			col2 <- tp2$columns[[4]]
			
			col$freq1 <- col1$freq
			col$freq2 <- col2$freq
			
			freq <- col$freq.diff <- col1$freq - col2$freq
			xinit <- apply(freq, MARGIN=1, function(x)sum(x[x<0]))
			
			ids <- t(apply(freq, MARGIN=1, orderRow))
			freq.sorted <- sortRows(freq, ids)
			
			widths <- abs(freq.sorted)
			x <- t(apply(widths, 1, cumsum)) + xinit
			x <- cbind(xinit, x[,1:(ncol(x)-1)])
			
			ids2 <- t(apply(ids, 1, order))
			
			col$x <- sortRows(x, ids2)
			
			col$widths <- sortRows(widths, ids2)
			
			col$x <- col$x * (1-midspace) / 2
			col$widths <- col$widths * (1-midspace) / 2
			
			
			col$x[col$x<0] <- col$x[col$x<0] - (midspace/2)
			col$x[col$x>=0] <- col$x[col$x>=0] + (midspace/2)
			
			col$x[col$widths==0] <- NA
			col$widths[col$widths==0] <- NA
			
			col$x <- (col$x) + 0.5
			
			col$freq <- NULL
		}
		col
	}, tp1$columns, tp2$columns, SIMPLIFY=FALSE)
	
	isNumber <- sapply(tp$columns, function(col) col$isnumeric)
	

	tp$columns[isNumber] <- lapply(tp$columns[isNumber], scaleNumCol, IQR_bias=5, compare=TRUE)
	limitsX <- list()
	tp$columns[isNumber] <- mapply(coorNumCol, tp$columns[isNumber], limitsX[isNumber], MoreArgs=list(bias_brokenX=0.8, compare=TRUE), SIMPLIFY=FALSE)
	
	tp$n1 <- tp1$n
	tp$n2 <- tp2$n
	tp$N1 <- tp1$N
	tp$N2 <- tp2$N
	tp$n <- NULL
	tp$N <- NULL
	
	tp$dataset1 <- tp1$dataset
	tp$dataset2 <- tp2$dataset

	tp <- tp[c(18:19, 2:9, 14:17, 10:13)]
	class(tp) <- "tabplot_compare"
	tp	
}

orderRow <- function(x) {
	c(which(x < 0), which(x == 0), which(x > 0))
}

sortRows <- function(x, ids) {
	t(apply(cbind(x, ids), MARGIN=1, function(x){
		n <- length(x) / 2
		y <- x[(n+1):(2*n)]
		x <- x[1:n]
		x[y]
	}))
}