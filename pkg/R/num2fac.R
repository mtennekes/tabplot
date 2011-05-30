#' Function that creates a factor vector out of a numerical vector.
#'
#' Creates a factor vector out of a numerical vector. The levels can be determined by several methods. This function can be called via \code{\link{tableGUI}}.
#'
#' @aliases num2fac
#' @param num numeric vector
#' @param method \itemize{
#' \item \code{"pretty"} intervals are determined by the base function \code{\link{pretty}}
#' \item \code{"kmeans"} the method intervals are determined by the method kmeans where \code{n} clusters (i.e. intervals) are found
#' \item \code{"fixed"} determines the intervals by the argument \code{brks}
#' \item \code{"discrete"} the unique values in \code{num} are mapped one to one to the levels of the new factor vector)
#' }
#' @param num_scale \itemize{
#' \item \code{"auto"} used scale is determined automatically
#' \item \code{"lin"} \code{num} is directly fed to the method pretty or kmeans
#' \item \code{"log"} a logarithmic transformation of \code{num} is fed to the method pretty or kmeans
#' }
#' @param n the (desired) number of levels. \code{n=0} means automatic
#' @param brks breaks that determine the levels (only required when \code{method="fixed"})
#' @return A factor vector
#' @export
#' @note \code{ff} vectors are not implemented yet
num2fac <-
function(num, method="pretty", num_scale="auto", n=0, brks=NA) {
    if (!require(classInt)){
	   stop("This function needs package classInt")
	}
	
	intervalLabels <- function(brks) {
		lbs <- formatC(brks,digits=12, width=1, big.mark=",")
		l <- length(lbs)
		ivls <- mapply(lbs[1:(l-1)], lbs[2:l], FUN=function(x,y){paste("[", x, ", ", y, ")", sep="")})
		substr(ivls[length(ivls)],nchar(ivls[length(ivls)]), nchar(ivls[length(ivls)])) <- "]"
		return(ivls)
	}
	
	if (method=="fixed") {
		ivls <- intervalLabels(brks)
		brks[length(brks)] <- brks[length(brks)]+1
		fac <- cut(num, breaks = brks, labels = ivls, right=FALSE)
		return(fac)
	} else if (method=="discrete") {
		fac <- as.factor(num)
		return(fac)
	}

	
	
	# determine quantiles
	quant <- quantile(num, na.rm=TRUE)
	
	# simple test to determine the proper num_scale
	# TODO implement IQR
	if (num_scale=="auto") {
		if (((quant[4] >= 0) && (quant[5] > (quant[4] * 100))) || ((quant[2] < 0) && (quant[1] < (quant[2] * 100)))) {
			num_scale <- "log"
		} else {
			uniq <- unique(num)
			if (length(uniq) > 12) {
				num_scale <- "lin"
			} else {
				num_scale <- "cat"
			}
		}
	}
	
	if (num_scale=="log") {
		# scale is logarithmic
		if (n==0) n <- 5
		lognum <- na.omit(num)
		posnum <- lognum>=0
		lognum[posnum] <- log10(1+lognum[posnum])
		lognum[!posnum] <- -log10(1-lognum[!posnum])
		
		logbrks <- classIntervals(lognum, n=n, style=method)$brks
		
		brks <- integer(length(logbrks))
		brks[logbrks >= 0] <- round(10^logbrks[logbrks >= 0] - ifelse(method=="kmeans", 1, 0))
		brks[logbrks < 0] <- -round(10^-logbrks[logbrks < 0] - ifelse(method=="kmeans", 1, 0))

		if (method=="pretty") brks[brks==1] <- 0
		
		ivls <- intervalLabels(brks)
		brks[length(brks)] <- brks[length(brks)]+1
		fac <- cut(num, breaks = brks, labels = ivls, right=FALSE)
	} else if (num_scale=="lin") {
		# scale is lineair
		if (method=="pretty") {
			if (n==0) {
				unif <- seq(quant[1], quant[5], length.out=5)
				deviation <- pmin(abs(quant), abs(unif)) / pmax(abs(quant), abs(unif))
				deviation[is.nan(deviation)] <- 1
				n <- round(5 + (1-min(deviation))*7)
			}
		} else {
			if (n==0) n <- 5
		}
	
		brksUnround <- classIntervals(num, n=n, style=method)$brks
		digits <- 0
		brks <- round(brksUnround, digits=digits)
		while (any(duplicated(brks))) {
			brks <- round(brksUnround, digits=digits)
			digits <- digits + 1
		}
		
		
		
		ivls <- intervalLabels(brks)
		brks[length(brks)] <- brks[length(brks)]+1
		fac <- cut(num, breaks = brks, labels = ivls, right=FALSE)
	} else {
		# levels are discrete categories.
		fac <- as.factor(num)
	}
	return(fac)
}

