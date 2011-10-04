#' Subsetting a ff vector or ffdfdata frame (modified from ffbase r62)
#'
#' @export subset.ff subset.ffdf
#' @aliases subset.ff subset.ffdf
#' @method subset ff
#' @param x \code{ff} vector or \code{ffdf} data.frame to be subset
#' @param subset an expression, \code{ri}, \code{bit} or logical \code{ff} vector that can be used to index x
#' @param ... not used
#' @return a new ff vector containing the subset, data is physically copied
subset.ff <- function(x, subset, ...){
	#y <- ff(length=sum(subset), vmode=vmode(x))
	y <- clone(x)
	length(y) <- sum(subset, na.rm=TRUE)
   #TODO fix this for very large subsets...
	y[] <- x[subset]
	y
}

subset.ffdf <- function(x, subset, ...){
	y <- clone(x)
	n <- 0
	#subset <- eval(expression(substitute(subset)))
	#print(subset)
	#TODO check if subset is an expression or logical vector. If so, then idx vector
	for (i in chunk(x)){
	   dat <- x[i,]
	   row.names(dat) <- min(i):max(i)
	   #sel <- eval(substitute(subset), dat, parent.frame())
	   dat <- dat[subset[i],]
	   s <- nrow(dat) 
	   if (s > 0){
	      y[(n+1):(n+s),] <- dat
		  n <- n + s
	   }
	}
	nrow(y) <- n
	y 
}