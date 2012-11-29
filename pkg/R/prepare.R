#' Prepares a data.frame or ffdf for tableplotting
#' 
#' The function \code{\link{bin_data}} needs a prepared data.frame
#' Prepare transforms the supplied data into a ffdf data.frame and calculates
#' the order of each of its columns. Knowing the order of the columns speeds up
#' the binning process consideratly, For large ffdf this may be a time consuming
#' step so it can be wise to call prepare before making a tableplot.
#' @param x data.frame or ffdf, will be transformed into a ffdf
#' @param path where the resulting prepared dataset should be stored, at the moment not working.
#' @param name name of the dataset
#' @param ... at the moment not used
#' @return a prepared object, including the data and order of each of the columns
#' @export
#' @import ffbase
prepare <- function(x, path = NULL, name=deparse(substitute(x)), ...){
	# TODO set path where prepared data set should be stored
	# TODO make it possible to sort on multiple columns
	cat("Preparing data for tableplotting, storing this result increases tableplotting speed (see `prepare`)...")
	
	require(ffbase)
	
	if (is.data.frame(x)){
		x <- as.ffdf(x)
	}
	
	N <- nrow(x)
	isFactor <- sapply(physical(x), is.factor.ff)
	
	ordered <- physical(x)
	
	# initial randomization
	# Question: is there a function ffsample?
	rand <- ff(vmode="double", length=N)
	for (i in chunk(rand)){
		rand[i] <- runif(sum(i))
	}
	rand_order <- fforder(rand)

	ordered <- lapply(ordered, function(o)o[rand_order])
	x <- x[rand_order, ]
	
	# create ordered ffdf
	ordered[isFactor] <- lapply(ordered[isFactor], function(f) { levels(f) <- NULL; f})
	ordered <- lapply(ordered, fforder, na.last=FALSE)
	ordered <- do.call(ffdf, ordered)
	
	#ranked <- lapply(ordered, fforder)
	
	
	cat("\r\n")
	structure(
		list( data = x
			, ordered = ordered
#			, ranked = ranked
		)
		, name = name
		, class="prepared"
	)
}
