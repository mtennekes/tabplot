#' Prepares a dataset for tableplotting
#' 
#' Tableplots from a large dataset can be generated very fast when the preprocessing stage is done only once. This function preprocesses the dataset, and returns an object that can be passed to \code{\link{tableplot}}. From this stage, tableplots are generated very fast, no matter on which column the data is sorted or how many row bins are chosen.
#' 
#' The function \code{\link{bin_data}} needs a prepared \code{\link{data.frame}}
#' Prepare transforms the supplied data into an \code{\link[ff:ffdf]{ffdf}} object and calculates
#' the order of each of its columns. Knowing the order of the columns speeds up
#' the binning process consideratly, For large \code{\link[ff:ffdf]{ffdf}} objects this may be a time consuming
#' step so it can be wise to call prepare before making a tableplot.
#' @param x \code{\link{data.frame}} or \code{\link[ff:ffdf]{ffdf}}, will be transformed into an \code{\link[ff:ffdf]{ffdf}} object.
#' @param name name of the dataset
#' @param ... at the moment not used
#' @return a prepared object, including the data and order of each of the columns
#' @example ../examples/tablePrepare.R
#' @export
#' @import ffbase
tablePrepare <- function(x, name=deparse(substitute(x)), maxN=1e4, ...){
	# TODO set path where prepared data set should be stored
	# TODO make it possible to sort on multiple columns
	# cat("Preparing data for tableplotting, storing this result increases tableplotting speed (see `prepare`)...")
	require(ffbase)
	
	if (is.data.frame(x)){
		x <- as.ffdf(x)
	}

	
	
	row.names(x) <- NULL
	N <- nrow(x)
	if (maxN==0) maxN <- N
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
	
	# sample
	sample_ids <- round(seq(1, N, length.out=maxN))
	ordered <- lapply(ordered, function(o){
		as.ff(o[sample_ids])
	})
	
	ordered <- do.call(ffdf, ordered)
	
	#ranked <- lapply(ordered, fforder)
	
	
	#cat("\r\n")
	structure(
		list( data = x
			, ordered = ordered
#			, ranked = ranked
		)
		, name = name
		, class="prepared"
	)
}
