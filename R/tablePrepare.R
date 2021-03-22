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
#' @param dir directory to store the prepared object. If unspecified, the prepared object will not be saved, and the underlying data will be stored temporarily in \code{options("fftempdir")}.
#' @param use_ff use `ff ` under the hood. Otherwise, use `data.table`
#' @param ... arguments passed to other methods (at the moment only \code{overwrite} from \code{\link{savePrepare}}) 
#' @return a prepared object, including the data and order of each of the columns
#' @example ./examples/tablePrepare.R
#' @export
#' @import ffbase
tablePrepare <- function(x, name=NULL, dir=NULL, use_ff = FALSE,...){
	# TODO set path where prepared data set should be stored
	# TODO make it possible to sort on multiple columns
	# cat("Preparing data for tableplotting, storing this result increases tableplotting speed (see `prepare`)...")
	
	if (missing(name)) name <- deparse(substitute(x))
	
	if (is.data.frame(x)){
		isChar <- sapply(x, FUN = is.character)
		if (any(isChar)) x[, isChar] <- lapply(x[, isChar, drop=FALSE], factor)
		x <- as.ffdf(x)
	}
	
	row.names(x) <- NULL
	N <- nrow(x)
	isFactor <- sapply(physical(x), ff::is.factor)
	
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
	
	close(rand)
	close(rand_order)
	
	#ranked <- lapply(ordered, fforder)
	
	tp <- structure(
		list( data = x
			, ordered = ordered
#			, ranked = ranked
		)
		, name = name
		, class="prepared"
	)
	if (!missing(dir)) tryCatch(savePrepare(tp, dir, ...), error = function(e) message("Saving to", dir, "failed with the following error:\n", e$message, "\n The returned prepare object can be saved with savePrepare."))
	tp
}

#' Closes an object created with tablePrepare
#' 
#' Closes an object created with tablePrepare. It frees the Memory Mapping resources and closes the underlying \code{ff} data without deleting the file data.
#' 
#' @param con object of class \code{prepared}, i.e. created with \code{\link{tablePrepare}}.
#' @param ... not used
#' @return \code{TRUE} if the files could be closed, \code{FALSE} if it was closed already
#' @export
close.prepared <- function(con, ...) {
	all(sapply(con, close))
}
