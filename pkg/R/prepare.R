#' Prepares a data.frame or ffdf for tableplotting
#' 
#' @param x data.frame or ffdf, will be transformed into a ffdf
#' @param ... at the moment not used
#' @return a prepared object, including the data and order of each of the columns
#' @export
prepare <- function(x, path = NULL, ...){
	# TODO set path where prepared data set should be stored
	# TODO make it possible to sort on multiple columns
	
	if (is.data.frame(x)){
		x <- as.ffdf(x)
	}
	
	N <- nrow(x)
	isFactor <- sapply(physical(x), is.factor)
	
	#TODO randomize initial ordering
	ordered <- physical(x)
	
	ordered[isFactor] <- lapply(ordered[isFactor], function(f) { levels(f) <- NULL; f})
	ordered <- lapply(ordered, fforder)
	
	structure(
		list( data = x
			, ordered = ordered
		    )
		, class="prepared"
	)
}
