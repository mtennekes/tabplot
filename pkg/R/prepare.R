#' Prepares a data.frame or ffdf for tableplotting
prepare <- function(x, ...){
	# TODO set path where prepared data set should be stored
	if (is.data.frame(x)){
		x <- as.ffdf(x)
	}
	
	N <- nrow(x)
	isNumeric <- !sapply(physical(x), is.factor)
	
	#TODO randomize initial ordering
	ordered <- physical(x)
	
	ordered[!isNumeric] <- lapply(ordered[!isNumeric], function(f) {levels(f) <- NULL; f})
	ordered <- lapply(ordered, fforder)
		
	structure(
		list( data = x
			, ordered = ordered
		    )
		, class="prepared"
	)
}

#TODO currently one sortcolumn supported
createBin <- function(o, from=0, to=1, nbins=100, decreasing=FALSE){
	N <- length(o)
	bin <- ff(0, vmode="integer", length = N)
	from <- max(floor(from*N), 1L)
	to <- min(ceiling(to*N), N)
	
	chunks <- chunk(from=from, to=to, length.out=nbins, method="seq")
	
	for (i in seq_along(chunks)){
		b <- o[chunks[[i]]]
		print(list(i=i, b=length(b), chunk=chunks[[i]]))
		bin[b] <- i
	}
	bin
}

bin_data <- function(x, bin, nbins){
	lapply(physical(x), function(v){
		if (is.factor(v)){
			bt <- binned_tabulate.ff(v, bin, nbins, nlevels(v))
			bt / rowSums(bt)
		} else {
			bs <- binned_sum.ff(v, bin, nbins)
			
			count <- bs[,1]
			mean <- bs[,2] / count
			complete <- 1 - (bs[,3] / count)
			
			if (is.logical(v)){
				cbind(count, "FALSE"=(1-mean), "TRUE"=mean, "NA"=bs[,3]/count)				
			} else {
				cbind(count, mean=mean, complete=complete)
			}
		}
	})
}


# # quick testing
# require(ggplot2)
# x <- diamonds
# x <- iris
# 
# px <- prepare(x)
# 
# x.data <- px$data
# x.ordered <- px$ordered
# 
# system.time({
# bin <- createBin( x.ordered[[1]]
# 		 , nbins=10
# 	     , from = 0.2
# 		 )
# bin
# binned_sum.ff(x.data[[1]], bin, 10)
# 
# })
# 
# 
# agg <- bin_data(x.data, bin, nbins=10)
# agg
# 
# # x <- NULL
# # for (i in 1:100) x <- ffdfappend(x, diamonds)
# # nrow(x)