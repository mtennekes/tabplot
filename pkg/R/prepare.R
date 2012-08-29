#' Prepares a data.frame or ffdf for tableplotting
prepare <- function(x, ...){
	# TODO set path where prepared data set should be stored
	if (is.data.frame(x)){
		x <- as.ffdf(x)
	}
	
	N <- nrow(x)
	isNumeric <- !sapply(physical(x), is.factor)
	print(isNumeric)
	
	#TODO randomize initial ordering
	ordered <- physical(x)
	
	ordered[!isNumeric] <- lapply(ordered[!isNumeric], function(f) {levels(f) <- NULL; f})
	ordered <- lapply(ordered, fforder)
		
	sorted <- lapply(1:length(ordered), function(o){
		x[[o]][ordered[[o]]]
	})
	
	ranked <- ordered
	ranked <- lapply(ordered, fforder)
	quantiled <- lapply(ranked, `/`, N)
	
	names(sorted) <- names(ranked) <- names(ordered)
	
	ordered <- do.call(ffdf, ordered)
	sorted <- do.call(ffdf, sorted)
	
	structure(
		list( data = x
			, ordered = ordered
			, sorted = sorted
			, ranked = ranked
			, quantiled = quantiled
		    )
		, class="prepared"
	)
}

createBin <- function(o, from=0, to=1, nbins=100, decrease=FALSE){
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
			cbind(bs[,1], bs[,2:3] / bs[,1])
		}
	})
}

# strategy 1
# use ff chunks and ordered index to calculate the means
# PRO, is simple, but behind the scenes the memory access is random (slower)
#
# strategy 2
# use ranked or quantiled in combination with findInterval and binned_sum/binned_mean
# findInterval is faster with a double vector (quantiled...)
#
# strategy 3
# use 

# quick testing
require(ggplot2)
x <- diamonds
x <- iris

px <- prepare(x)

x.data <- px$data
x.ordered <- px$ordered

system.time({
bin <- createBin( x.ordered[[1]]
		 , nbins=10
	     , from = 0.2
		 )
bin
binned_sum.ff(x.data[[1]], bin, 10)

})

system.time({
	nbins=10
	agg <- 

})

agg

# x <- NULL
# for (i in 1:100) x <- ffdfappend(x, diamonds)
# nrow(x)