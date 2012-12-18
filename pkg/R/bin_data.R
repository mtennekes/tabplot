#' Bin data
#' 
#' Working horse for tableplot, does the actual binning
#'
#' @param p prepared dataset (see \code{\link{tablePrepare}})
#' @param sortCol column on which the table will be sorted
#' @param cols columns of the data that will be used.
#' @param from lower boundary in quantiles
#' @param to upper boundary in quantiles
#' @param nbins number of bins
#' @param decreasing sort decreasingly
#' @export
bin_data <- function(p, sortCol=1L, cols=seq_along(p$data), from=0, to=1, nbins=100L, decreasing = FALSE){
	stopifnot(inherits(p, what="prepared"))
	x <- p$data[cols]
	o <- p$ordered[[cols[sortCol]]]
	
	# create bin vector
	N <- length(o)
	nbins <- max(min(nbins, as.integer(N*(to-from))), 2)
	bin <- ff(0L, vmode="integer", length = N)
	
	if (decreasing){
		from_r <- max(floor(N-N*to), 1L)
		to_r <- min(ceiling(N-N*from), N)
		chunks <- rev(binRanges(from=from_r, to=to_r, nbins=nbins))
	} else {
		from_r <- max(floor(from*N), 1L)
		to_r <- min(ceiling(to*N), N)
		chunks <- binRanges(from=from_r, to=to_r, nbins=nbins)
	}

	# assign bin numbers
	for (i in seq_along(chunks)){
		b <- o[chunks[[i]]]
		bin[b] <- i
	}

	# do the actual binning
	lapply(physical(x), function(v){
		if (vmode(v)=="logical") {
			bs <- binned_sum.ff(v, bin, nbins)
			cbind("TRUE"=bs[,2], "FALSE"=bs[,1]-bs[,2], "<NA>"=bs[,3])
		}
		else if (is.factor.ff(v)){
			bt <- binned_tabulate.ff(v, bin, nbins, nlevels(v))
			cbind(bt[,-1], "<NA>"=bt[,1]) / rowSums(bt)
		} else {
			bs <- binned_sum.ff(v, bin, nbins)
			
			count <- bs[,1]
			mean <- ifelse(count==0, NA, bs[,2] / count)
			na <- bs[,3] / (count + bs[,3])
			
			if (is.logical(v)){
				cbind(count, "FALSE"=(1-mean), "TRUE"=mean, "<NA>"=na)
			} else {
				cbind(count, mean=mean, complete = 1-na)
			}
		}
	})
}

binRanges <- function(from, to, nbins){
	r <- as.integer(seq(from, (to+1), length.out=nbins+1))
	
	f <- r[-(nbins+1)]
	t <- r[-1] - 1L
	
	mapply(ri, f, t, SIMPLIFY=FALSE)
}

# # quick testing
# require(ggplot2)
# x <- diamonds
# #x <- iris
# 
# px <- prepare(x)
# 
# system.time(
# 	agg <- bin_data(px, sortCol=2, nbins=100)
# )
# 
# binRanges(2, 100, nbins=3)
