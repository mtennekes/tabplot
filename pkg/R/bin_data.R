#TODO currently one sortcolumn supported
# working horse for tableplot, does the actual binning
#' binning data
#' 
#' @param p prepared data.frame or ffdf (see \code{\link{prepare}})
#' @param sortCol column on which the table will be sorted
#' @param cols columns of the data that will be used.
#' @param from lower boundary in quantiles
#' @param to upper boundary in quantiles
#' @param nbins number of bins
#' @param decreasing should the variable 
#' @export
bin_data <- function(p, sortCol=1, cols=seq_along(p$data), from=0, to=1, nbins=100, decreasing = FALSE){
	stopifnot(inherits(p, what="prepared"))
	x <- p$data[cols]
	o <- p$ordered[[sortCol]]
	
	# create bin vector
	N <- length(o)
	bin <- ff(0L, vmode="integer", length = N)
	
	if (decreasing){
		from <- max(floor(N-N*to), 1L)
		to <- min(ceiling(N-N*from), N)
		# can be improved (not that difficult)
		chunks <- rev(chunk(from=from, to=to, length.out=nbins, method="seq"))
	} else {
		from <- max(floor(from*N), 1L)
		to <- min(ceiling(to*N), N)
		chunks <- chunk(from=from, to=to, length.out=nbins, method="seq")
	}

	# assign bin numbers
	for (i in seq_along(chunks)){
		b <- o[chunks[[i]]]
		bin[b] <- i
	}
	
	# do the actual binning
	lapply(physical(x), function(v){
		if (is.factor(v)){
			bt <- binned_tabulate.ff(v, bin, nbins, nlevels(v))
			cbind(bt[,-1], "<NA>"=bt[,1]) / rowSums(bt)
		} else {
			bs <- binned_sum.ff(v, bin, nbins)
			
			count <- bs[,1]
			mean <- bs[,2] / count
			complete <- 1 - (bs[,3] / count)
			
			if (is.logical(v)){
				cbind(count, "FALSE"=(1-mean), "TRUE"=mean, "<NA>"=bs[,3]/count)				
			} else {
				cbind(count, mean=mean, complete=complete)
			}
		}
	})
}

# # quick testing
# require(ggplot2)
# x <- diamonds
# #x <- iris
# 
# px <- prepare(x)
# 
# 
# system.time(
# agg <- bin_data(px, bin, sortCol=2, nbins=100)
# )
