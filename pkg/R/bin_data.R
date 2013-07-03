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
#' @param maxN the maximum number of objects
#' @export
bin_data <- function(p, sortCol=1L, cols=seq_along(p$data), from=0, to=1, nbins=100L, decreasing = FALSE, maxN=1e6){
	stopifnot(inherits(p, what="prepared"))
	x <- p$data[cols]
	o <- p$ordered[[cols[sortCol]]]
	bin <- NULL
	
	N <- nrow(x)
	
	# first check how large from*N:to*N is. 
	# if this is larger then maxN sample otherwise don't
	
	from_r <- floor(from*N)
	to_r <- ceiling(to*N)
	n <- to_r - from_r
	v_w <- c(from_r, n, N - to_r)
	nbins <- max(min(nbins, n), 2)
	if (decreasing){
		v_w <- rev(v_w)
	}
	# set window
	vw(o) <- v_w
	
	if (maxN < n && n <= 1e5){
		#cat("sample")
		index <- as.integer(seq(1, n, length.out=maxN)) 
		n <- maxN
		if (decreasing){
			index <- rev(index)
		}
		o <- as.ff(o[index])
	} else {
		#cat("full data set/large sample")
		bin <- ff(0L, length=N)
		for (i in chunk(o)){
			b <- as.integer(seq.int(i[1], i[2]) / ((n+1)/nbins) + 1)
			if (decreasing){ b <- (nbins+1L) - b}
			bin[o[i]] <- b
		}
		o <- NULL
	}
	
	# do the actual binning
	lapply(physical(x), function(v){
		if (vmode(v)=="logical") {
			bs <- binned_sum.ff(v, bin, nbins, INDEX=o)
			cbind("TRUE"=bs[,2], "FALSE"=bs[,1]-bs[,2], "<NA>"=bs[,3])
		}
		else if (is.factor.ff(v)){
			bt <- binned_tabulate.ff(v, bin, nbins, nlevels(v), INDEX=o)
			cbind(bt[,-1], "<NA>"=bt[,1]) / rowSums(bt)
		} else {
			bs <-binned_sum.ff(v, bin, nbins, INDEX=o)
			
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
# px <- tablePrepare(x)
# 
# system.time(
# 	agg <- bin_data(px, sortCol=1, nbins=100)
# )
# 
# system.time(
# 	agg2 <- bin_data2(px, sortCol=1, nbins=100)
# )

# agg3 <- cbind(agg$carat, agg2$carat)
# # binRanges(2, 100, nbins=3)
# 
# 
# # x.big <- NULL
# # for (i in 1:100){
# # 	x.big <- ffdfappend(x.big, x)
# # 	cat("\r", i)
# # }
# # save.ffdf(x.big)
# 
# load.ffdf("ffdb/")
# px.big <- tablePrepare(x.big)
# py.big <- tablePrepare(y.big)
# # pz.big <- tablePrepare(z.big)
# # 
# system.time(
# 	agg <- bin_data(px.big, sortCol=1, nbins=100, maxN=1e4)
# )
# 
# 
# system.time(
# 	agg2 <- bin_data2(px.big, sortCol=1, from=0, to=1, nbins=100, maxN=1e4)
# )
# 
# x <- ff(1:10)
# index <- ff(10:1, vmode="integer")
# nbins <- 3
# 
# binned_sum2.ff(x, index, nbins)
# 
# y <- ff(as.factor(c("M","M","V")))
# index <- ff(1:3)
# nbins <- 3
# binned_tabulate2.ff(y, index, nbins, 2)
