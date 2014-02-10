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
#' @param sample sample or use whole dataset?
#' @param sampleBinSize sample size per bin
#' @export
#' @import ffbase
bin_data <- function( p, sortCol=1L, cols=seq_along(p$data), from=0, to=1
					, nbins=100L
					, decreasing = FALSE
					, sample=FALSE
					, sampleBinSize=100){
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
	nbins <- max(min(nbins, n), 2L)
	if (decreasing){
		v_w <- rev(v_w)
	}
	# set window
	vw(o) <- v_w
	
	
	if (sample){
		# if n < maxN(=sampleBinSize*nbins) but N >> n, then N=M, so we still have to loop through whole o vector.
		
		cat("sample\n")

		# n_s is wanted total number of data points in the sample
		n_s <- min(nbins * sampleBinSize, n)
		# n_s/n is sample fraction, M = expected number of records in chunk
		M = N*(n_s/n)
		
		Mmax <- sum(chunk(x[[1]])[[1]])
		
		# determine chunk size (larger then M, but smaller than max chuncksize)
		# M_chunk_approx: expected chunksize such that one chunk contains M usable records + 5*standard deviation
		Mchunk_approx <- sqrt(M/N)*N
 		Mchunk_approx <- Mchunk_approx + 
 			5 * sqrt(Mchunk_approx * (M/N) * (1-M/N))
		
		Mchunk <- min(max(M, Mchunk_approx), Mmax)

		## WHAT TO DO IF M > max chunksize?
		if (M>Mmax) M <- Mchunk
		
		# We have to loop though o from head to tail in order to sample from the whole distribution.
		# Solution: sample evenly from o
		o_s <- o[round(seq(.5+(n/(2*Mchunk)),
						   .5+n-(n/(2*Mchunk)), 
						   length.out=Mchunk))]
		o_s <- o_s[o_s<=Mchunk]
		los <- length(o_s)
		cat("length o_s: ", los, "\n")
		cat("needed (M): ", M, "\n")
		
		o <- ff(o_s[round(
			seq(.5+(los/(2*M)),.5+los-(los/(2*M)), length.out=M))], vmode="integer")
	} else {
		cat("full data set/large sample \n")
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
# x <- as.ffdf(diamonds)
# 
# # blow the data set 2^10 times up.
# for (i in 1:8){
# 	x <- ffdfappend(x, x)
# 	cat("\rnrow(x): ", nrow(x))
# }
# 
# px <- tablePrepare(x)
# 
# system.time(
# 	agg <- bin_data(px, sortCol=1, nbins=100, sample=TRUE, sampleBinSize=1e2)
# )
# 
# system.time(
# 	agg2 <- bin_data(px, sortCol=1, nbins=100, sample=FALSE)
# )
# 
# for (v in names(agg)){
# 	if (colnames(agg[[v]])[2] == "mean"){
# 		d <- 100*(agg2[[v]][,2] - agg[[v]][,2])/agg2[[v]][,2]
# 		plot(seq_along(d), d,main=v)
# 	}
# }
# carat_diff_p <- 100*(agg2$carat-agg$carat)/agg2$carat
# plot(1:nrow(carat_diff_p), carat_diff_p[,"mean"])
# summary(carat_diff_p)

# 
# system.time(
# 	agg2 <- bin_data2(px, sortCol=1, nbins=100)
# )

# agg3 <- cbind(agg$carat, agg2$carat)
# # binRanges(2, 100, nbins=3)
# 
# 
# x.big <- NULL
# for (i in 1:100){
# 	x.big <- ffdfappend(x.big, x)
# 	cat("\r", i)
# }
# save.ffdf(x.big)
# 
#load.ffdf("ffdb/")
#px.big <- tablePrepare(x.big)
#py.big <- tablePrepare(y.big)
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