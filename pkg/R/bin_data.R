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
bin_data <- function(p, sortCol=1L, cols=seq_along(p$data), from=0, to=1, nbins=100L, decreasing = FALSE, maxN=1e4){
	stopifnot(inherits(p, what="prepared"))
	x <- p$data[cols]
	o <- p$ordered[[cols[sortCol]]]
		
	# create bin vector
	N <- length(o)
	
	# first check how large from*N:to*N is. 
	# if this is larger then maxN sample otherwise don't
	
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
	
	if (maxN!=N) {
 		cat("sample\n")
 		sample_ids <- round(seq(from_r, to_r, length.out=maxN))
 		
 		sel <- bit(N)
 		sel[sample_ids] <- TRUE
 		
 		for (i in seq_along(chunks)){
 			b <- o[chunks[[i]]][sel[chunks[[i]]]]
 			bin[b] <- i
 		}
 	} else {
 		cat("full dataset\n")
		for (i in seq_along(chunks)){
			b <- o[chunks[[i]]]
			bin[b] <- i
		}
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

binned_sum2.ff <- function(x, index, nbins, ...){
	res <- matrix(0, nrow=nbins, ncol=3, dimnames=list(bin=1:nbins, c("count", "sum", "NA")))
	for (i in chunk(index, ...)){
		bin <- seq.int(i[1], i[2]) / ((sum(i)+1)/nbins) + 1
		res <- res + .Call("binned_sum", as.numeric(x[index[i]]), as.integer(bin), as.integer(nbins), PACKAGE = "ffbase")
	}
	res
}

binned_tabulate2.ff <- function(x, index, nbins, nlevels=nlevels(x), ...){
	lev <- if (nlevels(x)) c("na", levels(x)) else c("na", 1:nlevels)
	res <- matrix(0, nrow=nbins, ncol=length(lev), dimnames=list(bin=1:nbins, level=lev))
	for (i in chunk(index, ...)){
		bin <- seq.int(i[1], i[2]) / ((sum(i)+1)/nbins) + 1
		res <- res + .Call("binned_tabulate", x[index[i]], as.integer(bin), as.integer(nbins), as.integer(nlevels), PACKAGE = "ffbase")
	}
	res
}


bin_data2 <- function(p, sortCol=1L, cols=seq_along(p$data), from=0, to=1, nbins=100L, decreasing = FALSE, maxN=1e4){
	stopifnot(inherits(p, what="prepared"))
	x <- p$data[cols]
	o <- p$ordered[[cols[sortCol]]]
	
	N <- nrow(x)
	n <- floor(N*(to-from))
	
	# first check how large from*N:to*N is. 
	# if this is larger then maxN sample otherwise don't
	
	nbins <- max(min(nbins, n), 2)
	if (decreasing){
		from_r <- max(floor(N-N*to), 1L)
		to_r <- min(ceiling(N-N*from), N)
	} else {
		from_r <- max(floor(from*N), 1L)
		to_r <- min(ceiling(to*N), N)
	}
	if (maxN < n){
		cat("sample")
		n <- maxN
		o <- as.ff(o[round(seq(from_r, to_r, length.out=n))])
	} else if (n < N){
		o2 <- NULL
		for (i in chunk(o, from=from_r, to=to_r)){
			o2 <- ffappend(o2, o[i])
		}
		o <- o2
		cat("subset of data")
	} else {
		cat("full data set")
	}
	
	# do the actual binning
	lapply(physical(x), function(v){
		if (vmode(v)=="logical") {
			bs <- binned_sum2.ff(v, o, nbins)
			cbind("TRUE"=bs[,2], "FALSE"=bs[,1]-bs[,2], "<NA>"=bs[,3])
		}
		else if (is.factor.ff(v)){
			bt <- binned_tabulate2.ff(v, o, nbins, nlevels(v))
			cbind(bt[,-1], "<NA>"=bt[,1]) / rowSums(bt)
		} else {
			bs <- binned_sum2.ff(v, o, nbins)
			
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


# # quick testing
require(ggplot2)
x <- diamonds
#x <- iris

px <- tablePrepare(x)

system.time(
	agg <- bin_data(px, sortCol=1, nbins=100)
)

system.time(
	agg2 <- bin_data2(px, sortCol=1, nbins=100)
)

agg3 <- cbind(agg$carat, agg2$carat)
# binRanges(2, 100, nbins=3)


# x.big <- NULL
# for (i in 1:100){
# 	x.big <- ffdfappend(x.big, x)
# 	cat("\r", i)
# }
# save.ffdf(x.big)

load.ffdf("ffdb/")
px.big <- tablePrepare(x.big)
py.big <- tablePrepare(y.big)
pz.big <- tablePrepare(z.big)

system.time(
	agg <- bin_data(py.big, sortCol=1, nbins=100, maxN=1e4)
)

system.time(
	agg2 <- bin_data2(py.big, sortCol=1, from=0, to=0.0001, nbins=100, maxN=1e4)
)

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
