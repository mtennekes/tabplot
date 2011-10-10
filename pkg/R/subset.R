# Subsetting a ffdfdata frame (modified from ffbase r62)
subset.ffdf <- function(x, subset, ...){
	y <- clone(x)
	n <- 0
	#subset <- eval(expression(substitute(subset)))
	#print(subset)
	#TODO check if subset is an expression or logical vector. If so, then idx vector
	for (i in chunk(x)){
	   dat <- x[i,]
	   row.names(dat) <- min(i):max(i)
	   #sel <- eval(substitute(subset), dat, parent.frame())
	   dat <- dat[subset[i],]
	   s <- nrow(dat) 
	   if (s > 0){
	      y[(n+1):(n+s),] <- dat
		  n <- n + s
	   }
	}
	nrow(y) <- n
	y 
}