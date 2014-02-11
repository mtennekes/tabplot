"-.tabplot" <- function(tp1, tp2) {
	tp <- tp1
	tp$columns <- mapply(function(col1, col2) {
		col <- col1
		if (col1$isnumeric) {
			col$mean1 <- col1$mean
			col$mean2 <- col2$mean
			col$mean.diff <- col$mean <- col2$mean - col1$mean
			col$mean.diff.rel <- col$mean <- ((col2$mean - col1$mean) / col1$mean)*100
			col$mean <- NULL
			col$scale_init <- "lin"
		} else {
			
			col <- tp$columns[[4]]
			col1 <- tp1$columns[[4]]
			col2 <- tp2$columns[[4]]
			
			col$freq1 <- col1$freq
			col$freq2 <- col2$freq
			
			freq <- col$freq.diff <- col2$freq - col1$freq
			xinit <- apply(freq, MARGIN=1, function(x)sum(x[x<0]))
			
			ids <- t(apply(freq, MARGIN=1, orderRow))
			freq.sorted <- sortRows(freq, ids)
			
			widths <- abs(freq.sorted)
			x <- t(apply(widths, 1, cumsum)) + xinit
			x <- cbind(xinit, x[,1:(ncol(x)-1)])
			
			ids2 <- t(apply(ids, 1, order))
			
			col$x <- sortRows(x, ids2)
			
			col$widths <- sortRows(widths, ids2)
			
			col$x[col$x<0] <- col$x[col$x<0] - .04
			col$x[col$x>=0] <- col$x[col$x>=0] + .04
			
			col$x[col$widths==0] <- NA
			col$widths[col$widths==0] <- NA
			
			col$widths <- col$widths * .4807692
			col$x <- (col$x * .4807692) + 0.5
			
			col$freq <- NULL
			col
		}
		col
	}, tp1$columns, tp2$columns, SIMPLIFY=FALSE)
	
	isNumber <- sapply(tp$columns, function(col) col$isnumeric)
	
	tp$columns[isNumber] <- lapply(tp$columns[isNumber], scaleNumCol, IQR_bias=5, compare=TRUE)
	limitsX <- list()
	tp$columns[isNumber] <- mapply(coorNumCol, tp$columns[isNumber], limitsX[isNumber], MoreArgs=list(bias_brokenX=0.8, compare=TRUE), SIMPLIFY=FALSE)
	
	tp$n1 <- tp1$n
	tp$n2 <- tp2$n
	tp$N1 <- tp1$N
	tp$N2 <- tp2$N
	tp$n <- NULL
	tp$N <- NULL
	
	tp <- tp[c(1:9, 14:17, 10:13)]
	class(tp) <- "tabplot_compare"
	tp	
}

orderRow <- function(x) {
	c(which(x < 0), which(x == 0), which(x > 0))
}

sortRows <- function(x, ids) {
	t(apply(cbind(x, ids), MARGIN=1, function(x){
		n <- length(x) / 2
		y <- x[(n+1):(2*n)]
		x <- x[1:n]
		x[y]
	}))
}