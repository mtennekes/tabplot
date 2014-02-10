library(ggplot2)
library(ff)
data(diamonds)

bigdiamonds <- as.ffdf(diamonds[rep(1:nrow(diamonds), 1e2), ])
bigdiamonds <- as.ffdf(diamonds)

p1 <- tablePrepare(bigdiamonds)
tp1 <- tableplot(p1, sample=TRUE, sampleBinSize=1e2)

p2 <- tablePrepare(bigdiamonds)
tp2 <- tableplot(p2, sample=TRUE, sampleBinSize=1e2)

str(tp1)

# tableCompare <- function(tp1, tp2, mode="rel")
tp <- tp1

tp$columns <- mapply(function(col1, col2) {
	col <- col1
	if (col1$isnumeric) {
		if (mode=="abs") col$mean <- col2$mean - col1$mean
		if (mode=="rel") col$mean <- ((col2$mean - col1$mean) / col1$mean)*100
	} else {
		
		col$freq <- col2$freq - col1$freq
		
		ispos <- col$freq>=0
		isneg <- col$freq<0
		
		freqneg <- col$freq
		freqneg[!isneg] <- 0
		xinit <- rowSums(freqneg)
		
		xneg <- t(apply(freqneg, MARGIN=1, cumsum))
		
		width <- abs(col$freq)
		
		whichneg <- apply(isneg, MARGIN=1, FUN=which)
		whichpos <- apply(ispos, MARGIN=1, FUN=which)
		
		ids <- mapply(FUN=c, whichneg, whichpos, SIMPLIFY=FALSE)
		widths <- as.list(data.frame(t(width)))
		
		width <- t(mapply("[", widths, ids))
		
		
		x <- xinit + t(apply(width, MARGIN=1, cumsum))
		
		
		
		
	}
	col$scale_init <- "lin"
	col
}, tp1$columns, tp2$columns, SIMPLIFY=FALSE)

isNumber <- sapply(tp$columns, function(col) col$isnumeric)

IQR_bias <- 5
tp$columns[isNumber] <- lapply(tp$columns[isNumber], scaleNumCol, IQR_bias)


limitsX <- list()
bias_brokenX <- 0.8
tp$columns[isNumber] <- mapply(coorNumCol, tp$columns[isNumber], limitsX[isNumber], MoreArgs=list(bias_brokenX=bias_brokenX), SIMPLIFY=FALSE)

plot(tp)



str(tp)

## coordinates
tab$columns[!isNumber] <- lapply(tab$columns[!isNumber], coorCatCol, nBins)
tab$columns[isNumber] <- mapply(coorNumCol, tab$columns[isNumber], limitsX[isNumber], MoreArgs=list(bias_brokenX=bias_brokenX), SIMPLIFY=FALSE)
