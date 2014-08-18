columnTable <-
function(bd, datName, colNames, subset_string, sortCol,  decreasing, scales, pals, change_palette_type_at, rev_legend, colorNA, numPals, nBins, from, to, N, n) {
	
	m <- length(bd)
	nr <- nBins
	colNames <- names(bd)
	
	#####################################
	## Grammar of Graphics: Stats
	##
	## Perform statistical operations
	#####################################
	
	## Determine viewport, and check if nBins is at least number of items
	#vp <- tableViewport(nr, from, to)
	#if (nBins > vp$m) nBins <- vp$m

	## Calculate bin sizes
	binSizes <-	getBinSizes(n, nBins, decreasing)
	#print(list(binSizes=binSizes, bd=bd[[1]][,1]))


	#############################
	##
	## Create list object that contains all data needed to plot
	##
	#############################
	isNumber <- as.vector(sapply(bd, function(agg) colnames(agg)[2] == "mean"))
	
	tab <- structure(list(
		dataset = datName,
		select = colNames,
		subset = subset_string,
		nBins = nBins,
		binSizes = binSizes,
		sortCol=sortCol,
		decreasing=decreasing,
		from = from,
		to = to,
		n=n,
		N=N,
		m = m,
		isNumber = isNumber),
		class="tabplot")
	
	## tab$row contains info about bins/y-axis
	tab$rows <- list( heights = -(binSizes/n)
	                , y = 1- c(0,cumsum(binSizes/n)[-nBins])
	                , marks = pretty(c(from, to), 10)
	                )

	# create column list
	tab$columns <- mapply(function(agg, name, isnum, pal, numscale, numpal, revl) {
		col <- list(name=name, isnumeric=isnum)
		categories <- colnames(agg)
		dimnames(agg) <- NULL
		if (isnum) {
			col$mean <- agg[,2]
			col$compl <- 100*agg[,3]
			col$scale_init <- numscale
			col$paletname <- numpal
		} else {
			col$freq <- agg
			col$categories <- categories
			col$categories[ncol(agg)] <- "missing"
			col$paletname <- pal$name
			col$palet_recycled <- (ncol(agg)-1 <= change_palette_type_at)
			col$palet <- pal$palette
			col$rev_legend <- revl
			col$colorNA <- colorNA
		}
		col
	}, bd, colNames, isNumber, pals, scales, numPals, rev_legend, SIMPLIFY=FALSE)
	tab
}