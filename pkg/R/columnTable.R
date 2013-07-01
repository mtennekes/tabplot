columnTable <-
function(bd, datName, colNames, subset_string, sortCol,  decreasing, scales, pals, change_palette_type_at, colorNA, numPals, nBins, from, to, N, N.original) {
	
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
	binSizes <-	getBinSizes(N, nBins, decreasing)
	#print(list(binSizes=binSizes, bd=bd[[1]][,1]))


	#############################
	##
	## Create list object that contains all data needed to plot
	##
	#############################
	isNumber <- as.vector(sapply(bd, function(agg) colnames(agg)[2] == "mean"))
	
	tab <- structure(list(
		dataset = datName,
		filter = subset_string,
		nBins = nBins,
		binSizes = binSizes,
		n=N,
		m = m,
		colNames = colNames,
		isNumber = isNumber),
		class="tabplot")
	
	sort_decreasing <- rep(NA, m)
	sort_decreasing[sortCol] <- decreasing

	tab$sort_decreasing <- sort_decreasing
		
	## tab$row contains info about bins/y-axis
	tab$rows <- list( heights = -(binSizes/N)
	                , y = 1- c(0,cumsum(binSizes/N)[-nBins])
	                , n = N
					  , N = N.original
	                , from = from
	                , to = to
	                , marks = pretty(c(from, to), 10)
	                )

	# create column list
	tab$columns <- mapply(function(agg, name, isnum, sort, pal, numscale, numpal) {
		col <- list(name=name, isnumeric=isnum, sort_decreasing=sort)
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
			col$colorNA <- colorNA
		}
		col
	}, bd, colNames, isNumber, sort_decreasing, pals, scales, numPals, SIMPLIFY=FALSE)
	tab
}