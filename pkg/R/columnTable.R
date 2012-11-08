columnTable <-
function(bd, datName, colNames, subset_string, sortCol,  decreasing, scales, pals, change_palette_type_at, colorNA, numPals, nBins, from, to, N) {
	
	n <- length(bd)
	nr <- nBins
	colNames <- names(bd)
	
	#####################################
	## Grammar of Graphics: Stats
	##
	## Perform statistical operations
	#####################################
	
	## Determine viewport, and check if nBins is at least number of items
	vp <- tableViewport(nr, from, to)
	if (nBins > vp$m) nBins <- vp$m

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
		n = n,
		colNames = colNames,
		isNumber = isNumber),
		class="tabplot")
	
	sort_decreasing <- rep(NA, n)
	sort_decreasing[sortCol] <- decreasing

	tab$sort_decreasing <- sort_decreasing
		
	## tab$row contains info about bins/y-axis
	tab$rows <- list( heights = -(binSizes/N)
	                , y = 1- c(0,cumsum(binSizes/N)[-nBins])
	                , m = N
	                , from = from
	                , to = to
	                , marks = pretty(c(from, to), 10)
	                )
	
	## create column list
	tab$columns <- list()
	paletNr <- 1
	scales <- rep(scales, length.out=sum(isNumber))
	numP <- rep(numPals, length.out=sum(isNumber))
	scalesNr <- 1
	for (i in 1:n) {
		col <- list(name = colNames[i], isnumeric = isNumber[i], sort_decreasing=sort_decreasing[i])
		agg <- bd[[col$name]]
		categories <- colnames(agg)
		dimnames(agg) <- NULL
		#col$agg <- agg
		
		if (isNumber[i]) {
			col$mean <- agg[,2]
			col$compl <- 100*agg[,3]
			col$scale_init <- scales[scalesNr]
			col$paletname <- numP[scalesNr]
			scalesNr <- scalesNr + 1
		} else {
			col$freq <- agg
			col$categories <- categories
			col$categories[ncol(agg)] <- "missing"
			col$paletname <- pals$name[paletNr]
			col$palet_recycled <- (ncol(agg)-1 <= change_palette_type_at)
			col$palet <- pals$palette[[paletNr]]
			col$colorNA <- colorNA
			paletNr <- ifelse(paletNr==length(pals$name), 1, paletNr + 1)
		}
 		tab$columns[[i]] <- col
	}

	tab
}