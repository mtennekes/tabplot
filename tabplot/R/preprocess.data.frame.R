preprocess.data.frame <-
function(dat, colNames, sortCol,  decreasing, scales, nBins, from, to) {


	n <- length(colNames)

	#############################
	## Determine column classes
	#############################
	datClasses <- sapply(dat,FUN=function(x)class(x)[1])

	## find numerical variables
	isNumber <- (datClasses %in% c("numeric","integer"))

	## cast logical columns to factors
	for (i in which(datClasses == "logical")) {
		dat[,i] <- factor(dat[,i], levels=c("TRUE", "FALSE"))
	}
	
	## cast non-factor columns that are non-numeric either to factor
	isFactor <- (datClasses == "factor")
	castToFactor <- !isFactor & !isNumber
	
	for (i in colNames[castToFactor]) {
		dat[,i] <- as.factor(dat[,i])
	}

	#####################################
	## Grammar of Graphics: Stats
	##
	## Perform statistical operations
	#####################################
	
	## Determine viewport, and check if nBins is at least number of items
	vp <- tableViewport(nrow(dat), from, to)
	if (nBins > vp$m) nBins <- vp$m

	## Calculate bin sizes
	binSizes <-	getBinSizes(vp$m, nBins)


	#####################
	## Determine bin indices (needed for aggregation)
	#####################
	
	# create random vector
	rand <- sample.int(nrow(dat), nrow(dat))
	
	# put all columns that are sorted in a list, and if decreasing, then change sign ('order' cannot handle a vectorized decreasing)
	datList <- mapply(as.list(dat[sortCol]), decreasing, isNumber[sortCol], FUN=function(vec, decr, isNum) {
			if (decr & isNum) {
				return(-vec)
			} else if (decr) {
				return(-as.integer(vec))
			} else {
				return(vec)
			}
		}, SIMPLIFY=FALSE)
	
	# order all columns that are sorted
	o <- order(do.call(order, as.list(c(datList, list(rand=rand)))))
	
	brks <- c(0, cumsum(binSizes)) + (vp$iFrom-1)
	
	# TODO in stead of precalculating the brks, we can also give the number of breaks and calculate binSizes from summing the bins.
	# RE: Tried it, by cut(o, brks=nBins, ...) gives strange results (first and last bin were smaller)
	dat$aggIndex <- cut(o, brks, right=TRUE, labels=FALSE)

	# to make aggregation process faster(?)
	dat <- dat[!is.na(dat$aggIndex),]

	#####################
	## Aggregate numeric variables
	#####################
	if (sum(isNumber)>0) {
	
		## bypass Rcmd warning
		aggIndex <- NULL; rm(aggIndex)

		## calculate means
		datMean <- ddply(dat, .(aggIndex), numcolwise(mean), na.rm=TRUE)
		datMean <- datMean[-(nBins+1),-ncol(datMean), drop=FALSE]
		
		## calculate completion percentages
		datCompl <- ddply(dat, .(aggIndex), numcolwise(function(x){sum(is.na(x))}))
		datCompl <- datCompl[-(nBins+1),-ncol(datCompl), drop=FALSE]
		datCompl <- as.data.frame(apply(datCompl, 2, function(x,y){100-floor(x/y*100)}, binSizes))

		## set means of bins with all missings to 0
		datMean[datCompl==0] <- 0
	}
		
	#####################
	## Aggregate categorical variables
	#####################
	if (any(!isNumber)) {	
		datFreq <- lapply(dat[!isNumber], FUN=getFreqTable, dat$aggIndex, nBins)
	}
	
	#############################
	##
	## Create list object that contains all data needed to plot
	##
	#############################

	tab <- list()
	tab$n <- n
	tab$nBins <- nBins
	tab$binSizes <- binSizes
	tab$scales <- scales
	tab$isNumber <- isNumber
	## tab$row contains info about bins/y-axis

	tab$rows <- list( heights = -(binSizes/vp$m)
	                , y = 1- c(0,cumsum(binSizes/vp$m)[-nBins])
	                , m = vp$m
	                , from = from
	                , to = to
	                , marks = pretty(c(from, to), 10)
	                )
	
	## create column list
	tab$columns <- list()
	for (i in 1:n) {
		sortc <- ifelse(i %in% sortCol, ifelse(decreasing[which(i==sortCol)], "decreasing", "increasing"), "")
		col <- list(name = colNames[i], isnumeric = isNumber[i], sort=sortc)
		if (isNumber[i]) {
			col$mean <- datMean[[colNames[i]]]
			col$compl <- datCompl[[colNames[i]]]
		} else {
			col$freq <- datFreq[[colNames[i]]]$freqTable
			col$categories <- datFreq[[colNames[i]]]$categories
		}
		tab$columns[[i]] <- col
	}
	
	return(tab)
}