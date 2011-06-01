preprocess.data.table <-
function(dat, colNames, sortCol,  decreasing, scales, pals, nBins, from, to) {

	n <- length(colNames)

	#############################
	## Determine column classes
	#############################
	datClasses <- sapply(dat,FUN=function(x)class(x)[1])

	## find numerical variables
	isNumber <- (datClasses %in% c("numeric","integer"))

	## cast logical columns to factors
	for (i in which(datClasses == "logical")) {
		dat[[i]] <- factor(dat[[i]], levels=c("TRUE", "FALSE"))
	}
	
	## cast non-factor columns that are non-numeric either to factor
	isFactor <- (datClasses == "factor")
	castToFactor <- !isFactor & !isNumber
	
	for (i in colNames[castToFactor]) {
		dat[[i]] <- as.factor(dat[[i]])
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
	datList <- mapply(as.list(subset(dat, select=colNames[sortCol])), decreasing, isNumber[sortCol], FUN=function(vec, decr, isNum) {
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

	setkey(dat, aggIndex)
	
	#####################
	## Aggregate numeric variables
	#####################
	if (sum(isNumber)>0) {
	
		## bypass Rcmd warning
		aggIndex <- NULL; rm(aggIndex)

		## calculate means
		datMean <- dat[, c(colNames[isNumber], "aggIndex"), with=FALSE][,lapply(.SD, function(x)mean(x, na.rm=TRUE)),by=aggIndex]
		
		datMean <- subset(datMean, !is.na(datMean$aggIndex), select=names(datMean)[-1])
		
		

		## calculate completion percentages
		datCompl <- dat[, c(colNames[isNumber], "aggIndex"), with=FALSE][,lapply(.SD, function(x){sum(!is.na(x))/length(x)}),by=aggIndex]

		datCompl <- datCompl[!is.na(datCompl$aggIndex), names(datCompl)[-1], with=FALSE]

		datMissing <- 1 - datCompl
      
		#calculate the min and max value of each column
		datRange <- as.data.frame(lapply(subset(dat, select=colNames[isNumber]), range))
		
		#use it to calculate an lower and upper boundary for each bin
		datLower <- (datCompl * datMean) + (datMissing * rep(as.integer(datRange[1,]), each=nBins))
		datUpper <- (datCompl * datMean) + (datMissing * rep(as.integer(datRange[2,]), each=nBins))

		#print(datLower)
		#print(datMean)
		#print(datUpper)
		#print(datCompl)

		#this should not be in percentage, correct it later...
		datCompl <- 100 * datCompl

		## set means of bins with all missings to 0
		datMean <- data.table(mapply(datMean, datCompl, FUN=function(x,y)ifelse(y==0, 0, x)))
		
	}
		
	#####################
	## Aggregate categorical variables
	#####################
	if (any(!isNumber)) {	
		datFreq <- lapply(dat[, colNames[!isNumber], with=FALSE], FUN=getFreqTable_DT, dat$aggIndex, nBins)
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
	paletNr <- 1
	scales <- rep(scales, length.out=sum(isNumber))
	scalesNr <- 1
	for (i in 1:n) {
		sortc <- ifelse(i %in% sortCol, ifelse(decreasing[which(i==sortCol)], "decreasing", "increasing"), "")
		col <- list(name = colNames[i], isnumeric = isNumber[i], sort=sortc)
		if (isNumber[i]) {
			col$mean <- datMean[[colNames[i]]]
			col$compl <- datCompl[[colNames[i]]]
			col$lower <- datLower[[colNames[i]]]
			col$upper <- datUpper[[colNames[i]]]
			col$scale_init <- scales[scalesNr]
			scalesNr <- scalesNr + 1
		} else {
			col$freq <- datFreq[[colNames[i]]]$freqTable
			col$categories <- datFreq[[colNames[i]]]$categories
			col$palet <- pals[[paletNr]]
			paletNr <- ifelse(paletNr==length(pals), 1, paletNr + 1)
		}
 		tab$columns[[i]] <- col
	}
	
	return(tab)
}