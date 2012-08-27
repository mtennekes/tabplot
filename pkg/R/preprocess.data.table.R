preprocess.data.table <-
function(dat, datName, filterName, colNames, sortCol,  decreasing, scales, pals, colorNA, numPals, nBins, from, to) {
	

	
	n <- length(colNames)
	nr <- nrow(dat)
	
	## Optimize space (by calling gc()) for datasets of at least 10 million rows (arbitrary number)
	#optSpace <- (object.size(dat)*2.5 > (memory.limit() - memory.size())*2^20)
	optSpace <- (nr >= 1e7)
	
	
	#############################
	## Determine column classes
	#############################
	datClasses <- sapply(dat[1,],FUN=function(x)class(x)[1])

	
	
	
	## cast logical columns to factors
	isLogical <- datClasses == "logical"
	for (i in which(isLogical)) {
		dat[, i:=factor(dat[[i]], levels=c("TRUE", "FALSE")), with=FALSE]
	}
	
	## cast date/time columns to factors
	isDateTime <- (datClasses %in% c("POSIXct", "POSIXlt", "Date"))
	for (i in which(isDateTime)) {
		dat[, i:=datetime2fac(dat[[i]]), with=FALSE]
	}
	
	## cast non-factor columns that are non-numeric either to factor

	isNumber <- (datClasses %in% c("numeric","integer"))
	isFactor <- (datClasses %in% c("factor", "ordered"))
	castToFactor <- !isFactor & !isNumber & !isLogical & !isDateTime
	
	for (i in colNames[castToFactor]) {
		dat[, i:=as.factor(dat[[i]]), with=FALSE]
	}

	
	
	#####################################
	## Grammar of Graphics: Stats
	##
	## Perform statistical operations
	#####################################
	
	## Determine viewport, and check if nBins is at least number of items
	vp <- tableViewport(nr, from, to)
	if (nBins > vp$m) nBins <- vp$m

	## Calculate bin sizes
	binSizes <-	getBinSizes(vp$m, nBins)


	#####################
	## Determine bin indices (needed for aggregation)
	#####################
	#browser()
	
	colNames <- copy(colNames)
	
	# create random vector
	randCol <- NULL; rm(randCol); #trick R CMD check
	
	
	#dat[, randCol:= 1]
	if (optSpace) gc()
	dat[, randCol:= sample.int(nr, nr)]
	

	# put all columns that are sorted in a list, and if decreasing, then change sign ('order' cannot handle a vectorized decreasing)
	
	#isNumber[sortCol]
	
	sortColNames <- c(colNames[sortCol], "randCol")
	for (i in colNames[sortCol[decreasing & isNumber[sortCol]]]) {
		colName <- paste(i,"decreasing", sep="_")
		sortColNames[sortColNames==i] <- colName
		dat[, colName:=-dat[[i]], with=FALSE]
	}
	

	for (i in colNames[sortCol[decreasing & !isNumber[sortCol]]]) {
		colName <- paste(i, "decreasing", sep="_")
		sortColNames[sortColNames==i] <- colName
		dat[, colName:=-as.integer(dat[[i]]), with=FALSE]
	}
	
	if (optSpace) gc()
	o <- order(do.call(order, dat[, sortColNames, with=FALSE]))
	
	
	extraCols <- setdiff(sortColNames, colNames)
	for (col in extraCols) dat[, col:=NULL, with=FALSE]
	# dat[, extraCols:=NULL, with=FALSE] somehow doesn't work
	

	#colNames[sortCol[decreasing & !isNumber[sortCol]]]
	
	
	
	
	#for (i in sortCol[] which(decreasing & isNumber[sortCol]))
	
	
# 	datList <- mapply(subset(dat, select=colNames[sortCol]), decreasing, isNumber[sortCol], FUN=function(vec, decr, isNum) {
# 			if (decr & isNum) {
# 				return(-vec)
# 			} else if (decr) {
# 				return(-as.integer(vec))
# 			} else {
# 				return(vec)
# 			}
# 		}, SIMPLIFY=FALSE)
	
	# order all columns that are sorted
# 	o <- order(do.call(order, as.list(c(datList, list(rand=rand)))))
	
	brks <- c(0, cumsum(binSizes)) + (vp$iFrom-1)
	
	# TODO in stead of precalculating the brks, we can also give the number of breaks and calculate binSizes from summing the bins.
	# RE: Tried it, by cut(o, brks=nBins, ...) gives strange results (first and last bin were smaller)
	if (optSpace) gc()
	## bypass Rcmd warning
	aggIndex <- NULL; rm(aggIndex); #trick R CMD check
	dat[, aggIndex:= cut(o, brks, right=TRUE, labels=FALSE)]
	
	setkey(dat, aggIndex)
	
	#dat <- dat[!is.na(dat$aggIndex),]
	
	rm(o)

	#####################
	## Aggregate numeric variables
	#####################
	if (sum(isNumber)>0) {
	
	
		## calculate means
		.SD <- NULL; rm(.SD); #trick R CMD check
		if (optSpace) gc()
		datMean <- dat[, c(colNames[isNumber], "aggIndex"), with=FALSE][,lapply(.SD, function(x)mean(x, na.rm=TRUE)),by=aggIndex]
		
		datMean <- subset(datMean, !is.na(datMean$aggIndex), select=names(datMean)[-1])
		
		

		## calculate completion percentages
		if (optSpace) gc()
		datCompl <- dat[, c(colNames[isNumber], "aggIndex"), with=FALSE][,lapply(.SD, function(x){sum(!is.na(x))/length(x)}),by=aggIndex]
		if (optSpace) gc()
		
		datCompl <- datCompl[!is.na(datCompl$aggIndex), names(datCompl)[-1], with=FALSE]

		datMissing <- 1 - datCompl
      
		#calculate the min and max value of each column
		if (optSpace) gc()
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
		if (optSpace) gc()
		datFreq <- list()
		
		datCat <- dat[J(1:nBins), c("aggIndex", colNames[!isNumber]), with=FALSE]
		
		for (col in colNames[!isNumber]) {
			datFreq[[col]] <- getFreqTable_DT(dat[, c("aggIndex", col), with=FALSE], col)
		}
		
		# more memory-efficient than datFreq <- lapply(dat[, colNames[!isNumber], with=FALSE], FUN=getFreqTable_DT, dat$aggIndex)
		
		
	}

	dat[, aggIndex:=NULL]
	
	#browser()
	#############################
	##
	## Create list object that contains all data needed to plot
	##
	#############################
	
	tab <- list()
	tab$dataset <- datName
	tab$filter <- filterName
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
	numP <- rep(numPals, length.out=sum(isNumber))
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
			col$paletname <- numP[scalesNr]
			scalesNr <- scalesNr + 1
		} else {
			col$freq <- datFreq[[colNames[i]]]$freqTable
			col$categories <- datFreq[[colNames[i]]]$categories
			col$paletname <- pals$name[paletNr]
			col$palet <- pals$palette[[paletNr]]
			col$colorNA <- colorNA
			paletNr <- ifelse(paletNr==length(pals$name), 1, paletNr + 1)
		}
 		tab$columns[[i]] <- col
	}

	return(tab)
}