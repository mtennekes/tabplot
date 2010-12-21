preprocess.data.frame <-
function(dat, colNames=names(dat), sortCol=1,  decreasing=FALSE, scales="auto", nBins=100, from=0,to=100) {


	#####################################
	## Check arguments and cast dat-columns to numeric or factor
	#####################################
	
	## Check dat
	if (nrow(dat)==0) stop("<dat> doesn't have any rows")
	if (nrow(dat)==1) stop("<dat> has only one row")
	
	## Check colNames
	if (class(colNames)[1]!="character") stop("<colNames> is not a character(vector)")
	if (!all(colNames %in% names(dat))) stop("<colNames> contains column names that are not found in <dat>")

	## Only select the columns of colNames
	dat <- dat[colNames]
	n <- length(colNames)

	## Check sortCol, and (if necessary) cast it to indices
	if (class(sortCol)[1]=="character") {
		if (!all(sortCol %in% colNames)) stop("invalid <sortCol>")
		sortCol <- sapply(sortCol, FUN=function(x) which(x==colNames))
	} else if (class(sortCol)[1] %in% c("numeric", "integer")) {
		if (any(sortCol > ncol(dat)) || any(sortCol < 1)) {
			stop("<sortCol> has an invalid value")
		}
	} else {
		stop("<sortCol> is not a character or numeric value or vector")
	}

	## Check decreasing vector
	if (class(decreasing)[1]!="logical") stop("<decreasing> is not a logical")
	if (length(sortCol) != length(decreasing)) stop("<sortCol> and <decreasing> have different lengths")
	
	## Check scales
	if (length(scales)==1) scales <- rep(scales, n)
	if (length(scales)!=length(colNames)) stop(paste("<scales> should be of length ", length(colNames)))
	if (length(setdiff(scales, c("auto", "lin", "log")))>0) stop("<scales> should consist of auto, lin and log")

	## Check nBins
	if (class(nBins)[1]!="numeric") stop("<nBins> is not numeric")
	if (nBins >= nrow(dat)) stop("<nBins> greater than number of rows in <dat>")
	
	## Check from and to
	if (class(from)[1]!="numeric") stop("<from> is not numeric")
	if (class(to)[1]!="numeric") stop("<to> is not numeric")
	if (from < 0 && from > 100) stop("<from> is not a number in [0, 100]")
	if (to < 0 && to > 100) stop("<to> is not a number in [0, 100]")
	if (from >= to) stop("<from> is not smaller than <to>")


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
	rand <- sample.int(nrow(dat))
	
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
	tab$rows <- list( heights = binSizes/vp$m 
	                , y = c(0,cumsum(binSizes/vp$m)[-nBins])
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