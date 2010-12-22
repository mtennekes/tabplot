preprocess.ffdf <-

function(dat, colNames=names(dat), sortCol=1,  decreasing=FALSE, scales="auto", nBins=100, from=0,to=100) {
   if (!require(ff)){
		stop("This function needs package ff")
   }   

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
	if (nBins > nrow(dat)) { 
		warning("Setting nBins (",nBins,") to number of rows (", nrow(dat), ")")
		nBins <- nrow(dat)
	}
	
	## Check from and to
	if (class(from)[1]!="numeric") stop("<from> is not numeric")
	if (class(to)[1]!="numeric") stop("<to> is not numeric")
	if (from < 0 && from > 100) stop("<from> is not a number in [0, 100]")
	if (to < 0 && to > 100) stop("<to> is not a number in [0, 100]")
	if (from >= to) stop("<from> is not smaller than <to>")


	#############################
	## Determine column classes
	#############################
	
	for (i in which(vmode(dat) == "logical")) {
		levels(dat[,i]) <- c("TRUE", "FALSE")
	}
	
	isFactor <- sapply(physical(dat), is.factor)	
	
	## find numerical variables
	isNumber <- !isFactor
	
	## cast logical columns to factors
	#TODO
	# POSIXct????
	
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
	rand <- ff(vmode="double", length=nrow(dat))
	for (i in chunk(rand)){
	   rand[i] <- runif(sum(i))
	}

	if (any(decreasing)){
	   stop("decreasing ordering is not implemented on ffdf")
	}
	
	# put all columns that are sorted in a list, and if decreasing, then change sign ('order' cannot handle a vectorized decreasing)
	#TODO implement decreasing
	datList <- dat[sortCol]
	datList$rand <- rand
	# order all columns that are sorted
	#print(datList)
	o <- fforder(do.call(fforder,physical(datList)))
	
	#print(o)
	brks <- c(0, cumsum(binSizes)) + (vp$iFrom-1)
	
	# TODO in stead of precalculating the brks, we can also give the number of breaks and calculate binSizes from summing the bins.
	# RE: Tried it, by cut(o, brks=nBins, ...) gives strange results (first and last bin were smaller)
	
	dat$aggIndex <- ff(length=nrow(dat), vmode="integer")
	
	for (i in chunk(o)){
	  dat$aggIndex[i] <- cut(o[as.which(i)], brks, right=TRUE, labels=FALSE)
    }
	
	#dat$aggIndex <- cut(o, brks, right=TRUE, labels=FALSE)

	#####################
	## Aggregate numeric variables
	#####################
	if (sum(isNumber)>0) {
		
		numcols <- names(dat)[as.which(isNumber)] # needed because isNumber is otherwise recycled!!
		
		## calculate means by dividing by binSizes and sum
		ncwmean <- function(df){
		   size <- binSizes[df$aggIndex]
		   l <- lapply( df[,numcols]
		              , function(x){
					       sum(x/size, na.rm=TRUE)
						}
					  )
		   names(l) <- numcols
		   as.data.frame(l)
		}

		ncomplete <- function(df){
		   size <- binSizes[df$aggIndex[1]] #otherwise multiple output...
		   l <- lapply( (df[numcols])
		              , function(x){
					      100*sum(!is.na(x))/size
						}
					  )
		   names(l) <- numcols
		   as.data.frame(l)
		}
		
		datMean <- datCompl <- NULL
		for (i in chunk(dat)){
			cdat <- dat[i,]
			dmean <- ddply(cdat, .(aggIndex), ncwmean)
			datMean <- rbind(datMean,dmean)
			## calculate completion percentages
			dcompl <- ddply(cdat, .(aggIndex), ncomplete)
			datCompl <- rbind(datCompl, dcompl)
		}

		datMean <- ddply(datMean, .(aggIndex), colwise(sum, numcols))
		datCompl <- ddply(datCompl, .(aggIndex), colwise(sum, numcols))
		
		datMean <- datMean[-(nBins+1),-1, drop=FALSE]
		datCompl <- datCompl[-(nBins+1),-1, drop=FALSE]
		#datCompl <- as.data.frame(apply(datCompl, 2, function(x,y){100-floor(x/y*100)}, binSizes))
		## set means of bins with all missings to 0
		#datMean[datCompl==0] <- 0
	}
	#####################
	## Aggregate categorical variables
	#####################
	if (any(!isNumber)) {
		freq <- list()
		for (i in chunk(dat)){
		   cdat <- dat[i,]
		   for (catCol in (names(cdat)[!isNumber])){
		      tab <- freq[[catCol]]
			  if(is.null(tab)) {
				tab <- as.matrix(table(cdat$aggIndex,cdat[[catCol]], useNA="always"))
			  }
			  else {
				tab <- tab + as.matrix(table(cdat$aggIndex,cdat[[catCol]], useNA=c("always")))
			  }
		      freq[[catCol]] <- tab
		   }
		}
		datFreq <- lapply( freq
		                 , function(x){
						      naCol <- ncol(x)
						      colnames(x)[naCol] <- "missing"
#							  if (max(x[,naCol] == 0)){ #drop nacol
#							     x <- x[,-naCol]
#							  }
							  list(freqTable=x[1:nBins,], categories=colnames(x))      
		                   })
		
		#datFreq <- lapply(dat[,][!isNumber], FUN=getFreqTable, dat$aggIndex[], nBins)
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
