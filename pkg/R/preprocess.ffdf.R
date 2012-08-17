preprocess.ffdf <-
function(dat, datName, filterName, colNames, sortCol,  decreasing, scales, max_levels, pals, recycle_palette, colorNA, numPals, nBins, from, to) {
   if (!require(ff)){
		stop("This function needs package ff")
   }   
	n <- length(colNames)
   #############################
	## Determine column classes
	#############################
	isLogical <- vmode(dat) == "logical"
	for (i in which(isLogical)) {
		levels(dat[,i]) <- c("TRUE", "FALSE")
	}

	isBoolean <- vmode(dat) == "boolean"

   #isFactor <- sapply(physical(dat), is.factor)	
    isFactor <- sapply(physical(dat), function(x)nlevels(x)!=0)	
   
	## find numerical variables
	isNumber <- !isFactor & !isLogical & !isBoolean
	
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
	
	# put all columns that are sorted in a list, and if decreasing, then change sign ('order' cannot handle a vectorized decreasing)
	datList <- lapply( sortCol
					 , function(col){
					    col <- dat[[col]]
					    if (is.factor(col)){
							levels(col) <- NULL
					    }						
						col
					 }
					 )
	
	datList$rand <- rand
	datList$decreasing <- decreasing
	
	# order all columns that are sorted
	o <- fforder(do.call(fforder,datList))
	
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
	if (any(isNumber)) {
		
		numcols <- names(dat)[as.which(isNumber)] # needed because isNumber is otherwise recycled!!
		
		datSum <- datCompl <- NULL

		## bypass Rcmd warning
		aggIndex <- NULL; rm(aggIndex)

		.SD <- NULL; rm(.SD); #trick R CMD check
		for (i in chunk(dat)){
			cdat <- data.table(dat[i,])[, c(numcols, "aggIndex"), with=FALSE]
			setkey(cdat, aggIndex)
			dsum <- cdat[, lapply(.SD, function(x)sum(as.numeric(x), na.rm=TRUE)), by=aggIndex]

			datSum <- rbind(datSum, dsum)
			## calculate completion percentages
			dcompl <- cdat[, lapply(.SD, function(x)sum(!is.na(x))), by=aggIndex]

			datCompl <- rbind(datCompl, dcompl)
		}

		datSum <- tail(datSum[, lapply(.SD, sum), by=aggIndex], nBins) #to exclude aggIndex=NA
		datSum$binSizes <- binSizes[datSum$aggIndex]
		
		datCompl <- datCompl[, lapply(.SD, sum), by=aggIndex][!is.na(aggIndex),]
		datCompl$binSizes <- binSizes[datSum$aggIndex]
		datCompl <- as.data.table(lapply(datCompl[, numcols, with=FALSE], FUN="/", datCompl$binSizes))

		datMean <- datSum[, numcols, with=FALSE] / datCompl[, numcols, with=FALSE]
		datMean <- as.data.table(lapply(datMean, FUN="/", datSum$binSizes))
	
		datCompl <- 100 * datCompl

	}
	#####################
	## Aggregate categorical variables
	#####################
	if (any(isFactor | isLogical)) {
		catcols <- names(dat)[as.which(isFactor | isLogical)] # needed because isNumber is otherwise recycled!!

		datFreq <- list()
		
		chunks <- chunk(dat)
		
		cdat <- as.data.table(dat[chunks[[1]], c(catcols, "aggIndex")])
		setkey(cdat, aggIndex)

		# cast logicals to factors
		for (col in colNames[isLogical]) {
			cdat[, col:=factor(cdat[[col]], levels=c("TRUE", "FALSE")), with=FALSE]
		}
		
		paltype <- rep("recycled", n)
		for (col in catcols) {
			if (nlevels(cdat[[col]]) > recycle_palette) {
				paltype[which(col==colNames)] <- "interpolate"
			}
		}
		
		datFreq <- aggCatCols(cdat, catcols, max_levels)
	
		for (i in chunks[-1]){
			cdat <- as.data.table(dat[i, c(catcols, "aggIndex")])
			setkey(cdat, aggIndex)

			# cast logicals to factors
			for (col in colNames[isLogical]) {
				cdat[, col:=factor(cdat[[col]], levels=c("TRUE", "FALSE")), with=FALSE]
			}
			
			datFreq2 <- aggCatCols(cdat, catcols, max_levels)
			datFreq <- mapply(datFreq, datFreq2, FUN=function(df1, df2){
					return(list(freqTable=df1$freqTable + df2$freqTable, categories=df1$categories))
				}, SIMPLIFY=FALSE)
		}
	}	

  	#####################
	## Aggregate boolean variables (not logical!)
	#####################
	if (any(isBoolean)) {
		#browser()
		blncols <- names(dat)[as.which(isBoolean)] # needed because isNumber is otherwise recycled!!
				
		datFreqB <- list()
		
		chunks <- chunk(dat)
		
		for (i in chunks){
			cdat <- na.omit(data.table(dat[i,])[, c(blncols, "aggIndex"), with=FALSE]) # works because there are no NA's in data (only in aggIndex)
			setkey(cdat, aggIndex)
			dsum <- cdat[, lapply(.SD, function(x)sum(x, na.rm=TRUE)), by=aggIndex]

			datFreqB2 <- lapply(dsum[, blncols, with=FALSE], FUN=function(x){
					y <- unlist(x)
					list(freqTable=matrix(c(y, binSizes-y), ncol=2), categories=c("TRUE", "FALSE"))
				})

			if (length(datFreqB)==0)
				datFreqB <- datFreqB2
			else
				datFreqB <- mapply(datFreqB, datFreqB2, FUN=function(df1, df2){
						return(list(freqTable=df1$freqTable + df2$freqTable, categories=df1$categories))
				}, SIMPLIFY=FALSE)
	
		}
	
		if (exists("datFreq"))
			datFreq <- c(datFreq, datFreqB)
		else
			datFreq <- datFreqB
	}   
   
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
		sortc <- ifelse(i %in% sortCol, ifelse(decreasing[which(i==sortCol)], 
											    "decreasing", "increasing"), "")
		col <- list(name = colNames[i], isnumeric = isNumber[i], sort=sortc)
		if (isNumber[i]) {
			col$mean <- datMean[[colNames[i]]]
			col$compl <- datCompl[[colNames[i]]]
			# TODO
			#col$lower <- datLower[[colNames[i]]]
			#col$upper <- datUpper[[colNames[i]]]
			col$scale_init <- scales[scalesNr]
			col$paletname <- numP[scalesNr]
			scalesNr <- scalesNr + 1
		} else {
			col$freq <- datFreq[[colNames[i]]]$freqTable
			col$categories <- datFreq[[colNames[i]]]$categories
			col$paletname <- pals$name[paletNr]
			col$palettype <- paltype[i]
			col$palet <- pals$palette[[paletNr]]
			col$colorNA <- colorNA

			paletNr <- ifelse(paletNr==length(pals$name), 1, paletNr + 1)
		}
 		tab$columns[[i]] <- col
	}
	return(tab)
}