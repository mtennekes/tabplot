preprocess.ffdf <-
function(dat, colNames, sortCol,  decreasing, scales, pals, nBins, from, to) {
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
	
	isFactor <- sapply(physical(dat), is.factor)	
	
	## find numerical variables
	isNumber <- !isFactor & !isLogical
	
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
	if (sum(isNumber)>0) {
		
		numcols <- names(dat)[as.which(isNumber)] # needed because isNumber is otherwise recycled!!
		
		datSum <- datCompl <- NULL

		## bypass Rcmd warning
		aggIndex <- NULL; rm(aggIndex)

		for (i in chunk(dat)){
			cdat <- data.table(dat[i,])[, c(numcols, "aggIndex"), with=FALSE]
			setkey(cdat, aggIndex)
			dsum <- cdat[, lapply(.SD, function(x)sum(x, na.rm=TRUE)), by=aggIndex]

			datSum <- rbind(datSum, dsum)
			## calculate completion percentages
			dcompl <- cdat[, lapply(.SD, function(x)sum(!is.na(x))), by=aggIndex]

			datCompl <- rbind(datCompl, dcompl)
		}

		datSum <- datSum[, lapply(.SD, sum), by=aggIndex][!is.na(aggIndex),]
		datSum$binSizes <- binSizes[datSum$aggIndex]
		
		datCompl <- datCompl[, lapply(.SD, sum), by=aggIndex][!is.na(aggIndex),]
		datCompl$binSizes <- binSizes[datSum$aggIndex]
		datCompl <- data.table(lapply(datCompl[, numcols, with=FALSE], FUN="/", datCompl$binSizes))

		datMean <- datSum[, numcols, with=FALSE] / datCompl[, numcols, with=FALSE]
		datMean <- data.table(lapply(datMean, FUN="/", datSum$binSizes))
	
		datCompl <- 100 * datCompl

	}
	#####################
	## Aggregate categorical variables
	#####################
	if (any(!isNumber)) {
		catcols <- names(dat)[as.which(!isNumber)] # needed because isNumber is otherwise recycled!!


		datFreq <- list()
		maxlevels <- max(sapply(dat[1,], nlevels))
		
		chunks <- chunk(dat)
		
		
		cdat <- dat[chunks[[1]], c(catcols, "aggIndex")]
		# cast logicals to factors
		for (i in colNames[isLogical]) {
			cdat[[i]] <- factor(cdat[[i]], levels=c("TRUE", "FALSE"))
		}

		datFreq <- lapply(cdat[, catcols], FUN=getFreqTable_DT, cdat$aggIndex, nBins, useNA="always")
	
		for (i in chunks[-1]){
			cdat <- dat[i, c(catcols, "aggIndex")]
			# cast logicals to factors
			for (i in colNames[isLogical]) {
				cdat[[i]] <- factor(cdat[[i]], levels=c("TRUE", "FALSE"))
			}

			datFreq2 <- lapply(cdat[, catcols], FUN=getFreqTable_DT, cdat$aggIndex, nBins, useNA="always")

			datFreq <- mapply(datFreq, datFreq2, FUN=function(df1, df2){
					return(list(freqTable=df1$freqTable + df2$freqTable, categories=df1$categories))
				}, SIMPLIFY=FALSE)
		}

		datFreq <- mapply(datFreq, FUN=function(df) {
				if (all(df$freqTable[,"missing"]==0)) {
					ncols <- ncol(df$freqTable)
					df$freqTable <- df$freqTable[,-ncols]
					df$categories <- df$categories[-ncols]
				}
				return(df)
			}, SIMPLIFY=FALSE)
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
	                , y = 1 - c(0,cumsum(binSizes/vp$m)[-nBins])
	                , m = vp$m
	                , from = from
	                , to = to
	                , marks = pretty(c(from, to), 10)
	                )
	
	## create column list
	tab$columns <- list()
	paletNr <- 1
	for (i in 1:n) {
		sortc <- ifelse(i %in% sortCol, ifelse(decreasing[which(i==sortCol)], "decreasing", "increasing"), "")
		col <- list(name = colNames[i], isnumeric = isNumber[i], sort=sortc)
		if (isNumber[i]) {
			col$mean <- datMean[[colNames[i]]]
			col$compl <- datCompl[[colNames[i]]]
		} else {
			col$freq <- datFreq[[colNames[i]]]$freqTable
			col$categories <- datFreq[[colNames[i]]]$categories
			col$palet <- pals[[paletNr]]
			paletNr <- ifelse(paletNr==length(pals), 1, paletNr + 1)
		}
		tab$columns[[i]] <- col
	}
	class(tab) <- "tabplot"
	return(tab)
}
     