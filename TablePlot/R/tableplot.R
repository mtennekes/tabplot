tableplot <-
function(dat, colNames=names(dat), sortCol=1,  decreasing=FALSE, scales="auto", nBins=100, from=0,to=100) {




	###########################
	##### Function to determine logarithmic scale
	###########################
	getLog <- function(x) {
		if (x>=0) {
			return(log10(x+1))
		} else {
			return(-log10(abs(x)+1))
		}
	}

	## todo: logging
	beginTime <- proc.time()[3]
	
	#############################
	## Checks
	#############################
	
	## Check dat
	if (!exists("dat")) stop("Data.frame <dat> not defined")
	if (!inherits(dat,"data.frame") && class(dat)!="ffdf") stop("<dat> is not a data.frame")
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
	## Make row selection of the data
	#############################
	
	# make rowselection of the data
	iFrom <- round(nrow(dat) * from * 0.01) + 1 
	iTo <- round(nrow(dat) * to * 0.01)
	m <- (iTo - iFrom) + 1

	# does selection consist of at least two rows?
	if (m < 2) stop("Selection does not consist of at least two rows")

	# check if nBins is at least number of items
	if (nBins > m) nBins <- m

	#############################
	## Calculate bin sizes
	#############################
	
	normalBinSize <- m %/% nBins
	binSizes <- rep(normalBinSize, nBins)
	if ( (rest <- m %% nBins) > 0) {
		binSizes[1:rest] <- binSizes[1:rest] + 1
	}

	#############################
	## Determine column classes
	#############################
	if (class(dat)=="ffdf") {
		datClasses <- character(n)
		for (i in 1:n) {
			datClasses[i] <- ifelse(is.null(ramclass(dat[[i]])), "numeric", ramclass(dat[[i]])[1])
		}
	} else {
		datClasses <- sapply(dat,FUN=function(x)class(x)[1])
	}

	## find numerical variables
	isNumber <- (datClasses %in% c("numeric","integer"))

	## cast logical columns to factors
	for (i in which(datClasses == "logical")) {
		if (class(dat)=="ffdf") {
			levels(dat[[i]]) <- c("FALSE", "TRUE")
		} else {
			dat[,i] <- factor(dat[,i], levels=c("TRUE", "FALSE"))
		}
	}
	
	## cast non-factor columns that are non-numeric either to factor
	if (class(dat)!="ffdf") {
		isFactor <- (datClasses == "factor")
		castToFactor <- !isFactor & !isNumber
		
		for (i in colNames[castToFactor]) {
			dat[,i] <- as.factor(dat[,i])
		}
	}

	#############################
	## Determine bin indices (needed for aggregation
	#############################
	
	if (class(dat)=="ffdf") {
		# TODO
	} else {
		rand <- sample.int(nrow(dat))
		
		datList <- as.list(dat[sortCol])
		for (i in which(decreasing)) {
			if (isNumber[sortCol[i]]) {
				datList[[i]] <- -datList[[i]]
			} else {
				datList[[i]] <- -as.integer(datList[[i]])
			}
		}
		o <- order(do.call(order, as.list(c(datList, list(rand=rand)))))
		
		brks <- c(0, cumsum(binSizes)) + (iFrom-1)
		
		# TODO in stead of precalculating the brks, we can also give the number of breaks and calculate binSizes from summing the bins.
		dat$aggIndex <- cut(o, brks, right=TRUE, labels=FALSE)
	}
		
	#############################
	## Determine scales (in case they are set to "auto")
	#############################
		
	if (class(dat)=="ffdf") {
		## TODO
		scales[scales=="auto"] <- "lin"
	} else {
		for (i in which(isNumber)) {
			if (scales[i]=="auto") {
				quant <- quantile(dat[,i], na.rm=TRUE)
				## Simple test to determine whether scale is lin or log
				if (scales[i]=="auto") {
					if (((quant[4] >= 0) && (quant[5] > (quant[4] * 100))) || ((quant[2] < 0) && (quant[1] < (quant[2] * 100)))) {
						scales[i] <- "log" 
					} else {
						scales[i] <- "lin" 
					}
				}
			}
		}
	}

	#############################
	## Create list object that contains all data needed to plot
	#############################

	tab <- list()
	tab$n <- n
	tab$nBins <- nBins
	
	## tab$row contains info about bins/y-axis
	tab$rows <- list( heights = binSizes/m 
	                , y = c(0,cumsum(binSizes/m)[-nBins])
	                , m = m
	                , from = from
	                , to = to
	                , marks = pretty(c(from, to), 10)
	                )
	
	## create column list
	tab$columns <- list()

	## fill with general information
	for (i in 1:n) {
		## sorting method
		sortc <- ifelse(i %in% sortCol, ifelse(decreasing[which(i==sortCol)], "decreasing", "increasing"), "")
		tab$columns[[colNames[i]]] <- list(name=colNames[i], isnumeric=isNumber[i], sort=sortc)
	}
	
	
cat("time after preparation data", proc.time()[3] - beginTime, "\n")

	#############################
	## Aggregation process
	#############################

	if (class(dat)=="ffdf") {
		## TODO
	} else {
		
		
		#### aggregate numeric variables
		if (sum(isNumber)>0) {
			## parameter between 0 en 1 that determines when x-axis is broken (see below)
			bias_brokenX <- 0.8
			
			## calculate means
			datMean <- ddply(dat, .(aggIndex), numcolwise(mean), na.rm=TRUE)
			datMean <- datMean[-(nBins+1),-ncol(datMean), drop=FALSE]
			
			## calculate completion percentages
			datCompl <- ddply(dat, .(aggIndex), numcolwise(function(x){sum(is.na(x))}))
			datCompl <- datCompl[-(nBins+1),-ncol(datCompl), drop=FALSE]
			datCompl <- as.data.frame(apply(datCompl, 2, function(x,y){100-floor(x/y*100)}, binSizes))

			## set means of bins with all missings to 0
			datMean[datCompl==0] <- 0
			
			## for each numeric column do the following:
			for (i in which(isNumber)) {
			    numCol <- tab$columns[[i]]
				numCol$mean <- datMean[[numCol$name]]
				numCol$compl <- datCompl[[numCol$name]]
				numCol$scale <- scales[i]
				
				## determine whether x-axis is broken, and adjust values in that case
				minmax <- range(numCol$mean, na.rm=TRUE)
				brokenX <- 0
				if (scales[i]=="log") {
					values <- sapply(numCol$mean, FUN = getLog)
				} else {
					if ((minmax[2]) > 0 && minmax[1] > (bias_brokenX * minmax[2])) {
						## broken x-axis has positive values
						brokenX <- 1
						values <- numCol$mean - minmax[1]
					} else if ((minmax[1]) < 0 && minmax[2] < (bias_brokenX * minmax[1])) {
						## broken x-axis has negative values
						brokenX <- -1
						values <- numCol$mean - minmax[2]
					} else {
						## x-axis not broken
						values <- numCol$mean
					}
				}
				
				## scale values to 0-1, and determine 0-1 value of the y-axis
				minV <- min(values, na.rm=TRUE)
				maxV <- max(values, na.rm=TRUE)
				if (minV < 0 && maxV > 0) {
					xline <- -minV / (maxV - minV)
					widths <- (values) / (maxV - minV)
				} else if (brokenX==1) {
					xline <- 0
					widths <- 0.3 + (values) * 0.7 / (maxV - minV)
				} else if (brokenX==-1) {
					xline <- 1
					widths <- -0.3 + (values) * 0.7 / (maxV - minV)
				} else {
					xline <- ifelse(maxV > 0, 0, 1)
					widths <- (values) / max(abs(minV), abs(maxV))
				}
				
				## assign to tab object
				numCol$xline <- xline
				numCol$brokenX <- brokenX
				numCol$widths <- widths
				
				tab$columns[[i]] <- numCol
			}
		}
		
		#### aggregate categorical variables
		for (i in which(!isNumber)) {
            catCol <- tab$columns[[i]]
			## determine categories and frequencies
			catCol$categories <- levels(dat[[i]])
			catCol$widths <- table( dat[,"aggIndex"]
			                      , dat[[i]]
								  , useNA = "ifany"
								  )[1:nBins,]
								  
			if (ncol(catCol$widths) > length(catCol$categories)) {
				catCol$categories <- c(catCol$categories, "missing")
			}
			catCol$widths <- catCol$widths / rep(binSizes, length(catCol$categories))
			
			catCol$x <- cbind(0,(t(apply(catCol$widths, 1, cumsum)))[, -length(catCol$categories)])
			tab$columns[[i]] <- catCol
		}
	}

cat("time after data aggregation", proc.time()[3] - beginTime, "\n")

	## plot
	.plotTable(tab)
	
cat("total time", proc.time()[3] - beginTime, "\n")
}

