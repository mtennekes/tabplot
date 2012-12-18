#' Transform a date-time vector to a factor
#'
#' Transform a date-time vector from class \code{\link{POSIXt}} or \code{\link{Date}} to a factor.
#'
#' The range \code{rng} is cut according to different pretty rounded time periods. The cut with the number of levels that is closest to 6 is chosen. Vector \code{p} is cut accordingly. Values of \code{p} outside \code{rng} are translated to \code{NA}.
#' @param p date-time vector
#' @param rng range of the factor. 
#' @return A factor vector. 
#' @example ../examples/datetime2fac.R
#' @export
#' @seealso \code{\link{num2fac}}
#' @note This function is still in development stage, and can be improved and optimized. \code{ff} vectors are not implemented yet
datetime2fac <- function(p, rng=range(p, na.rm=TRUE)) {

	if (!inherits(p, c("Date", "POSIXt"))) stop(paste(parse(substitute(p)), "is not a valid date/time vector"))

	cutSteps <- c(
		paste(c(50, 20, 10, 5, 4, 2, 1), "year"),
		"quarter",
		"month",
		"week",
		"day",
		paste(c(6, 3, 1), "hour"),
		paste(c(15, 5, 1), "min"),
		paste(c(10, 5, 1), "sec"))
	
	if ("Date" %in% class(p)) {
		cutSteps <- cutSteps[1:11]
	}
	
	lvls <- rep(100, length(cutSteps))
	i <- 1
	for (stp in cutSteps) {
		lvls[i] <- nlevels(cut(rng, breaks=stp))
		if (lvls[i] > 15) break;
		i <- i + 1
	}

	## get last element for which the minimum is closest to 6.4
	idealStpIndex <- length(cutSteps) - which.min(rev(abs(6.4 - lvls))) + 1
	
	if (idealStpIndex %in% c(12,13)) # in case x hours
		startDT <- trunc(rng[1], "days") else
	if (idealStpIndex %in% c(15,16)) # in case x mins
		startDT <- trunc(rng[1], "hours") else
	if (idealStpIndex %in% c(18,19)) # in case x secs
		startDT <- trunc(rng[1], "secs") else startDT <- rng[1]
	p[length(p)+1] <- startDT
	

	preDates <- which(p<rng[1])
	postDates <- which(p>rng[2])

	p[preDates] <- NA
	p[postDates ] <- NA

	p2 <- cut(p, breaks=cutSteps[idealStpIndex])
	p2 <- p2[1:(length(p2)-1)]

	if (idealStpIndex <= 9) levels(p2) <- substr(levels(p2), 1, 7) # shown only years and months
	if (idealStpIndex <= 7) {
		# shown year intervals
		levels(p2) <- substr(levels(p2), 1, 4)
		interval <- (as.numeric(levels(p2)[2]) - as.numeric(levels(p2)[1])) - 1
		levels(p2) <- paste(levels(p2), "-", as.numeric(levels(p2)) + interval, sep = "")
	}

	if (length(preDates)>0) {
		preLevel <- paste("<", levels(p2)[1])
		p2 <- factor(p2, levels=c(preLevel , head(levels(p2), -1)))
		p2[preDates] <- preLevel 
	}	

	if (length(postDates)>0) {
		postLevel <- paste(">", tail(levels(p2), 1))
		p2 <- factor(p2, levels=c(head(levels(p2), -1), postLevel))
		p2[preDates] <- postLevel
	}	

	return(p2)
}