coorNumCol <- function(tabCol, limitsX, bias_brokenX, compare=FALSE) {
	if (compare) {
		results <- coorNumVector(tabCol$mean.diff.scaled, limitsX, bias_brokenX, tabCol$name, tabCol$sd.diff.scaled)
		names(results) <- c("brokenX", "mean.diff.coor", "marks.labels", "marks.x", "xline", "widths", "x1", "x2")
		results.rel <- coorNumVector(tabCol$mean.diff.rel.scaled, limitsX, bias_brokenX, tabCol$name, tabCol$sd.diff.rel.scaled)
		names(results.rel) <- c("brokenX.rel", "mean.diff.coor.rel", "marks.labels.rel", "marks.x.rel", "xline.rel", "widths.rel", "x1.rel", "x2.rel")
		tabCol <- c(tabCol, results, results.rel)
	} else {
		results <- coorNumVector(tabCol$mean.scaled, limitsX, bias_brokenX, tabCol$name, tabCol$sd.scaled)
		names(results) <- c("brokenX", "mean.coor", "marks.labels", "marks.x", "xline", "widths", "x1", "x2")
		tabCol <- c(tabCol, results)
	}
	
	
	tabCol
}

coorNumVector <- function(num, limitsX, bias_brokenX, name, sd=NULL) {
	ignoreMarks <- all(is.na(num))
	
	if (!is.null(sd)) {
		numL <- num - sd
		numR <- num + sd
	} else {
		numL <- num
		numR <- num
	}
	
	## determine whether broken X-axis are applied, and transform values accordingly
	
	if (is.numeric(limitsX)) {
		minmax <- limitsX
		if (!ignoreMarks && min(numL, na.rm=TRUE)<limitsX[1]) {
			warning(paste("some mean values in", name, "fall outside the given limits and are therefore truncated"))
			num[num<limitsX[1]] <- limitsX[1]
			numL[numL<limitsX[1]] <- limitsX[1]
			numR[numR<limitsX[1]] <- limitsX[1]
		}
		if (!ignoreMarks && max(numR, na.rm=TRUE)>limitsX[2]) {
			warning(paste("some mean values in", name, "fall outside the given limits and are therefore truncated"))
			num[num>limitsX[2]] <- limitsX[2]
			numL[numL>limitsX[2]] <- limitsX[2]
			numR[numR>limitsX[2]] <- limitsX[2]
		}
		bias_brokenX <- 0
		ignoreMarks <- FALSE
	} else if (ignoreMarks) {
		minmax <- c(0, 1)
	} else {
		minmax <-  range(c(numL, numR), na.rm=TRUE)	
	}
	isConstant <- minmax[1]==minmax[2]
	if (!isConstant && minmax[2] > 0 && minmax[1] > (bias_brokenX * minmax[2])) {
		## broken x-axis has positive values
		brokenX <- 1
		values <- num - minmax[1]
		valuesL <- numL - minmax[1]
		valuesR <- numR - minmax[1]
		marks <- unique(c(0, pretty(minmax, 3)))
		marks_coor <- marks - minmax[1]
		minV <- 0
		maxV <- minmax[2] - minmax[1]
	} else if (!isConstant && minmax[1] < 0 && minmax[2] < (bias_brokenX * minmax[1])) {
		## broken x-axis has negative values
		brokenX <- -1
		values <- num - minmax[2]
		valuesL <- numL - minmax[2]
		valuesR <- numR - minmax[2]
		marks <- unique(c(pretty(minmax, 3), 0))
		marks_coor <- marks - minmax[2]
		minV <- minmax[1] - minmax[2]
		maxV <- 0
	} else {
		## x-axis not broken
		brokenX <- 0
		values <- num
		valuesL <- numL
		valuesR <- numR
		marks <- pretty(c(0, minmax), 4)
		marks_coor <- marks
		minV <- minmax[1]
		maxV <- minmax[2]
	}
	
	## scale values to 0-1, and determine 0-1 value of the y-axis
	if (minV < 0 && maxV > 0) {
		xline <- -minV / (maxV - minV)
		widths <- (values) / (maxV - minV)
		x1 <- (valuesL - minV) / (maxV - minV)
		x2 <- (valuesR - minV) / (maxV - minV)
		marks_x <- (marks_coor - minV) / (maxV - minV)
	} else if (brokenX==1) {
		xline <- 0
		widths <- 0.3 + (values) * 0.7 / (maxV - minV)
		x1 <- 0.3 + (valuesL) * 0.7 / (maxV - minV)
		x2 <- 0.3 + (valuesR) * 0.7 / (maxV - minV)
		marks_x <- 0.3 + (marks_coor) * 0.7 / (maxV - minV)
	} else if (brokenX==-1) {
		xline <- 1
		widths <- -0.3 + (values) * 0.7 / (maxV - minV)
		x1 <- 0.7 + (valuesL) * 0.7 / (maxV - minV)
		x2 <- 0.7 + (valuesR) * 0.7 / (maxV - minV)
		marks_x <- 0.7 + (marks_coor) * 0.7 / (maxV - minV)
	} else {
		xline <- ifelse(maxV > 0, 0, 1)
		widths <- (values) / max(abs(minV), abs(maxV))
		x1 <- (valuesL) / max(abs(minV), abs(maxV)) + xline
		x2 <- (valuesR) / max(abs(minV), abs(maxV)) + xline
		marks_x <- (marks_coor) / max(abs(minV), abs(maxV)) + xline
	}
	widths[is.nan(widths)] <- minV
	x1[is.nan(x1)] <- minV
	x2[is.nan(x2)] <- minV
	
	if (ignoreMarks) {
		marks.labels <- integer(0)
		marks.x <- integer(0)
	} else {
		marksVis <- marks_x >= ifelse(brokenX==1, 0.15, 0) &
					marks_x <= ifelse(brokenX==-1, 0.85, 1)
		
		marks_x[marks==0] <- xline
		marksVis[marks==0] <- TRUE
		marks.labels <- marks[marksVis]
		marks.x <- marks_x[marksVis]
	}

	list(brokenX, values, marks.labels, marks.x, xline,	widths, x1, x2)
}
