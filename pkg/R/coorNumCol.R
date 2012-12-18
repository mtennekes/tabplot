coorNumCol <- function(tabCol, limitsX, bias_brokenX) {
	
	num <- tabCol$mean.scaled
	ignoreMarks <- all(is.na(num))

	## determine whether broken X-axis are applied, and transform values accordingly
	
	if (is.numeric(limitsX)) {
		minmax <- limitsX
		if (!ignoreMarks && min(num, na.rm=TRUE)<limitsX[1]) {
			warning(paste("some mean values in", tabCol$name, "fall outside the given limits and are therefore truncated"))
			num[num<limitsX[1]] <- limitsX[1]
		}
		if (!ignoreMarks && max(num, na.rm=TRUE)>limitsX[2]) {
			warning(paste("some mean values in", tabCol$name, "fall outside the given limits and are therefore truncated"))
			num[num>limitsX[2]] <- limitsX[2]
		}
		bias_brokenX <- 0
		ignoreMarks <- FALSE
	} else if (ignoreMarks) {
		minmax <- c(0, 1)
	} else {
		minmax <-  range(num, na.rm=TRUE)	
	}
	
	if ((minmax[2]) > 0 && minmax[1] > (bias_brokenX * minmax[2])) {
		## broken x-axis has positive values
		brokenX <- 1
		values <- num - minmax[1]
		marks <- unique(c(0, pretty(minmax, 3)))
		marks_coor <- marks - minmax[1]
		minV <- 0
		maxV <- minmax[2] - minmax[1]
	} else if ((minmax[1]) < 0 && minmax[2] < (bias_brokenX * minmax[1])) {
		## broken x-axis has negative values
		brokenX <- -1
		values <- num - minmax[2]
		marks <- unique(c(pretty(minmax, 3), 0))
		marks_coor <- marks - minmax[2]
		minV <- minmax[1] - minmax[2]
		maxV <- 0
	} else {
		## x-axis not broken
		brokenX <- 0
		values <- num
		marks <- pretty(c(0, minmax), 4)
		marks_coor <- marks
		minV <- minmax[1]
		maxV <- minmax[2]
	}
	
	## scale values to 0-1, and determine 0-1 value of the y-axis
	if (minV < 0 && maxV > 0) {
		xline <- -minV / (maxV - minV)
		widths <- (values) / (maxV - minV)
		marks_x <- (marks_coor - minV) / (maxV - minV)
	} else if (brokenX==1) {
		xline <- 0
		widths <- 0.3 + (values) * 0.7 / (maxV - minV)
		marks_x <- 0.3 + (marks_coor) * 0.7 / (maxV - minV)
	} else if (brokenX==-1) {
		xline <- 1
		widths <- -0.3 + (values) * 0.7 / (maxV - minV)
		marks_x <- 0.7 + (marks_coor) * 0.7 / (maxV - minV)
	} else {
		xline <- ifelse(maxV > 0, 0, 1)
		widths <- (values) / max(abs(minV), abs(maxV))
		marks_x <- (marks_coor) / max(abs(minV), abs(maxV))
	}
	widths[is.nan(widths)] <- minV

	tabCol$brokenX <- brokenX
	tabCol$mean.coor <- values
	
	if (ignoreMarks) {
		tabCol$marks.labels <- integer(0)
		tabCol$marks.x <- integer(0)
	} else {
		marksVis <- marks_x >= ifelse(brokenX==1, 0.15, 0) &
					marks_x <= ifelse(brokenX==-1, 0.85, 1)
		
		marks_x[marks==0] <- xline
		marksVis[marks==0] <- TRUE
		tabCol$marks.labels <- marks[marksVis]
		tabCol$marks.x <- marks_x[marksVis]
	}
	
	tabCol$xline <- xline
	tabCol$widths <- widths
	
	tabCol
}
