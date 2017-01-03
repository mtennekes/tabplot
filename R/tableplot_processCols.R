tableplot_processCols <- function(tab, colNames1, colNames2, IQR_bias, bias_brokenX, limitsX, nBins, sortColName) {
	

	midspace <- .05
	colNames_string <- ifelse(is.na(colNames2), colNames1, paste(colNames1, colNames2, sep="-"))
	cols <- tab$columns
	tab$columns <- mapply(function(c1, c2, cname) {
		if (is.na(c2)) {
			col <- cols[[c1]]
			
			if (col$isnumeric) {
				col <- scaleNumCol(col, IQR_bias)
				col <- coorNumCol(col, limitsX = limitsX[col$name], bias_brokenX=bias_brokenX)
			} else {
				col <- coorCatCol(col, nBins)
			}
			col$type <- "normal"
			
			col
		} else {
			col1 <- cols[[c1]]
			col2 <- cols[[c2]]
			
			col <- col1
			if (col1$isnumeric) {
				col$mean1 <- col1$mean
				col$mean2 <- col2$mean
				col$mean.diff <- col1$mean - col2$mean
				col$mean.diff.rel <- ((col1$mean - col2$mean) / col1$mean)*100
				
				col$sd1 <- col1$sd
				col$sd2 <- col2$sd
				col$sd.diff <- sqrt(col1$sd^2 + col2$sd^2)
				col$sd.diff.rel <- col$sd.diff / col1$mean * 100
				
				
				col$scale_init <- "lin"
				col$compl <- pmin(col1$compl, col2$compl)
				col[c("mean", "sd", "scale_final", "mean.scaled", "brokenX", "mean.diff.coor", "marks.labels", "marks.x", "xline", "widths")] <- NULL
				
				col <- scaleNumCol(col, IQR_bias=5, compare=TRUE)
				col <- coorNumCol(col, limitsX=list(), bias_brokenX=0.8, compare=TRUE)
				
			} else {
				
				# 			col <- tp$columns[[4]]
				# 			col1 <- tp1$columns[[4]]
				# 			col2 <- tp2$columns[[4]]
				
				col$freq1 <- col1$freq
				col$freq2 <- col2$freq
				
				freq <- col$freq.diff <- col1$freq - col2$freq
				xinit <- apply(freq, MARGIN=1, function(x)sum(x[x<0]))
				
				ids <- t(apply(freq, MARGIN=1, orderRow))
				freq.sorted <- sortRows(freq, ids)
				
				widths <- abs(freq.sorted)
				x <- t(apply(widths, 1, cumsum)) + xinit
				x <- cbind(xinit, x[,1:(ncol(x)-1)])
				
				ids2 <- t(apply(ids, 1, order))
				
				col$x <- sortRows(x, ids2)
				
				col$widths <- sortRows(widths, ids2)
				
				col$x <- col$x * (1-midspace) / 2
				col$widths <- col$widths * (1-midspace) / 2
				
				
				col$x[col$x<0] <- col$x[col$x<0] - (midspace/2)
				col$x[col$x>=0] <- col$x[col$x>=0] + (midspace/2)
				
				col$x[col$widths==0] <- NA
				col$widths[col$widths==0] <- NA
				
				col$x <- (col$x) + 0.5
				
				col$freq <- NULL
			}
			col$type <- "compare"
			col$name <- cname
			col
		}
	}, colNames1, colNames2, colNames_string, SIMPLIFY=FALSE)
	tab$m <- length(colNames1)
	tab$select <- colNames_string
	tab$sortCol <- which(sortColName==colNames_string)[1]
	names(tab$columns) <- colNames_string
	tab
}
