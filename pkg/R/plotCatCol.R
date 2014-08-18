plotCatCol <- function(tCol, tab, vpTitle, vpGraph, vpLegend, max_print_levels, text_NA, legend.lines, compare){
	midspace <- .05
	drawContours <- TRUE

	anyNA <- tail(tCol$categories, 1)=="missing"
	
	categories <- tCol$categories
	if (anyNA) categories <- categories[-length(categories)]
	
	
	nCategories <- length(categories)
	spread <- (nCategories > max_print_levels)
	
	## determine color indices for categories
	palet <- if (tCol$palet_recycled) {
		rep(tCol$palet, length.out = nCategories)
	} else {
		colorRampPalette(tCol$palet)(nCategories)
	}
	
	if (anyNA) {
		palet[nCategories+1] <- tCol$colorNA
	}
	
	if (compare) {
		marks.x <- seq(0, 1, length.out=5)
	}
	mgrey <- "#D8D8D8"
	
	cellplot(2,1,vpGraph, {
		if (compare) grid.rect(gp = gpar(col=mgrey,fill = mgrey))
		
		
		## create large vector of colors (one color for each bin*category
		colorset <- rep(palet, each=tab$nBins)
	
		missings <- which(tCol$widths==0)
		
		if (drawContours) {
			cols <- colorset
			cols[missings] <- NA
		} else {
			cols <- NA
		}

		## draw bins
		grid.rect( x = tCol$x, y = tab$rows$y
				 , width = tCol$widths, height = tab$rows$heights
				 , just=c("left","bottom")
				 , gp = gpar(col=cols, fill = colorset, linejoin="mitre", lwd=0))
		
		## draw white rect at the right to correct for rounding errors during plotting
# 		grid.rect(x = 1,  y=-.005, width=0.1, height=1.01, just=c("left", "bottom"), 
# 				  gp=gpar(col=NA, fill="white"))
		if (compare) grid.rect(width=midspace, gp = gpar(col="white", fill = "white"))
		
		
		
	})


	## draw legend
	cellplot(3,1, vpLegend, {
		nLegendSpread <- min(((legend.lines-1) %/% 2) + 1, max_print_levels, nCategories)
		nLegendSpreadRows <- nLegendSpread * 2 -1
		
		nLegendRows <- ifelse(spread, nLegendSpreadRows, nCategories) + 2 * anyNA
		
		Layout2 <- grid.layout(nrow = nLegendRows, ncol = 1 + spread, 
							   widths=if(spread) c(0.25, 0.75) else {1})
		
	
		cex <- min(1, 1 / (convertHeight(unit(1,"lines"), "npc", valueOnly=TRUE)
						   * nLegendRows))

		pushViewport(viewport(name="legendblocks", layout = Layout2, gp=gpar(cex=cex)))
		#print(current.vpPath())
		grid.rect(gp=gpar(col=NA, fill="white"))
		
		if (spread) {
			if (tCol$rev_legend) {
				palet <- rev(palet)
			}
			cellplot(1:nLegendSpreadRows,1, NULL, {
				grid.rect( x = 0, y = seq(1, 0, length.out=nCategories+1)[-(nCategories+1)]
						   , width = 0.8, height = 1/nCategories
						   , just=c("left", "top")
						   , gp = gpar(col=palet, fill = palet)
				)
			})
			labels <- rep("...", nLegendSpreadRows)
			labels[seq(1, nLegendSpreadRows, by=2)] <- tCol$categories[seq(1, nCategories - anyNA, length.out=nLegendSpread)]
				
			for (j in 1:nLegendSpreadRows) {
				k <- ifelse(tCol$rev_legend, (nLegendSpreadRows+1)-j, j)
				cellplot(j,2, NULL, {
					grid.text( labels[k]
							   , x = 0
							   , just="left")
				})
			}
			if (anyNA) {
				cellplot(nLegendRows, 1, NULL, {
					grid.rect( x = 0, y = 0.5, width = 0.8, height = 1
							   , just=c("left")
							   , gp = gpar(col=palet[nCategories + 1], 
							   			fill = palet[nCategories + 1])
					)
				})
				cellplot(nLegendRows, 2, NULL, {
					grid.text( text_NA
							   , x = 0
							   , just="left")
				})
			}
			
		} else {
			for (j in 1:nCategories) {
				k <- ifelse(tCol$rev_legend, (nCategories + 1) - j, j)
				cellplot(j,1, NULL, {
					grid.rect( x = 0, y = 0.5, width = 0.2, height = 1
							   , just=c("left")
							   , gp = gpar(col=palet[k], fill = palet[k])
					)
					grid.text( categories[k]
							   , x = 0.25
							   , just="left")
				})
			}
			if (anyNA) {
				cellplot(nLegendRows, 1, NULL, {
					grid.rect( x = 0, y = 0.5, width = 0.2, height = 1
							   , just=c("left")
							   , gp = gpar(col=palet[nCategories + 1], 
							   			fill = palet[nCategories + 1])
					)
					grid.text( text_NA
							   , x = 0.25
							   , just="left")
				})
			}
		}
		
		
		popViewport(1)
	})
}