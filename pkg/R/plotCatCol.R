plotCatCol <- function(tCol, tab, vpTitle, vpGraph, vpLegend, max_print_levels, text_NA){
	drawContours <- TRUE

	anyNA <- tail(tCol$categories, 1)=="missing"
	nCategories <- ncol(tCol$freq) - anyNA
	nCategoriesLabels <- length(tCol$categories) - anyNA
	spread <- (nCategories > max_print_levels)
	
	## determine color indices for categories
	palet <- if (tCol$palettype=="recycled") {
		rep(tCol$palet, length.out = nCategories)
	} else {
		colorRampPalette(tCol$palet)(nCategories)
	}
	
	if (anyNA) {
		palet[nCategories+1] <- tCol$colorNA
	}

	cellplot(2,1,vpGraph, {

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
		grid.rect(x = 1,  y=-.005, width=0.1, height=1.01, just=c("left", "bottom"), 
				  gp=gpar(col=NA, fill="white"))
	})


	## draw legend
	cellplot(3,1, vpLegend, {
		nLegendRows <- ifelse(spread, 7, nCategories) + 2 * anyNA
		
		Layout2 <- grid.layout(nrow = nLegendRows, ncol = 1 + spread, 
							   widths=if(spread) c(0.25, 0.75) else {1})
	
		cex <- min(1, 1 / (convertHeight(unit(1,"lines"), "npc", valueOnly=TRUE)
						   * nLegendRows))

		pushViewport(viewport(name="legendblocks", layout = Layout2, gp=gpar(cex=cex)))
		#print(current.vpPath())
		grid.rect(gp=gpar(col=NA, fill="white"))
		
		if (spread) {
			cellplot(1:7,1, NULL, {
				grid.rect( x = 0, y = seq(1, 0, length.out=nCategories+1)[-(nCategories+1)]
						   , width = 0.8, height = 1/nCategories
						   , just=c("left", "top")
						   , gp = gpar(col=palet, fill = palet)
				)
			})
			labels <- rep("...", 7)
			labels[c(1,3,5,7)] <- tCol$categories[c(1, 
										round(nCategoriesLabels/3), 
										round(nCategoriesLabels/3*2),
										nCategoriesLabels)]
			
			for (j in 1:7) {
				cellplot(j,2, NULL, {
					grid.text( labels[j]
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
				cellplot(j,1, NULL, {
					grid.rect( x = 0, y = 0.5, width = 0.2, height = 1
							   , just=c("left")
							   , gp = gpar(col=palet[j], fill = palet[j])
					)
					grid.text( tCol$categories[j]
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
		
		
		popViewport(n = 1)
	})
}