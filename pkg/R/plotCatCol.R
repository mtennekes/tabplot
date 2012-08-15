plotCatCol <- function(tCol, tab, vpTitle, vpGraph, vpLegend, max_print_levels){
	drawContours <- TRUE
	anyNA <- tail(tCol$categories, 1)=="missing"
	
	cellplot(2,1,vpGraph, {

		## determine color indices for categories
		
		nCategories <- ncol(tCol$freq)
		
		if (tCol$palettype=="recycled") {
			palet <- rep(tCol$palet, length.out = nCategories)
			if (anyNA) {
				palet[nCategories] <- tCol$colorNA
			}
		} else {
			if (anyNA) {
				palet <- c(colorRampPalette(tCol$palet)(nCategories-1), 
						   tCol$colorNA)
			} else {
				palet <- colorRampPalette(tCol$palet)(nCategories)
			}
		}
			
			
		
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
		grid.rect(x = 1,  y=-.005, width=0.1, height=1.01, just=c("left", "bottom"), gp=gpar(col=NA, fill="white"))
	})


	## draw legend
	cellplot(3,1, vpLegend, {

		nCategoriesLabels <- length(tCol$categories) - anyNA

		if (nCategories <= max_print_levels) {
			nLegendRows <- nCategories + 2 * anyNA 
		} else {
			nLegendRows <- 7 + 2 * anyNA
		}
		
		Layout2 <- grid.layout(nrow = nLegendRows, ncol = 1 + (nCategories > max_print_levels), 
							   widths=if(nCategories > max_print_levels)c(0.25, 0.75)else{1})
	
		cex <- min(1, 1 / (convertHeight(unit(1,"lines"), "npc", valueOnly=TRUE) * nLegendRows))

		pushViewport(viewport(name="legendblocks", layout = Layout2, gp=gpar(cex=cex)))
		#print(current.vpPath())
		grid.rect(gp=gpar(col=NA, fill="white"))
		
		if (nCategories <= max_print_levels) {
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
		} else {
			cellplot(1:7,1, NULL, {
				grid.rect( x = 0, y = seq(1, 0, length.out=nCategories+1)[-(nCategories+1)]
						   , width = 0.8, height = 1/nCategories
						   , just=c("left", "top")
						   , gp = gpar(col=palet, fill = palet)
				)
			})
			labels <- c(tCol$categories[1], "...", 
						tCol$categories[round(nCategoriesLabels/3)], "...",
						tCol$categories[round(nCategoriesLabels/3*2)], "...",
						tCol$categories[nCategoriesLabels])
			
			
			for (j in 1:7) {
				cellplot(j,2, NULL, {
					grid.text( labels[j]
							   , x = 0
							   , just="left")
				})
			}
		}
		
		if (anyNA) {
			cellplot(nLegendRows,1, NULL, {
				grid.rect( x = 0, y = 0.5, width = 0.2, height = 1
						 , just=c("left")
						 , gp = gpar(col=palet[nCategories + 1], fill = palet[nCategories + 1])
						 )
				grid.text( tCol$categories[nCategories + 1]
						 , x = 0.25
						 , just="left")
			})
		}
		
		popViewport(n = 1)
	})
}