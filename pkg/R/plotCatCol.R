plotCatCol <- function(tCol, tab, colorpalet, vpTitle, vpGraph, vpLegend){
	drawContours <- TRUE
	
	cellplot(2,1,vpGraph, {

		## determine color indices for categories
		colorID <- rep(2:length(colorpalet), length.out=length(tCol$categories))
		if (tail(tCol$categories, 1)=="missing") {
			colorID[length(colorID)] <- 1
		}
		
		## create large vector of colors (one color for each bin*category
		colorset <- colorpalet[rep(colorID, each=tab$nBins)]
		
		if (drawContours) {
			cols <- colorset
		} else {
			cols <- NA
		}

		## draw bins
		grid.rect( x = tCol$x, y = tab$rows$y
				 , width = tCol$widths, height = tab$rows$heights
				 , just=c("left","bottom")
				 , gp = gpar(col=cols, fill = colorset, linejoin="mitre"))
	})


	## draw layout
	cellplot(3,1, vpLegend, {

		anyNA <- tail(tCol$categories, 1)=="missing"
		nCategories <- length(tCol$categories) - anyNA
		nLegendRows <- nCategories + 2 * anyNA
		
		Layout2 <- grid.layout(nrow = nLegendRows, ncol = 1)

		cex <- min(1, 1 / (convertHeight(unit(1,"lines"), "npc", valueOnly=TRUE) * nLegendRows))

		pushViewport(viewport(name="legendblocks", layout = Layout2, gp=gpar(cex=cex)))
		#print(current.vpPath())
		grid.rect(gp=gpar(col=NA, fill="white"))
		
		for (j in 1:nCategories) {
			cellplot(j,1, NULL, {
				grid.rect( x = 0, y = 0.5, width = 0.2, height = 1
						 , just=c("left")
						 , gp = gpar(col=colorpalet[colorID][j], fill = colorpalet[colorID][j])
						 )
				grid.text( tCol$categories[j]
						 , x = 0.25
						 , just="left")
			})
		}
		if (anyNA) {
			cellplot(nLegendRows,1, NULL, {
				grid.rect( x = 0, y = 0.5, width = 0.2, height = 1
						 , just=c("left")
						 , gp = gpar(col=colorpalet[colorID][nCategories + 1], fill = colorpalet[colorID][nCategories + 1])
						 )
				grid.text( tCol$categories[nCategories + 1]
						 , x = 0.25
						 , just="left")
			})
		}
		
		popViewport(n = 1)
	})
}