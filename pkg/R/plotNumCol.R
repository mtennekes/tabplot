plotNumCol <- function(tCol, tab, vpTitle, vpGraph, vpLegend, showNumAxes){
	## checks if device is Cario {cairoDevice}
	drawContours <- TRUE

	lgrey <- "#F0F0F0"	#brewer.pal(9,"Greys")[2]
	mgrey <- "#D0D0D0"
	lred <- "#FEE0D2"	#brewer.pal(9,"Reds")[2]

	colors <- c(NA, colorRampPalette(tabplotPalettes$seq[[tCol$paletname]],space="rgb")(100))

	
	cellplot(2,1,vpGraph, {		
		grid.rect(gp = gpar(col=NA,fill = lgrey))
		
		## bins with all missings
		missings <- which(tCol$compl==0)

		## when cairoDevice is enabled, not only fill the bins with colors, but also color the contours
		if (drawContours) {
			cols <- colors[tCol$compl + 1]
		} else {
			cols <- NA
		}
		
		
		
		## plot bins
		grid.rect( x = rep(tCol$xline,tab$nBins)
				 , y = tab$rows$y
				 , width = tCol$widths
				 , height = tab$rows$heights
				 , just=c("left","bottom")
				 , gp = gpar(col=cols, fill = colors[tCol$compl + 1], linejoin="mitre", lwd=0.01)
				 )
		
		## plot small lines at the righthand side of the bins
		grid.rect( x = rep(tCol$xline,tab$nBins)+tCol$widths
				 , y = tab$rows$y
				 , width = unit(0.5, "points")
				 , height = tab$rows$heights
				 , just=c("left","bottom")
				 , gp = gpar(col=colors[length(colors)], fill = colors[length(colors)], lwd=0.01)
				 )

		
		if (drawContours) {
			cols <- lred
		} else {
			cols <- NA
		}
		 
		## plot bins with all missings as light red
		if (length(missings>0)) {
			grid.rect( x = rep(0, length(missings))
					 , y = tab$rows$y[missings]
					 , width =  rep(1, length(missings))
					 , height = tab$rows$heights[missings]
					 , just=c("left","bottom")
					 , gp = gpar(fill = lred, col=cols, linejoin="mitre", lwd=0.01)
					 )
		}

		## plot grid lines
		if (showNumAxes) 
			grid.polyline(x=rep(tCol$marks.x,each=2),
					  y=rep(c(0,1),length(tCol$marks.x)),
					  id=rep(1:length(tCol$marks.x),each=2),
					  gp=gpar(col=mgrey))
		
		
		## plot broken x-axis
		if (tCol$brokenX != 0) {
			blX <- ifelse(tCol$brokenX==1, 0.15, 0.85)
			blW <- 0.05
			grid.rect(x=blX, width=blW, gp = gpar(col=NA,fill = lgrey))
			grid.polyline( x= blX + rep(c(-.5 * blW + c(-0.01, 0.01), .5 * blW + c(-0.01, 0.01)), 2)
						 , y = c(rep(c(-0.01, 0.01), 2), rep(c(0.99, 1.01), 2))
						 , id = rep(1:4,each=2), gp=gpar(col="white", lwd=3))
			grid.polyline(x= blX + rep(c(-.5 * blW + c(-0.01, 0.01), .5 * blW + c(-0.01, 0.01)), 2),
				y = c(rep(c(-0.01, 0.01), 2), rep(c(0.99, 1.01), 2)), 
				id = rep(1:4,each=2), gp=gpar(lwd=1))
		}
	})
	if (showNumAxes) cellplot(3,1,vpLegend, {
		
		grid.polyline(x=c(0,1,rep(tCol$marks.x,each=2)),
					  y=c(1,1,rep(c(0.98,1),length(tCol$marks.x))),
					  id=rep(1:(length(tCol$marks.x)+1),each=2))
		marks <- tCol$marks.labels
		marksLabels <- format(marks, trim=TRUE)
		marksLarge <- abs(marks)>=10000
		marksLabels[marksLarge] <- formatC(marks[marksLarge])
		
		grid.text(marksLabels,x=tCol$marks.x, y=0.93, 
				  just="center",
				  gp=gpar(cex=0.8))
		
	})
}
