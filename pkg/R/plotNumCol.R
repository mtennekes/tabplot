plotNumCol <- function(tCol, tab, vpTitle, vpGraph, vpLegend, showNumAxes, relative=FALSE){
	## checks if device is Cario {cairoDevice}
	drawContours <- TRUE

	if (relative) {
		tCol[c("brokenX", "mean.diff.coor", "marks.labels", "marks.x", "xline", "widths")] <-
			tCol[c("brokenX.rel", "mean.diff.coor.rel", "marks.labels.rel", "marks.x.rel", "xline.rel", "widths.rel")]
	}
	
	lgrey <- "#F0F0F0"	#brewer.pal(9,"Greys")[2]
	#mgrey <- "#D0D0D0"
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
				 , width = 0
				 , height = tab$rows$heights
				 , just=c("left","bottom")
				 , gp = gpar(col=colors[length(colors)], fill = NA)
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
		if (showNumAxes && length(tCol$marks.x)) 
			grid.rect(x=tCol$marks.x,
					  width=0,
					  gp=gpar(col="black", alpha=0.3))
			
		
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
	if (showNumAxes && length(tCol$marks.x)) cellplot(3,1,vpLegend, {
		brokenX <- tCol$brokenX
		
		marks <- tCol$marks.labels
		marks.x <- tCol$marks.x
		marksLab <- markLabels(marks, brokenX=brokenX)
		markLabels <- marksLab$markLabels
		stepLabel <- marksLab$stepLabel
		interceptLabel <- marksLab$interceptLabel
		showLabels <- marksLab$showLabels
		
		grid.polyline(x=rep(marks.x,each=2),
					  y=rep(c(0.98,1),length(marks.x)),
					  id=rep(1:length(marks.x),each=2))
		
		
		## plot broken x-axis
		if (brokenX != 0) {
			blX <- ifelse(brokenX==1, 0.15, 0.85)
			blW <- 0.05
			grid.polyline(x= blX + c(-.5 * blW + c(-0.01, 0.01), .5 * blW + c(-0.01, 0.01)), 
						  y = rep(c(0.97, 1.03), 2), 
						  id = rep(1:2,each=2), gp=gpar(lwd=1))
			grid.polyline(x=c(0, ifelse(brokenX==1, 0.125, 0.825),
							  ifelse(brokenX==1, 0.175, 0.875), 1), 
						  y=c(1,1,1,1),
						  id=c(1,1,2,2))
		} else {
			grid.lines(y=c(1, 1))
		}

		grid.text(markLabels[showLabels],x=marks.x[showLabels], y=0.93, 
				  just="center",
				  gp=gpar(cex=0.8))
		
		
		
		add <- paste(stepLabel, interceptLabel, 
					 sep=ifelse(stepLabel=="" || 
					 		   	interceptLabel=="","", "\n"))
		
		grid.text(add,x=0.5, y=0.83, 
				  just=c("center", "top"),
				  gp=gpar(cex=0.8))
		
	})
}
