# auxillary function
cellplot <- function(x,y, vp=NULL, e){
	name <- paste("(", deparse(substitute(x)),",",deparse(substitute(y)), ")", sep="")
	pushViewport(viewport( name=name, layout.pos.row=x, layout.pos.col=y))
	n <- 1
	if (!is.null(vp)){ 
		pushViewport(vp)
		n <- n + 1
	}
	e
	popViewport(n=n)
}

plotNumCol <- function(tCol, tab, blues, vpTitle, vpGraph, vpLegend){
	## checks if device is Cario {cairoDevice}
	isCairo <- (names(dev.cur())=="Cairo")

	lgrey <- brewer.pal(9,"Greys")[2]
	lred <- brewer.pal(9,"Reds")[2]
		
	cellplot(2,1,vpGraph, {		
		grid.rect(gp = gpar(col=NA,fill = lgrey))
		
		## bins with all missings
		missings <- which(tCol$compl==0)

		## when cairoDevice is enabled, not only fill the bins with colors, but also color the contours
		if (isCairo) {
			cols <- blues[tCol$compl + 1]
		} else {
			cols <- NA
		}
		
		## plot bins
		grid.rect( x = rep(tCol$xline,tab$nBins)
				 , y = tab$rows$y
				 , width = tCol$widths
				 , height = tab$rows$heights
				 , just=c("left","bottom")
				 , gp = gpar(col=cols, fill = blues[tCol$compl + 1], linejoin="mitre")
				 )
		
		## plot small lines at the righthand side of the bins
		grid.rect( x = rep(tCol$xline,tab$nBins)+tCol$widths
				 , y = tab$rows$y
				 , width = unit(0.75, "points")
				 , height = tab$rows$heights
				 , just=c("left","bottom")
				 , gp = gpar(col=NA, fill = blues[length(blues)])
				 )

		
		if (isCairo) {
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
					 , gp = gpar(fill = lred,col=cols, linejoin="mitre")
					 )
		}
	 
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
}

plotCatCol <- function(tCol, tab, colorpalet, vpTitle, vpGraph, vpLegend){
	isCairo <- (names(dev.cur())=="Cairo")
	
	cellplot(2,1,vpGraph, {

		## determine color indices for categories
		colorID <- rep(2:(length(colorpalet)+1), length.out=length(tCol$categories))
		if (tail(tCol$categories, 1)=="missing") {
			colorID[length(colorID)] <- 1
		}
		
		## create large vector of colors (one color for each bin*category
		colorset <- colorpalet[rep(colorID, each=tab$nBins)]
		
		if (isCairo) {
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
						 , gp = gpar(col=NA, fill = colorpalet[colorID][j])
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
						 , gp = gpar(col=NA, fill = colorpalet[colorID][nCategories + 1])
						 )
				grid.text( tCol$categories[nCategories + 1]
						 , x = 0.25
						 , just="left")
			})
		}
		
		popViewport(n = 1)
	})
}


plotTable <-
function(tab) {
	
		
	#############################
	## Determine colors and color scales
	#############################
	
	blues <- c(NA, colorRampPalette(brewer.pal(9,"Blues")[2:9],space="rgb")(100))
	# blues[1] is for missing values, blues[2]...blues[101] is for completion percentages of 1...100

	lgrey <- brewer.pal(9,"Greys")[2]
	lred <- brewer.pal(9,"Reds")[2]
	red <- brewer.pal(9,"Set1")[1]
	
	set1 <- brewer.pal(9,"Set1")[2:9]
	set2 <- brewer.pal(8,"Set2")[c(1:6,8,7)]
	
	color <- c(red, set1, set2)
	
	#############################
	## Set layout
	#############################
	
	## configure viewports
	marginT <- 0.02;	marginB <- 0.02
	marginL <- 0.05;	marginR <- 0.05
	marginLT <- 0.0;	marginLB <- 0.02
	
	vpColumn <- viewport( name="Column"
	                   ,  x = unit(marginL, "npc")
 					    , width = unit(1 - marginL - marginR, "npc")
	                    , layout = grid.layout( nrow=3
						                      , ncol=1
						                      , heights = unit(c(1,1, 6), c("lines","null", "lines"))
						    				  )
	                   , gp=gpar(cex=0.75)
					   )
						
	vpTitle <- viewport( name = "title"
					   , just = c("left", "top")
					   )
	  
	vpGraph <- viewport( name = "graph"
					   , y = unit(marginB, "npc")
					   , height = unit(1 - marginB - marginT,"npc")
					   , just = c("left", "bottom")
					   )
	  
	vpLegend <- viewport( name = "legend"
						, y = unit(marginLB, "npc")
						, height = unit(1 - marginLB - marginLT,"npc")
						, just = c("left", "bottom")
						)
	
	## set grid layout
	grid.newpage()
	
	Layout <- grid.layout( nrow = 1, ncol = tab$n+1
	                     , widths = unit(c(3,rep(1,tab$n)), c("lines",rep("null",tab$n)))
						 )

	pushViewport(viewport(layout = Layout))
	
	
	#############################
	## Configure y-axis
	#############################
	
	cellplot(1,1,vpColumn, {
		cellplot(2,1,vpGraph,{
			## y axes and bin ticks
			grid.polyline( x=c(0.80,0.80,rep(c(0.80,0.83),tab$nBins+1))
			             , y=c(0,1,rep(c(tab$rows$y,1),each=2))
						 , id=rep(1:(tab$nBins+2),each=2)
						 )

			## percentages ticks
			rests <- formatC(tab$rows$marks - floor(tab$rows$marks))	
			digits <- max(0,(max(sapply(rests, FUN=nchar))-2))
			marksChar <- paste(formatC(tab$rows$marks, format="f", digits=digits),"%", sep="")
			
			ticks <- seq(1, 0, length.out=length(tab$rows$marks))
			grid.polyline(x=c(0.80,0.80,rep(c(0.75,0.80),length(ticks))),y=c(0,1,rep(ticks,each=2)),id=rep(1:(length(ticks)+1),each=2))
			
			## percentages labels
			grid.text(marksChar,x=0.75, y=ticks, just="right")
		
		})
		#############################
		## Draw legend of the bins (bottom left)
		#############################

		cellplot(3,1, vpLegend, {
			grid.text("row bins:", x=0.1, y=unit(5, units="lines"), just="left")
			grid.text(paste("  ", tab$nBins), x=0.1, y=unit(4, units="lines"), just="left")
			grid.text("objects:", x=0.1, y=unit(2, units="lines"), just="left")
			grid.text(paste("  ", tab$rows$m), x=0.1, y=unit(1, units="lines"), just="left")
		})
	})
	
	#############################
	## Draw columns from left to right. Per column, check whether it is numeric or categorial.
	#############################

	for (i in 1:tab$n) {
		cellplot(1,i+1, vpColumn, {
			tCol <- tab$columns[[i]]
			cellplot(1,1, vpTitle, {
				## Determine column name. Place "log(...)" around name when scale is logarithmic
				columnName <- ifelse(tCol$isnumeric && tCol$scale=="log", paste("log(",tCol$name, ")", sep=""), tCol$name)
				nameWidth <- convertWidth(stringWidth(columnName), "npc",valueOnly=TRUE)
				cex <- max(0.7, min(1, 1/nameWidth))
				if ((nameWidth * cex) > 1) {
					columnName <- substr(columnName, 1, floor(nchar(columnName)/(nameWidth * cex)))
					nameWidth <- convertWidth(stringWidth(columnName), "npc",valueOnly=TRUE)
				}
				
				grid.text(columnName, gp=gpar(cex=cex))

				## Place sorting arrow before name
				if (tCol$sort!="") {
					arrowX <- min(0.1, 0.45 - 0.5 * nameWidth * cex)
					grid.lines(x=rep(arrowX, 2), y=c(0.7,0.2), arrow=arrow(angle = 20, length = unit(0.3, "npc"),ends = ifelse(tCol$sort=="decreasing", "first", "last"), type = "open"))
				}
			})
		
			if (tCol$isnumeric){
				plotNumCol(tCol, tab, blues, vpTitle, vpGraph, vpLegend)
			}
			else {
				catPalet <- color[c(1, (tCol$palet+1):length(color))]
				if (tCol$palet > 1) catPalet <- c(catPalet, color[2:(tCol$palet+1)])
			
				plotCatCol(tCol, tab, catPalet, vpTitle, vpGraph, vpLegend)
			}
		})
	}
}