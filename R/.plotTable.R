.plotTable <-
function(tab) {
	
	#############################
	## Function to set viewport to specific grid cell
	#############################
	subplot <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

	
	#############################
	## Determine colors and color scales
	#############################
	blues <- colorRampPalette(brewer.pal(9,"Blues")[2:9],space="rgb")(100)
	
	lgrey <- brewer.pal(9,"Greys")[2]
	lred <- brewer.pal(9,"Reds")[2]

	set1 <- brewer.pal(9,"Set1")[2:9]
	set2 <- brewer.pal(8,"Set2")[c(1:6,8,7)]
	red <- brewer.pal(9,"Set1")[1]
	
	color <- list()
	color[[1]] <- c(red, set1, set2) 
	color[[2]] <- c(red, set2, set1)
	color[[3]] <- c(red, set1[3:8], set2, set1[1:2])
	color[[4]] <- c(red, set2[2:8], set1, set2[1])
	
	## checks if device is Cario {cairoDevice}
	isCairo <- (names(dev.cur())=="Cairo")
	
	
	#############################
	## Set layout
	#############################
	
	## configure viewports
	marginT <- 0.02;	marginB <- 0.02
	marginL <- 0.05;	marginR <- 0.05
	marginLT <- 0.0;	marginLB <- 0.02
	
	vpColLabel <- viewport(name = "histo", 
	  x = unit(marginL, "npc"),
	  y = unit(1 - marginT, "npc") - unit(1, "lines"),
	  width = unit(1 - marginL - marginR, "npc"),
	  height = unit(1, "lines"),
	  just = c("left", "bottom"),
	  gp=gpar(cex=0.75))
	vpCol <- viewport(name = "histo", 
	  x = unit(marginL, "npc"),
	  y = unit(marginB, "npc"),
	  width = unit(1 - marginL - marginR, "npc"),
	  height = unit(1 - marginB - marginT,"npc") - unit(1, "lines"),
	  just = c("left", "bottom"),
	  gp=gpar(cex=0.75))
	  
	vpColLeg <- viewport(name = "histo", 
	  x = unit(marginL, "npc"),
	  y = unit(marginLB, "npc"),
	  width = unit(1 - marginL - marginR, "npc"),
	  height = unit(1 - marginLB - marginLT,"npc"),
	  just = c("left", "bottom"),
	  gp=gpar(cex=0.75))
	
	## set grid layout
	grid.newpage()
	Layout <- grid.layout(nrow = 2, ncol = tab$n+1, widths = unit(c(3,rep(1,tab$n)), 
	c("lines",rep("null",tab$n))), heights = unit(c(1, 6), c("null", "lines")))

	pushViewport(viewport(layout = Layout))
	
	
	#############################
	## Configure y-axis
	#############################
	
	pushViewport(subplot(1, 1))
	pushViewport(vpCol)
	
	## y axes and bin ticks
	grid.polyline(x=c(0.80,0.80,rep(c(0.80,0.83),tab$nBins+1)),y=c(0,1,rep(c(tab$rows$y,1),each=2)),id=rep(1:(tab$nBins+2),each=2))

	## percentages ticks
	rests <- formatC(tab$rows$marks - floor(tab$rows$marks))	
	digits <- max(0,(max(sapply(rests, FUN=nchar))-2))
	marksChar <- formatC(tab$rows$marks, format="f", digits=digits)
	
	ticks <- seq(0, 1, length.out=length(tab$rows$marks))
	grid.polyline(x=c(0.80,0.80,rep(c(0.75,0.80),length(ticks))),y=c(0,1,rep(ticks,each=2)),id=rep(1:(length(ticks)+1),each=2))
	
	## percentages labels
	grid.text(marksChar,x=0.75, y=ticks, just="right")
	
	popViewport(n=2)


	#############################
	## Draw legend of the bins (bottom left)
	#############################

	pushViewport(subplot(2, 1))
	pushViewport(vpColLeg)

	grid.text("row bins:", x=0.1, y=unit(6, units="lines"), just="left")
	grid.text(paste("  ", tab$nBins), x=0.1, y=unit(5, units="lines"), just="left")
	grid.text("objects:", x=0.1, y=unit(3, units="lines"), just="left")
	grid.text(paste("  ", tab$rows$m), x=0.1, y=unit(2, units="lines"), just="left")

	popViewport(2)

	
	#############################
	## Draw columns from left to right. Per column, check whether it is numeric or categorial.
	#############################

	## count that is used to switch color palets for categorical variables.
	palet <- 1
	for (i in 1:tab$n) {
		#### Print column header
		pushViewport(subplot(1, i+1))
		pushViewport(vpColLabel)
		
		## Determine column name. Place "log(...)" around name when scale is logarithmic
		columnName <- ifelse(tab$columns[[i]]$isnumeric && tab$columns[[i]]$scale=="log", paste("log(",tab$columns[[i]]$name, ")", sep=""), tab$columns[[i]]$name)
		nameWidth <- convertWidth(stringWidth(columnName), "npc",valueOnly=TRUE)
		cex <- max(0.7, min(1, 1/nameWidth))
		if ((nameWidth * cex) > 1) {
			columnName <- substr(columnName, 1, floor(nchar(columnName)/(nameWidth * cex)))
			nameWidth <- convertWidth(stringWidth(columnName), "npc",valueOnly=TRUE)
		}
		
		grid.text(columnName, gp=gpar(cex=cex))

		## Place sorting arrow before name
		if (tab$columns[[i]]$sort!="") {
			arrowX <- min(0.1, 0.45 - 0.5 * nameWidth * cex)
			grid.lines(x=rep(arrowX, 2), y=c(0.7,0.2), arrow=arrow(angle = 20, length = unit(0.3, "npc"),ends = ifelse(tab$columns[[i]]$sort=="decreasing", "last", "first"), type = "open"))
		}

		popViewport()

		if (tab$columns[[i]]$isnumeric) {
			#### variable is numeric
			pushViewport(vpCol)
			grid.rect(gp = gpar(col=NA,fill = lgrey))
			
			## bins with all missings
			missings <- which(tab$columns[[i]]$compl==0)

			## when cairoDevice is enabled, not only fill the bins with colors, but also color the contours
			if (isCairo) {
				cols <- blues[tab$columns[[i]]$compl]
			} else {
				cols <- NA
			}
			
			## plot bins
			grid.rect(x = rep(tab$columns[[i]]$xline,tab$nBins), y = tab$rows$y,
			width = tab$columns[[i]]$widths, height = tab$rows$heights, just=c("left","bottom"),
			 gp = gpar(col=cols, fill = blues[tab$columns[[i]]$compl], linejoin="mitre"))
			
			## plot small lines at the righthand side of the bins
			grid.rect(x = rep(tab$columns[[i]]$xline,tab$nBins)+tab$columns[[i]]$widths, y = tab$rows$y,
			width = unit(0.75, "points"), height = tab$rows$heights, just=c("left","bottom"),
			 gp = gpar(col=NA, fill = blues[length(blues)]))

			
			if (isCairo) {
				cols <- lred
			} else {
				cols <- NA
			}
			 
			## plot bins with all missings as light red
			if (length(missings>0)) {
				grid.rect(x = rep(0, length(missings)), y = tab$rows$y[missings], width =  rep(1, length(missings)), height = tab$rows$heights[missings], just=c("left","bottom"), gp = gpar(fill = lred,col=cols, linejoin="mitre"))
			}
		 
			## plot broken x-axis
			if (tab$columns[[i]]$brokenX != 0) {
				blX <- ifelse(tab$columns[[i]]$brokenX==1, 0.15, 0.85)
				blW <- 0.05
				grid.rect(x=blX, width=blW, gp = gpar(col=NA,fill = lgrey))
				grid.polyline(x= blX + rep(c(-.5 * blW + c(-0.01, 0.01), .5 * blW + c(-0.01, 0.01)), 2),
					y = c(rep(c(-0.01, 0.01), 2), rep(c(0.99, 1.01), 2)), 
					id = rep(1:4,each=2), gp=gpar(col="white", lwd=3))
				grid.polyline(x= blX + rep(c(-.5 * blW + c(-0.01, 0.01), .5 * blW + c(-0.01, 0.01)), 2),
					y = c(rep(c(-0.01, 0.01), 2), rep(c(0.99, 1.01), 2)), 
					id = rep(1:4,each=2), gp=gpar(lwd=1))
			}

			popViewport(n = 2)
					
		} else {
			#### variable is categorical
			pushViewport(vpCol)

			## determine color indices for categories
			colorID <- rep(2:(length(color[[palet]])+1), length.out=length(tab$columns[[i]]$categories))
			if (tail(tab$columns[[i]]$categories, 1)=="missing") {
				colorID[length(colorID)] <- 1
			}
			
			## create large vector of colors (one color for each bin*category
			colorset <- color[[palet]][rep(colorID, each=tab$nBins)]
			
			if (isCairo) {
				cols <- colorset
			} else {
				cols <- NA
			}

			## draw bins
			grid.rect(x = tab$columns[[i]]$x, y = tab$rows$y,
			width = tab$columns[[i]]$widths, height = tab$rows$heights, just=c("left","bottom"),
			gp = gpar(col=cols, fill = colorset, linejoin="mitre"))

			popViewport(n=2)
			
			## draw layout
			pushViewport(subplot(2, i+1))
			pushViewport(vpColLeg)

			Layout2 <- grid.layout(nrow = length(tab$columns[[i]]$categories), ncol = 1)
		
			cex <- min(1, 1 / (convertHeight(unit(1,"lines"), "npc", valueOnly=TRUE) * length(tab$columns[[i]]$categories)))

			pushViewport(viewport(layout = Layout2, gp=gpar(cex=cex)))
			grid.rect(gp=gpar(col=NA, fill="white"))
			
			for (j in 1:length(tab$columns[[i]]$categories)) {
				pushViewport(subplot(j, 1))
				grid.rect(x = 0, y = 0.5, width = 0.2, height = 1, just=c("left"), gp = gpar(col=NA, fill = color[[palet]][colorID][j]))
				grid.text(tab$columns[[i]]$categories[j], x = 0.25, just="left")
				popViewport(n = 1)
			}
			
			popViewport(n = 3)
			palet <- ifelse(palet==4, 1, palet+1)
		}
	}
}