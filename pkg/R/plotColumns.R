plotNumCol2 <- function(tCol, tab, blues, vpTitle, vpGraph, vpLegend){

	r <- tCol$range
   
   lower <- (tCol$lower)/max(r)
   upper <- (tCol$upper)/max(r)
   m <- (tCol$mean)/max(r)
   
   cellplot(2,1,vpGraph, {		
		
      grid.rect( x = lower
               , y = tab$row$y
               , height = 0.9*tab$rows$heights
				   , just=c("left","bottom")
				   , width = upper-lower
               , gp = gpar(col=NA,fill = gray(0.9))
               )
		
		## plot small lines at the righthand side of the bins
		grid.rect( x = m
				   , y = tab$rows$y
				   , width = unit(0.75, "points")
				   , height = tab$rows$heights
				   , just=c("left","bottom")
				   , gp = gpar(col=NA, fill = "blue")
				   )		 
	})
}

plotColumns <-
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
					grid.polygon( x = c(0.1, 0.4, 0.7)
					            , y = if (tCol$sort=="decreasing") 
									     c(0.6, 0.2, 0.6)
								      else
						                 c(0.2, 0.6, 0.2)
							    , gp = gpar(fill="black")
							    , default.units = "snpc"
							    )
				}
			})
		
			if (tCol$isnumeric){
				plotNumCol2(tCol, tab, blues, vpTitle, vpGraph, vpLegend)
			}
			else {
				catPalet <- color[c(1, (tCol$palet+1):length(color))]
				if (tCol$palet > 1) catPalet <- c(catPalet, color[2:(tCol$palet+1)])
			
				plotCatCol(tCol, tab, catPalet, vpTitle, vpGraph, vpLegend)
			}
		})
	}
}