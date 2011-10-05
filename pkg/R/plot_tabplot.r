#' Function that plots a \link{tabplot-object}.
#'
#' @aliases plot.tabplot
#' @param x tabplot object
#' @param fontsize the (maximum) fontsize
#' @param legend.lines the number of lines preserved for the legend
#' @param title title of the plot (shown if \code{showTitle==TRUE})
#' @param showTitle show the title
#' @param ... arguments passed to other methods
#' @export
#' @method plot tabplot
plot.tabplot <-
function(x, fontsize = 8, legend.lines = 8, title = ifelse(length(x$filter)==0, x$dataset, paste(x$dataset, " (", x$filter, ")", sep="")), showTitle = FALSE, ...) {
	
		
	#############################
	## Determine colors and color scales
	#############################
	
	blues <- c(NA, colorRampPalette(brewer.pal(9,"Blues")[2:9],space="rgb")(100))
	# blues[1] is for missing values, blues[2]...blues[101] is for completion percentages of 1...100

	lgrey <- brewer.pal(9,"Greys")[2]
	lred <- brewer.pal(9,"Reds")[2]
	red <- brewer.pal(9,"Set1")[1]
	
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
						                      , heights = unit(c(1,1, legend.lines), c("lines","null", "lines"))
						    				  )
	                   , gp=gpar(fontsize=fontsize)
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
	
	Layout <- grid.layout( nrow = 2, ncol = 1, height = unit(c(ifelse(showTitle, 2, 0), 1), c("lines", "null")))
	
	pushViewport(viewport(layout = Layout))
	
	if (showTitle) cellplot(1, 1, e={
		grid.text(title)	
	})
	
	cellplot(2, 1, e={
	
		BodyLayout <- grid.layout( nrow = 1, ncol = x$n+1
		                     , widths = unit(c(3,rep(1,x$n)), c("lines",rep("null",x$n)))
							 )
		pushViewport(viewport(layout = BodyLayout))
		
		
		#############################
		## Configure y-axis
		#############################
		
		cellplot(1,1,vpColumn, {
			cellplot(2,1,vpGraph,{
				## y axes and bin ticks
				grid.polyline( x=c(0.80,0.80,rep(c(0.80,0.83),x$nBins+1))
				             , y=c(0,1,rep(c(x$rows$y,1),each=2))
							 , id=rep(1:(x$nBins+2),each=2)
							 )
	
				## percentages ticks
				rests <- formatC(x$rows$marks - floor(x$rows$marks))	
				digits <- max(0,(max(sapply(rests, FUN=nchar))-2))
				marksChar <- paste(formatC(x$rows$marks, format="f", digits=digits),"%", sep="")
				
				ticks <- seq(1, 0, length.out=length(x$rows$marks))
				grid.polyline(x=c(0.80,0.80,rep(c(0.75,0.80),length(ticks))),y=c(0,1,rep(ticks,each=2)),id=rep(1:(length(ticks)+1),each=2))
				
				## percentages labels
				grid.text(marksChar,x=0.75, y=ticks, just="right")
			
			})
			#############################
			## Draw legend of the bins (bottom left)
			#############################
	
			cellplot(3,1, vpLegend, {
				grid.text("row bins:", x=0.1, y=unit(5, units="lines"), just="left")
				grid.text(paste("  ", x$nBins), x=0.1, y=unit(4, units="lines"), just="left")
				grid.text("objects:", x=0.1, y=unit(2, units="lines"), just="left")
				grid.text(paste("  ", x$rows$m), x=0.1, y=unit(1, units="lines"), just="left")
			})
		})
		
		#############################
		## Draw columns from left to right. Per column, check whether it is numeric or categorial.
		#############################
	
		for (i in 1:x$n) {
			cellplot(1,i+1, vpColumn, {
				tCol <- x$columns[[i]]
				cellplot(1,1, vpTitle, {
					## Determine column name. Place "log(...)" around name when scale is logarithmic
					columnName <- ifelse(tCol$isnumeric && tCol$scale_final=="log", paste("log(",tCol$name, ")", sep=""), tCol$name)
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
					plotNumCol(tCol, x, blues, vpTitle, vpGraph, vpLegend)
				}
				else {
					catPalet <- c(red, tCol$palet)
					plotCatCol(tCol, x, catPalet, vpTitle, vpGraph, vpLegend)
				}
			})
		}
	})

}