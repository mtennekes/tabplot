#' Plot a \link{tabplot-object}
#'
#' @aliases plot.tabplot
#' @param x \link{tabplot-object}
#' @param fontsize the (maximum) fontsize
#' @param legend.lines the number of lines preserved for the legend
#' @param max_print_levels maximum number of printed category labels in the legend
#' @param text_NA text printed for the missing values category in the legend
#' @param title title of the plot (shown if \code{showTitle==TRUE})
#' @param showTitle show the title. By default \code{FALSE}, unless a \code{title} is given.
#' @param fontsize.title the fontsize of the title
#' @param showNumAxes plots an x-axis for each numerical variable, along with grid lines (\code{TRUE} by default).
#' @param vp \code{\link[grid:viewport]{viewport}} to draw plot in (for instance useful to stack multiple tableplots)
#' @param ... other arguments are not used
#' @example ../examples/plot_tabplot.R
#' @export
#' @method plot tabplot
plot.tabplot <-
function(x, fontsize = 10, legend.lines = 8, max_print_levels = 15, text_NA = "missing", title = NULL, showTitle = NULL, fontsize.title = 14, showNumAxes=TRUE, vp=NULL, ...) {

	require(grid) # only needed for devtools::load_all(), not for package building
	
	if (class(x)[1]!="tabplot") p(paste(deparse(substitute(x)), "is not a tabplot-object"))
	
	if (length(fontsize)!=1 || !is.numeric(fontsize)) stop("invalid fontsize")

	if (length(legend.lines)!=1 || !is.numeric(legend.lines)) stop("invalid legend.lines")
	if (length(max_print_levels)!=1 || !is.numeric(max_print_levels)) stop("invalid max_print_levels")
	
	if (max_print_levels < legend.lines) warning("max_print_levels is less than legend.lines")
	
	if (length(text_NA)!=1) stop("invalid text_NA")
	
	if (missing(showTitle)) showTitle <- !missing(title)
	
	if (missing(title)) 
		title <- ifelse(length(x$filter)==0, x$dataset, paste(x$dataset, " (", x$filter, ")", sep=""))
	
	#############################
	## Determine colors and color scales
	#############################
	
	#blues <- c(NA, colorRampPalette(brewer.pal(9,"Blues")[2:9],space="rgb")(100))

	lgrey <- "#F0F0F0"	#brewer.pal(9,"Greys")[2]
	lred <- "#FEE0D2"	#brewer.pal(9,"Reds")[2]
	#red <- "#E41A1C"	#brewer.pal(9,"Set1")[1]
	
	#############################
	## Set layout
	#############################
	
	## configure viewports
	marginT <- 0.01;	marginB <- 0.02
	marginL <- 0.05;	marginR <- 0.05
	marginLT <- 0.0;	marginLB <- 0.02
	
	vpColumn <- viewport( name="Column"
	                   ,  x = unit(marginL, "npc")
 					    , width = unit(1 - marginL - marginR, "npc")
	                    , layout = grid.layout( nrow=3
						                      , ncol=1
						                      , heights = unit(c(1, 1, legend.lines), c("lines", "null", "lines"))
						    				  )
					   )
						
	vpTitle <- viewport( name = "title"
					   , just = c("left", "center")
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
	
	vpBody <- viewport( name="Body"
						, layout = grid.layout( nrow=2
												, ncol=1
												, heights = unit(c(ifelse(showTitle, 2, 0), 1), c("lines", "null"))
						)
						, gp=gpar(fontsize=fontsize.title)
	)
	
	vpBodyCols <- viewport(name="BodyCols"
						 , layout = grid.layout( nrow = 1
						 						, ncol = x$n+1
						 						, widths = unit(c(3,rep(1,x$n)), c("lines",rep("null",x$n)))
						   )
					   , gp=gpar(fontsize=fontsize)
	)
	
	  
	## set grid layout
	if (is.null(vp)) {
	  grid.newpage()
	} else {
	  if (is.character(vp)) 
	    seekViewport(vp)
	  else pushViewport(vp)
	}
	
	pushViewport(vpBody)
	
	if (showTitle) cellplot(1, 1, e={
		grid.text(title)	
	})
	
	cellplot(2, 1, vpBodyCols, {
	
		#BodyLayout <- grid.layout( nrow = 1, ncol = x$n+1
		#                     , widths = unit(c(3,rep(1,x$n)), c("lines",rep("null",x$n)))
	#						 )
		#pushViewport(viewport(layout = BodyLayout))
		
		
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
				marks <- x$rows$marks
				from <- x$rows$from
				to <- x$rows$to

				marksPos <- 1 - (marks - from) / (to - from)
				marksVis <- marksPos >=0 & marksPos <=1
				
				rests <- formatC(marks - floor(marks))	
				digits <- max(0,(max(sapply(rests, FUN=nchar))-2))
				marksChar <- paste(formatC(marks, format="f", digits=digits),"%", sep="")
				
				grid.polyline(x=c(0.80,0.80,rep(c(0.75,0.80),sum(marksVis))),
							  y=c(0,1,rep(marksPos[marksVis],each=2)),
							  id=rep(1:(sum(marksVis)+1),each=2))
				
				## percentages labels
				grid.text(marksChar[marksVis],x=0.75, y=marksPos[marksVis], just="right")
			
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
				cellplot(1, 1, vpTitle, {
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
					if (!is.na(tCol$sort_decreasing)) {
						grid.polygon( x = c(0.1, 0.4, 0.7)
						            , y = if (tCol$sort_decreasing) 
										     c(0.6, 0.2, 0.6)
									      else
							                 c(0.2, 0.6, 0.2)
								    , gp = gpar(fill="black")
								    , default.units = "snpc"
								    )
					}
				})
			
				if (tCol$isnumeric){
					plotNumCol(tCol, x, vpTitle, vpGraph, vpLegend, showNumAxes)
				}
				else {
					plotCatCol(tCol, x, vpTitle, vpGraph, vpLegend, max_print_levels,
							   text_NA, legend.lines)
				}
			})
		}
	})
  
	upViewport(1 + !is.null(vp))
}