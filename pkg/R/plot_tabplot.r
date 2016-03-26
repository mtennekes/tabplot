#' Plot a \link{tabplot-object}
#' 
#' Plot a \link{tabplot-object}. The arguments of this function, which specify the layout, can also be passed on to \code{\link{tableplot}} directly. New in version 1.2 is the argument \code{numMode}, which determines how numeric variables are plot.
#'
#' @aliases plot.tabplot
#' @name plot.tabplot
#' @rdname plot.tabplot
#' @param x \code{\link{tabplot-object}} or \code{\link{tabplot_compare-object}}
#' @param fontsize the (maximum) fontsize
#' @param legend.lines the number of lines preserved for the legend
#' @param max_print_levels maximum number of printed category labels in the legend
#' @param text_NA text printed for the missing values category in the legend
#' @param title title of the plot (shown if \code{showTitle==TRUE})
#' @param showTitle show the title. By default \code{FALSE}, unless a \code{title} is given.
#' @param fontsize.title the fontsize of the title
#' @param showNumAxes plots an x-axis for each numerical variable, along with grid lines (\code{TRUE} by default).
#' @param rotateNames logical or numeric value that determines the rotation angle of the column names. If \code{TRUE}, they are rotated 90 degrees. By default, column names are rotated when the number of columns is greater than 15.
#' @param relative boolean that determines whether relative scales are used for relative tableplots. If \code{TRUE}, then \code{mean.diff.rel<-(mean2-mean1)/mean1*100} are used. If \code{FALSE}, then the absolute diference is taken: \code{mean <- mean2-mean}.
#' @param numMode character value that determines how numeric values are plotted. The value consists of the following building blocks, which are concatenated with the "-" symbol. The default value is "mb-sdb-sdl". Prior to version 1.2, "MB-ML" was the default value.
#' \describe{
#' \item{\code{sdb}}{sd bars between mean-sd to mean+sd are shown}
#' \item{\code{sdl}}{sd lines at mean-sd and mean+sd are shown}
#' \item{\code{mb}}{mean bars are shown}
#' \item{\code{MB}}{mean bars are shown, where the color of the bar indicate completeness where positive mean values are blue and negative orange}
#' \item{\code{ml}}{mean lines are shown}
#' \item{\code{ML}}{mean lines are shown, where positive mean values are blue and negative orange}
#' \item{\code{mean2}}{mean values are shown}
#' }
#' @param vp \code{\link[grid:viewport]{viewport}} to draw plot in (for instance useful to stack multiple tableplots)
#' @param ... other arguments are not used
#' @example ../examples/plot_tabplot.R
#' @export
#' @import grid
#' @method plot tabplot
plot.tabplot <-
function(x, fontsize = 10, legend.lines = 8, max_print_levels = 15, text_NA = "missing", title = NULL, showTitle = NULL, fontsize.title = 14, showNumAxes=TRUE, rotateNames = NA, relative=FALSE, numMode="mb-sdb-ml", vp=NULL, ...) {

	
	if (!(class(x)[1] %in% c("tabplot", "tabplot_compare"))) stop(paste(deparse(substitute(x)), "is not a tabplot-object"))
	
	compare <- (class(x)=="tabplot_compare")
	#relative <- relative && compare
	
	numMode <- strsplit(numMode, "-", fixed=TRUE)[[1]]
	
	if (compare && any(c("sdb", "sdl") %in% numMode)) message("Independence of the compared tableplots assumed for the calculation of the standard deviations.")
	
	if (length(fontsize)!=1 || !is.numeric(fontsize)) stop("invalid fontsize")

	if (length(legend.lines)!=1 || !is.numeric(legend.lines)) stop("invalid legend.lines")
	if (length(max_print_levels)!=1 || !is.numeric(max_print_levels)) stop("invalid max_print_levels")
	
	if (max_print_levels < legend.lines) warning("max_print_levels is less than legend.lines")
	
	if (length(text_NA)!=1) stop("invalid text_NA")
	
	if (missing(showTitle)) showTitle <- !missing(title)
	
	if (missing(title)) {
		dataset <- ifelse(class(x)=="tabplot", x$dataset, paste(x$dataset2, x$dataset1, sep=" - "))
		title <- ifelse(length(x$subset)==0, dataset, paste(dataset, " (", x$subset, ")", sep=""))
	}
	
	if (is.na(rotateNames)) {
		if (x$m	> 15) rotateNames <- 90 else rotateNames <- 0
	} else if (is.logical(rotateNames)) {
		rotateNames <- ifelse(rotateNames, 90, 0)	
	}
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
	
	## set grid layout
	if (is.null(vp)) {
		grid.newpage()
	} else {
		if (is.character(vp)) 
			seekViewport(vp)
		else pushViewport(vp)
	}
	
	## configure viewports
	marginT <- 0.01;	marginB <- 0.02
	marginL <- 0.05;	marginR <- 0.05
	marginLT <- 0.0;	marginLB <- 0.02
	
	colNameHeight <- if (rotateNames==0) 1 else max(convertWidth(stringWidth(x$select), "lines", valueOnly = TRUE)) + .5
	vpColumn <- viewport( name="Column"
	                   ,  x = unit(marginL, "npc")
 					    , width = unit(1 - marginL - marginR, "npc")
	                    , layout = grid.layout( nrow=3
						                      , ncol=1
						                      , heights = unit(c(colNameHeight, 1, legend.lines), c("lines", "null", "lines"))
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
						 						, ncol = x$m+1
						 						, widths = unit(c(3,rep(1,x$m)), c("lines",rep("null",x$m)))
						   )
					   , gp=gpar(fontsize=fontsize)
	)
	
	  
	pushViewport(vpBody)
	
	if (showTitle) cellplot(1, 1, e={
		grid.text(title)	
	})
	
	cellplot(2, 1, vpBodyCols, {
	
		
		
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
				from <- x$from
				to <- x$to

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
				if (compare) {
					numbers <- with(x, c(n1, n2))
				} else {
					numbers <- with(x, c(n, round(n/nBins), N))
				}
				formats <- format(numbers, big.mark=",")
				widths <- convertWidth(stringWidth(formats), "npc", valueOnly=TRUE)
				width <- max(widths)
				spacer <- 0.1 + convertWidth(stringWidth("\t"), "npc", valueOnly=TRUE)
				xpos <- spacer + width
				space.row_bins <- convertWidth(stringWidth("row bins:  "), "npc", valueOnly=TRUE)
				grid.text("row bins:", x=0.1, y=unit(5.5, units="lines"), just="left")
				grid.text(x$nBins, x=space.row_bins, y=unit(5.5, units="lines"), just="left")
				
				
				grid.text("objects:", x=0.1, y=unit(4, units="lines"), just="left")
				grid.text(formats[1], x=xpos, y=unit(3, units="lines"), just="right")
				grid.text(formats[2], x=xpos, y=unit(2, units="lines"), just="right")

				if (compare) {
					grid.text(paste(" (", x$dataset1, ")", sep=""), x=xpos, y=unit(3, units="lines"), just="left")
					grid.text(paste(" (", x$dataset2, ")", sep=""), x=xpos, y=unit(2, units="lines"), just="left")
				} else {
					grid.text(" (per bin)", x=xpos, y=unit(2, units="lines"), just="left")
					if (numbers[1]!=numbers[3]) {
						grid.text(" (sample)", x=xpos, y=unit(3, units="lines"), just="left")
						grid.text(formats[3], x=xpos, y=unit(1, units="lines"), just="right")
						grid.text(" (full data)", x=xpos, y=unit(1, units="lines"), just="left")
					}
				}
			})
		})
		
		#############################
		## Draw columns from left to right. Per column, check whether it is numeric or categorial.
		#############################
		
	
		for (i in 1:x$m) {
			cellplot(1,i+1, vpColumn, {
				tCol <- x$columns[[i]]
				decreasing <- ifelse(i==x$sortCol, x$decreasing, NA)
				cellplot(1, 1, vpTitle, {
					## Determine column name. Place "log(...)" around name when scale is logarithmic
					columnName <- ifelse(tCol$isnumeric && tCol$scale_final=="log", paste("log(",tCol$name, ")", sep=""), tCol$name)
					nameWidth <- convertWidth(stringWidth(columnName), "npc",valueOnly=TRUE)
					cex <- max(0.7, min(1, 1/nameWidth))
					if ((nameWidth * cex) > 1) {
						columnName <- substr(columnName, 1, floor(nchar(columnName)/(nameWidth * cex)))
						nameWidth <- convertWidth(stringWidth(columnName), "npc",valueOnly=TRUE)
					}
					
					grid.text(columnName, gp=gpar(cex=cex), rot=rotateNames)
					
					## Place sorting arrow before name
					if (!is.na(decreasing)) {
						#grid.rect(gp=gpar(fill="red"))
						grid.polygon( x = c(0.1, 0.4, 0.7) / colNameHeight
						            , y = if (decreasing) 
										     c(0.6, 0.2, 0.6) / colNameHeight
									      else
							                 c(0.2, 0.6, 0.2) / colNameHeight
								    , gp = gpar(fill="black")
								    , default.units = "snpc"
								    )
					}
				})
			
				if (tCol$isnumeric){
					plotNumCol(tCol, x, vpTitle, vpGraph, vpLegend, showNumAxes, relative, numMode)
				}
				else {
					plotCatCol(tCol, x, vpTitle, vpGraph, vpLegend, max_print_levels,
							   text_NA, legend.lines, compare)
				}
			})
		}
	})
  
	upViewport(1 + !is.null(vp))
}

#' @rdname plot.tabplot
#' @usage \method{plot}{tabplot_compare}(x, ...)
#' @export
plot.tabplot_compare <-function(x, ...) {
	plot.tabplot(x, ...)
}