## dependencies
require(gWidgets)
require(gWidgetsRGtk2)
require(plyr)
require(RColorBrewer)
require(classInt)

## load tableplot scripts
scriptmap <- "./TablePlot/R/"
setwd(scriptmap)
sapply(list.files(), source)

## load test data
require(ggplot2)
data(diamonds)
## add some NA's
diamonds$color[sample.int(50000, 5000)] <- NA


tableplot(diamonds)
tableplot(diamonds
		, colNames=names(diamonds)
		, sortCol=c(3,1)
		, decreasing=c(FALSE, TRUE)
		, scales="auto"
		, nBins=100
		, from=0
		,to=100)


tableGUI()

		
		
## test tableplot.R  manually
dat <- diamonds
colNames <- names(dat)[c(1,3,4)]
sortCol <- colNames[c(1,2)]
decreasing <- c(FALSE, TRUE)
scales <- "auto"
nBins <- 100
from <- 25
to <- 50



