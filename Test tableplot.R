## dependencies
require(gWidgets)
require(gWidgetsRGtk2)
require(plyr)
require(RColorBrewer)
require(classInt)

## load tableplot scripts
scriptmap <- "./tabplot/R/"
setwd(scriptmap)
sapply(list.files(), source)

## load test data
require(ggplot2)
data(diamonds)
## add some NA's
is.na(diamonds$price) <- diamonds$cut=="Ideal"
is.na(diamonds$cut) <- (runif(nrow(diamonds))>0.8)

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

tab <- preprocess(diamonds
		, colNames=names(diamonds)
		, sortCol=c(3,1)
		, decreasing=c(FALSE, TRUE)
		, scales="auto"
		, nBins=100
		, from=0
		,to=100)

