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

# temp
sapply(list.files(), source)
tableGUI()


## load test data
require(ggplot2)
data(diamonds)
## add some NA's
is.na(diamonds$price) <- diamonds$cut=="Ideal"
is.na(diamonds$cut) <- (runif(nrow(diamonds))>0.8)

data(iris)



tableplot(diamonds)

## test user-specified palettes
tableplot(diamonds,pals=list(1, gray(seq(0,1,length.out=10)), rainbow(8), 4))

tableplot(diamonds, colNames=c("depth","table","price","x","y"), sortCol=1, decreasing=TRUE, scales="auto", nBins=100, from=0, to=100)


tableGUI()
	
		
