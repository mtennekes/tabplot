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

## test user-specified palettes
tableplot(diamonds,pals=list(1, gray(seq(0,1,length.out=10)), rainbow(8), 4))


tableGUI()
		
		
