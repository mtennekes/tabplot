######################################
## dependencies
######################################
require(gWidgets)
require(gWidgetsRGtk2)
require(plyr)
require(RColorBrewer)
require(classInt)

require(data.table)

######################################
## load tableplot scripts
######################################
scriptmap <- "./tabplot/R/"
setwd(scriptmap)
sapply(list.files(), source)

setwd("../data/")
load("tabplotPalettes.Rda")
setwd("../R/")

## load data
library(ggplot2)
data(diamonds)
## add some NA's
is.na(diamonds$price) <- diamonds$cut=="Ideal"
is.na(diamonds$cut) <- (runif(nrow(diamonds))>0.8)
## create logical variable
diamonds$expensive <- diamonds$price >= 10000

## duplicate diamonds 5 times: 1726080 records
for(i in 1:5){
	diamonds <- rbind(diamonds, diamonds)
}

dDT <- data.table(diamonds)


## test data.table

system.time({
	dev.new(width=13)
	tableplot(diamonds, useDT=FALSE)
})

system.time({
	dev.new(width=13)
	tableplot(dDT, useDT=TRUE)
})




dat<-diamonds
colNames<-names(dat)
sortCol<-c(1, 3)
decreasing<-TRUE
scales<-"auto"
pals<-list(1, 9, 3, 10)
nBins<-100
from<-5
to<-100
plot<-TRUE
filter<-NULL
useDT<-TRUE



