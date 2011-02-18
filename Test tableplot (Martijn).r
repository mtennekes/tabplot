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


### test package #######
install.packages("./tabplot_0.9-1.zip")
library(tabplot)


## load data
library(ggplot2)
data(diamonds)

diamonds$color[sample.int(50000, 5000)] <- NA


irisNA <- iris
# simulate missing data
is.na(irisNA$Sepal.Width) <- sample(1:nrow(iris), 30)
is.na(irisNA$Species) <- sample(1:nrow(iris), 15)

diamondsNA <- diamonds
# simulate missing data
is.na(diamondsNA$price) <- diamondsNA$cut == "Ideal"


## test GUI

tableGUI()

tableplot(diamonds,pals=list(1, gray(seq(0,1,length.out=10)), rainbow(8), 4))











tableplot(diamondsNA, from=40,to=50)

data(movies)
tableplot(movies[,c(3:5,17:24)], sortCol="rating", decreasing=FALSE, scales="lin", nBins=100)
	

tableplot(data.frame(palet_1_8 = factor(1:8), palet_9_16 = factor(9:16)))	

# test manually
dat <- diamonds
colNames <- names(dat)[c(1,3,4)]
sortCol <- colNames[c(1,2)]
decreasing <- c(FALSE, TRUE)
scales <- "auto"
nBins <- 100
from <- 25
to <- 50



