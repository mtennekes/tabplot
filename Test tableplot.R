

rootmap <- "d:/Rvis/"
scriptmap <- paste(rootmap, "TablePlot/R/", sep="")
datamap <- paste(rootmap, "data/", sep="")

setwd(scriptmap)
source("tableplot.R")
source(".plotTable.R")
source("num2fac.R") 
source("tableGUI.R")
require(gWidgets)
require(gWidgetsRGtk2)
#require(gWidgetstcltk)
require(plyr)
require(RColorBrewer)
require(classInt)

require(ff)

library(ggplot2)
data(diamonds)

diamonds$color[sample.int(50000, 5000)] <- NA


setwd(datamap)
load("ps0607.Rdata")
load("ps07pop.Rdata")



tableplot(diamonds)


# test manually
dat <- diamonds
colNames <- names(dat)[c(1,3,4)]
sortCol <- colNames[c(1,2)]
decreasing <- c(FALSE, TRUE)
scales <- "auto"
nBins <- 100
from <- 25
to <- 50



