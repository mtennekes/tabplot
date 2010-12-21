#rootmap <- "d:/Rvis/"
rootmap <- "D:/R/Tableplot/Tableplot"
#scriptmap <- paste(rootmap, "TablePlot/R/", sep="")
datamap <- paste(rootmap, "data/", sep="")

setwd(rootmap)

## dependencies
require(RColorBrewer)
require(plyr)
require(grid)

## load tableplot scripts
sapply(list.files("R", full.names=TRUE), source)

#require(ggplot2)

source("demo/ffdf.R")
source("demo/tableplot.R")

#tableGUI()