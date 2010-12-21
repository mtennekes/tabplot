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

# demo tests
source("demo/ffdf.R")
#source("demo/tableplot.R")
#source("demo/tableGUI.R")
