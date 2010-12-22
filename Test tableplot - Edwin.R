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
source("demo/tableplot.R")
source("demo/tableGUI.R")




# data(iris)
# irisNA <- iris

# # simulate missing data
# is.na(irisNA$Sepal.Width) <- sample(1:nrow(iris), 30)
# is.na(irisNA$Species) <- sample(1:nrow(iris), 15)

# library(ff)
# irisNA <- as.ffdf(irisNA)
# tableplot(irisNA, sortCol=5)
