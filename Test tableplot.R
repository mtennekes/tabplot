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

setwd("../data/")
load("tabplotPalettes.Rda")
setwd("../R/")

## load test data
require(ggplot2)
data(diamonds)
## add some NA's
is.na(diamonds$price) <- diamonds$cut=="Ideal"
is.na(diamonds$cut) <- (runif(nrow(diamonds))>0.8)

## create logical variable
diamonds$expensive <- diamonds$price >= 10000


data(iris)

tableGUI_showAllPals()


## test tableplot
tableplot(diamonds)

## test tableplot (ff)
diamondsFF <- as.ffdf(diamonds)
tableplot(diamondsFF)


tableplot(diamonds, colNames=c("depth","table","price","x","y"), sortCol=1, decreasing=TRUE, scales="auto", nBins=100, from=0, to=100)



## test user-specified palettes
tableplot(diamonds,pals=list(1, gray(seq(0,1,length.out=10)), rainbow(8), brewer.pal(n=8,"Set2")))

## test checkPals
pals<-list(1, gray(seq(0,1,length.out=10)), rainbow(8), brewer.pal(n=8,"Set2"), "Set2", "default(9)")



## temp
diamonds$expensive <- diamonds$price > 5000
diamonds$date <- as.POSIXct(diamonds$table, origin="1970-01-01")
sapply(list.files(), source)
tableGUI()

tableplot(diamonds, colNames=c("table","price","x","y","z","expensive"), sortCol=2, decreasing=FALSE, scales="auto", nBins=100, from=0, to=100)

tableGUI(diamonds, colNames=c("table","price","x","y","z","expensive"), sortCol=2, decreasing=FALSE, scales="auto", nBins=100, from=0, to=100)

## test GUI
tableGUI()

tableGUI(diamonds, colNames=c("table","price","x","y","z","expensive"), sortCol=2, decreasing=FALSE, scales="auto", nBins=100, from=0, to=100)
tableGUI(diamonds, pals=list(1, gray(seq(0,1,length.out=10)), rainbow(8), 4))
tableplot(diamonds, pals=list("hcl12(4)", gray(seq(0,1,length.out=10)), rainbow(8), 4))

tableGUI(diamonds)