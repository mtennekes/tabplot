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


## show palettes
tableGUI_showAllPals()


## tableplot
tableplot(diamonds)

## with generic plot function
tab <- tableplot(diamonds, plot=FALSE)
plot(tab)

## test tableplot (ff)
require(ff)
diamondsFF <- as.ffdf(diamonds)


## test user-specified palettes
tableplot(diamonds,pals=list(1, gray(seq(0,1,length.out=10)), rainbow(8), brewer.pal(n=8,"Set2")))

## test GUI
tableGUI()

tableGUI(diamonds, colNames=c("table","price","x","y","z","expensive"), sortCol=2, decreasing=FALSE, scales="auto", nBins=100, from=0, to=100)
tableGUI(diamonds, pals=list(1, gray(seq(0,1,length.out=10)), rainbow(8), 4))
tableplot(diamonds, pals=list("hcl12(4)", gray(seq(0,1,length.out=10)), rainbow(8), 4))

tableGUI(diamonds)