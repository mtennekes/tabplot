######################################
## dependencies
######################################
require(gWidgets)
require(gWidgetsRGtk2)
require(data.table)
require(ff)
require(RColorBrewer)
require(classInt)


######################################
## load tableplot scripts
######################################
scriptmap <- "../pkg/R/"
setwd(scriptmap)
sapply(list.files(), source)

setwd("../")  # in order to let it find data(tabplotPalettes)
data(tabplotPalettes)
wd1 <- getwd()
setwd("inst/doc/")
wd2 <- getwd()

## before compiling (build.bat), uncomment two lines in tableplot_checkPals and tableGUI_showAllPals


## Sweave
setwd(wd2)
Sweave("tabplot-vignette.Snw")

## compile
setwd(wd1)
setwd("R/")
setwd(scriptmap)
sapply(list.files(), source)

######################################
## load test data
######################################

require(ggplot2)
data(diamonds)
## add some NA's
is.na(diamonds$price) <- diamonds$cut=="Ideal"
is.na(diamonds$cut) <- (runif(nrow(diamonds))>0.8)
## create logical variable
diamonds$expensive <- diamonds$price >= 10000

diamondsDT <- data.table(diamonds)
diamondsFF <- as.ffdf(diamonds)

data(iris)

######################################
## test functions
######################################

## show palettes
tableplot_showPalettes()


## tableplot
tableplot(diamonds, fontsize=7, legend.lines=5)

## with generic plot function
tab <- tableplot(diamonds, plot=FALSE)
plot(tab)

pals <- list(1, gray(seq(0,1,length.out=10)), rainbow(8), 4)
tab2 <- changeTabplot(tab, colNames=rev(names(diamonds)), flip=TRUE, pals=pals)

plot(tab2)


## test GUI
tableGUI()

tableGUI(diamonds, colNames=c("table","price","x","y","z","expensive"), sortCol=2, decreasing=FALSE, scales="auto", nBins=100, from=0, to=100)
tableGUI(diamonds, pals=list(1, gray(seq(0,1,length.out=10)), rainbow(8), 4))
tableplot(diamonds, pals=list("hcl12(4)", gray(seq(0,1,length.out=10)), rainbow(8), 4))

tableGUI(diamonds)