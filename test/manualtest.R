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
setwd(wd1)

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
tab2 <- changeTabplot(tab, colNames=rev(names(diamonds)), flip=TRUE, pals=list(rainbow(6), rainbow(3)))


plot(tab2)


## test GUI
tableGUI()

tableGUI(tab2)

tableGUI(diamonds, colNames=c("table","price","x","y","z"), sortCol=2, decreasing=FALSE, scales="auto", nBins=100, from=0, to=100)
tableGUI(diamonds, pals=list(1, gray(seq(0,1,length.out=10)), rainbow(8), 4))
tableplot(diamonds, pals=list("hcl12(4)", gray(seq(0,1,length.out=10)), rainbow(8), 4))

tableGUI(diamonds)

###########################################
## test datatime2fac
###########################################
dat <- data.frame(x = rnorm(10000,mean=10, sd=2), y=factor(round(runif(10000)*6)))
dat$df <- ISOdate(2011, 7, 14) + 10000*86400*runif(100)
dat$df2 <- ISOdate(2011, 7, 14) + 10000*3600*runif(100)
dat$df3 <- ISOdate(2011, 7, 14) + 20*runif(100)
dat$df4 <- rep(sample(seq(Sys.Date(), length.out=100, by="1 week")), length.out=10000)
dat$df5 <- rep(Sys.Date(), length.out=10000)
dat$temp <- datetime2fac(dat$df4)

str(datetime2fac(dat$df))

