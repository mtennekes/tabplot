require(gWidgets)
require(gWidgetsRGtk2)
require(data.table)
require(ff)
require(RColorBrewer)
require(classInt)

scriptmap <- "../pkg/R/"
setwd(scriptmap)
sapply(list.files(), source)

setwd("../")  # in order to let it find data(tabplotPalettes)
data(tabplotPalettes)


## ff
options(fftempdir = "c:/fftemp/")
#require(ffbase)

library(ggplot2)
data(diamonds)
tableplot(diamonds)


diamFF <- as.ffdf(diamonds)

diamFF$expensive <- as.ff(bit(nrow(diamFF)))
diamFF$very_expensive <- ff(length=nrow(diamFF), vmode="logical")

for (i in chunk(diamFF)) {
  diamFF$expensive[i] <- diamFF$price[i] >= 5000
  diamFF$price[i][diamFF$cut[i]=="Ideal"] <- NA
  diamFF$very_expensive[i] <- diamFF$price[i] >= 10000
}

tableplot(diamFF)

diam <- diamonds
diam$expensive <- diam$price >= 5000
diam$price[diam$cut=="Ideal"] <- NA
diam$very_expensive <- diam$price >= 10000

tableplot(diam)

tableplot(diamonds, filter=expression(color=="D"))

tab <- tableplot(diamFF, filter=expression(color=="D"))

tab2 <- tableplot(diamFF)


tableSave(filename="test.png", tab=tab)
tableSave(filename="test.png", width=7, height=5, tab=tab, fontsize = 6, legend.lines = 6, showTitle = TRUE)