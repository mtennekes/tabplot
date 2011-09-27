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



library(ff)
library(ggplot2)
data(diamonds)
tableplot(diamonds)


diamFF <- as.ffdf(diamonds)

diamFF$expensive <- as.ff(bit(nrow(diamFF)))

for (i in chunk(diamFF)) {
  diamFF$expensive[i] <- diamFF$price[i] >= 10000
}

tableplot(diamFF)