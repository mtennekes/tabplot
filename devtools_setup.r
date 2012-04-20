library(devtools)

load_all("pkg")


build("pkg")
check("pkg")

document("pkg")

build_vignettes("pkg")

### test installed package
library(tabplot)


### test

library(ff)

library(ggplot2)
data(diamonds)

tableplot(diamonds)

diamondsFF <- as.ffdf(diamonds)

tab <- tableplot(diamondsFF)


tableSave(tab, filename="test.svg")
tableSave(tab, width=18, height=15, filename="test.eps")
