library(devtools)

load_all("pkg")

### test installed package
library(tabplot)
library(tabplotGTK)


### test
library(ggplot2)
data(diamonds)

dDT <- as.data.table(diamonds)
dDT <- dDT[sample.int(nrow(dDT), 1e7, replace=TRUE),]

system.time({
	tab <- tableplot(dDT, plot=FALSE)
})