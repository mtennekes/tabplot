library(devtools)

load_all("pkg")

### test installed package
library(tabplot)
library(tabplotGTK)


### test
library(ggplot2)
data(diamonds)

dDT <- as.data.table(diamonds)
dDT <- dDT[sample.int(nrow(dDT), 2.5e7, replace=TRUE),]

Rprof(tmp <- tempfile())
	tab <- tableplot(dDT, plot=FALSE)
Rprof(); summaryRprof(tmp); unlink(tmp)
