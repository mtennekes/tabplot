library(devtools)

load_all("pkg")

### test installed package
library(tabplot)
library(tabplotGTK)


### test
library(ggplot2)
data(diamonds)


Rprof(tmp <- tempfile())

tab <- tableplot(diamonds, select=c(depth, carat, color), subset_string="cut=='Fair'", sortCol=depth, showTitle=FALSE)

plot(tab)


tableplot(diamonds, select=c(carat, depth), subset=price > 5000)

tableplot(diamonds, select=c(1,6))
tableplot(diamonds, select=c(TRUE, FALSE))


Rprof(); summaryRprof(tmp); unlink(tmp)



dDT <- as.data.table(diamonds)
dDT <- dDT[sample.int(nrow(dDT), 1e7, replace=TRUE),]




system.time({
	tab <- tableplot(dDT, plot=FALSE)
})