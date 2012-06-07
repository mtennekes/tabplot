library(devtools)

load_all("pkg")

### test installed package
library(tabplot)
library(tabplotGTK)


### test
library(ggplot2)
data(diamonds)


Rprof(tmp <- tempfile())

tab <- tableplot(diamonds, select=c(carat, depth, cut), 
				 subset=color=="F", ncols=4, plot=TRUE)


tableplot(diamonds, select=c(carat, depth), subset=price > 5000, title="test", showTitle=TRUE)

tableplot(diamonds, select=c(1,6))
tableplot(diamonds, select=c(TRUE, FALSE))


Rprof(); summaryRprof(tmp); unlink(tmp)



dDT <- as.data.table(diamonds)
dDT <- dDT[sample.int(nrow(dDT), 1e7, replace=TRUE),]




system.time({
	tab <- tableplot(dDT, plot=FALSE)
})