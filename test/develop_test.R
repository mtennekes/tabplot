load_all()

### test installed package
library(tabplot)
library(tabplotGTK)


### test
library(ggplot2)
data(diamonds)
str(diamonds)

diamonds$carat2 <- factor(diamonds$carat)

tableplot(diamonds)






Rprof(tmp <- tempfile())

depth="table"
tab <- tableplot(diamonds, select=c(1, 3), subset_string="cut=='Fair'", sortCol=1, showTitle=TRUE)

plot(tab)

tableplot(diamonds, sortCol=price, colorNA="blue", numPals="Greens")


tableplot(diamonds, select=c(carat, depth), subset=price > 5000)

tableplot(diamonds, select=c(1,6))
tableplot(diamonds, select=c(TRUE, FALSE))


Rprof(); summaryRprof(tmp); unlink(tmp)

dt1 <- data.table(x=factor(1), y=1:10, c=factor(c("a", "b")), d=NA)
tab <- tableplot(dt1)


dt1 <- dt1[,rep(1:3,4), with=FALSE]
dt1$nieuw <- NA
tableplot(dt1)

dDT <- as.data.table(diamonds)
dDT <- dDT[sample.int(nrow(dDT), 1e6, replace=TRUE),]




system.time({
	tab <- tableplot(dDT, plot=FALSE)
})