library(devtools)

load_all("pkg")
load_all("../tabplotGTK/pkg")


build("pkg")
check("pkg")

document("pkg")

build_vignettes("pkg")

### test installed package
library(tabplot)
library(tabplotGTK)


### test

library(ff)

library(ggplot2)
data(diamonds)


dDT <- as.data.table(diamonds)
dDT <- dDT[sample.int(nrow(dDT), 1e7, replace=TRUE),]

Rprof(tf <- "rprof.log", memory.profiling=TRUE)
system.time({
tab <- tableplot(dDT, plot=FALSE)
})
Rprof(NULL)
summaryRprof(tf)




tab <- tableplot(diamonds, ncolumns=5)
tableplot(diamonds, 
		  colNames=c("carat", "price", "cut", "color", "clarity"), 
		  sortCol="price")



tableSave(tab, 
		  filename="diamonds.png", 
		  width=5,
		  height=3,
		  fontsize = 6, 
		  legend.lines = 6)


tableplot(diamonds)

diamondsFF <- as.ffdf(diamonds)

tab <- tableplot(diamondsFF)

tableGUI()

tableSave(tab, filename="test.svg")
tableSave(tab, width=18, height=15, filename="test.eps")


## ps data
load("tabs_ps.Rdata")

tableSave(tab_ruw, file="sbs_unprocessed.eps", width=7, height=3.5, fontsize=5, legend.lines=6)

tableSave(tab_ruw, file="sbs_unprocessed.pdf", width=7, height=3.5, fontsize=5, legend.lines=6)