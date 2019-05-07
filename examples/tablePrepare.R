# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

p <- tablePrepare(diamonds)

tableplot(p, nBins=200, sortCol=depth)
tableplot(p, nBins=50, sortCol=price)

close(p)
