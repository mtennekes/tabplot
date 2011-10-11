# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

# assign tableplot as tabplot object
tab <- tableplot(diamonds, plot=FALSE)
 
# modify the tabplot object: reverse order of columns and customize palette
tab <- tableChange(tab, colNames=names(diamonds[1:7]), pals=list("Set4", "Set3", gray(seq(0,1,length.out=8))))
 
# plot modified tabplot object
plot(tab)
