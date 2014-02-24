# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

# assign tableplot as tabplot object
tab <- tableplot(diamonds)
 
# modify the tabplot object: reverse order of columns and customize palette
tab <- tableChange(tab, select_string=rev(names(diamonds)), 
				   pals=list(clarity=gray(seq(0,1,length.out=8))))
 
# plot modified tabplot object
plot(tab)
