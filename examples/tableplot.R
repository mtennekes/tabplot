# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

# default tableplot
tableplot(diamonds)

# most expensive diamonds
tableplot(diamonds, 
		  select=c(carat, cut, color, clarity, price), 
		  sortCol=price, 
		  from=0, 
		  to=5)

# for large datasets, we recommend to preprocess the data with tablePrepare:
p <- tablePrepare(diamonds)

# specific subsetting
tableplot(p, subset=price < 5000 & cut=='Ideal')

# change palettes
tableplot(p, 
		  pals=list(cut="Set4", color="Paired", clarity=grey(seq(0, 1,length.out=7))),
		  numPals=c(carat="Greens", price="Purples"))



# create a tableplot cut category, and fix scale limits of carat, table, and price
tabs <- tableplot(p, subset=cut,
	limitsX=list(carat=c(0,4), table=c(55, 65), price=c(0, 20000)), plot=FALSE)
plot(tabs[[3]], title="Very good cut diamonds")

