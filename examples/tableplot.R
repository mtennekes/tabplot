# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

# default tableplot
tableplot(diamonds)

# customized tableplot
tableplot(diamonds, 
		  select=c(carat, cut, color, clarity, price), 
		  sortCol=price, 
		  from=0, 
		  to=5)

# specific subsetting
tableplot(diamonds, subset=price < 5000 & cut=='Premium')

# create a tableplot per category of the variable cut
tableplot(diamonds, subset=cut)
 



