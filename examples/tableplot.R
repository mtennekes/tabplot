# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

# default tableplot
tableplot(diamonds)

# customized tableplot
tableplot(diamonds, select=c(carat, cut, color, clarity, price), sortCol=price, from=0, to=5)

# apply filter
tableplot(diamonds, subset=price < 5000 & cut=='Premium')
tableplot(diamonds, subset=cut)


