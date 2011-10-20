# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

# default tableplot
tableplot(diamonds)

# customized tableplot
tableplot(diamonds, colNames=c("carat", "cut", "color", "clarity", "price"), sortCol="price", from=0, to=5)

# apply filter
tableplot(diamonds, filter="price < 5000 & cut=='Premium'")
tableplot(diamonds, filter="cut")

