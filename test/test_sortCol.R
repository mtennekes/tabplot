library(ggplot2)
data(diamonds)

a <- function(id) tableplot(diamonds, sortCol=id)


tableplot(diamonds, sortCol="color")
tableplot(diamonds, sortCol=3)
a(3)
a("color")
tableplot(diamonds, sortCol=color)
