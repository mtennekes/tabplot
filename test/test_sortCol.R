library(ggplot2)
data(diamonds)

diamonds$order <- 1:nrow(diamonds)

a <- function(id) tableplot(diamonds, sortCol=id)


tableplot(diamonds, sortCol="color")
tableplot(diamonds, sortCol=3)
tableplot(diamonds, sortCol=color)

a(3)
a("color")


tableplot(diamonds, sortCol="order")
tableplot(diamonds, sortCol=3)
tableplot(diamonds, sortCol=order)


depth <- "table"

tableplot(diamonds, sortCol=depth)
