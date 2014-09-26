library(ggplot2)
data(diamonds)

tableplot(diamonds)

diamonds$x[sample(nrow(diamonds), 20000,  prob=diamonds$price/max(diamonds$price))] <- NA

tableplot(diamonds, select=c(carat,cut, x, y, x-y))
