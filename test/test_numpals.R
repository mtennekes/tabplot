library(ggplot2)

data(diamonds)
tableplot(diamonds)

diamonds$x[sample(nrow(diamonds), 40000,  prob=diamonds$price/max(diamonds$price))] <- NA

tableplot(diamonds, select=c(carat,cut, price, x, y, x-y), sortCol=price)
