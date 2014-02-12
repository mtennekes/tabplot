library(ggplot2)
library(ff)
data(diamonds)


tp1 <- tableplot(diamonds, from=0, to=20)
tp2 <- tableplot(diamonds, from=10, to=30)

tp <- tp1 - tp2
plot(tp, relative=TRUE)


diamonds$carat <- diamonds$carat-10

tableplot(diamonds)
