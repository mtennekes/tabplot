library(ggplot2)
library(ff)
data(diamonds)


tp1 <- tableplot(diamonds, sortCol=color, from=0, to=10)
tp2 <- tableplot(diamonds, sortCol=color, from=5, to=15)

tp <- tp1 - tp2
plot(tp, relative=TRUE)


tableplot(diamonds)

diamonds$cut[diamonds$carat>.5 & diamonds$carat < 1.5] <- NA
diamonds$price[diamonds$carat>.5 & diamonds$carat < 1.5] <- NA

tp1 <- tableplot(diamonds, from=0, to=75)
tp2 <- tableplot(diamonds, from=25, to=100)

plot(tp <- tp2 - tp1, relative=FALSE)

