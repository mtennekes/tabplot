library(ggplot2)
data(diamonds)

d1 <- diamonds[sample(nrow(diamonds), 1000), ]
d2 <- diamonds[sample(nrow(diamonds), 1000), ]

t1 <- tableplot(d1)
t2 <- tableplot(d2)

td <- t2 - t1
plot(td)

names(d1) <- paste0(names(d1), "2")


d12 <- cbind(d1, d2)

tableplot(d12)

tableplot(d12, select_string=c("carat2", "carat2-carat"))
tableplot(d12, select_string=c("carat2", "carat2-carat", "cut2-cut"))

x <- c("carat2", "carat2-carat")


x <- c("carat2", "carat2- carat")

allColNames <- strsplit(x, "[ ]?-[ ]?")
colNames <- sapply(allColNames, function(x)x[1])
colNames2 <- sapply(allColNames, function(x)x[2])
