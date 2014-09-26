library(ggplot2)
data(diamonds)

d1 <- diamonds[sample(nrow(diamonds), 1000), ]
d2 <- diamonds[sample(nrow(diamonds), 1000), ]

t1 <- tableplot(d1)
t2 <- tableplot(d2)

td <- t2 - t1
plot(td)

names(d2) <- paste0(names(d1), "2")


d12 <- cbind(d1, d2)

tableplot(d12)

tableplot(d12, select_string=c("carat2", "carat2-carat"))
tab <- tableplot(d12, select_string=c("carat2", "carat2-carat", "cut2-cut"))
tab <- tableplot(d12, select=c(carat2, carat2-carat, cut2-cut))

tab <- tableplot(d12, select=c(carat2, carat2-carat, cut2-cut), relative=TRUE)

