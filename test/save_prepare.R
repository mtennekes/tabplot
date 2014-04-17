library(ffbase)
options(fftempdir = "d:/temp")

library(ggplot2); data(diamonds)

## create missings, and high cardinality categorical variables
# diamonds$carat[sample.int(nrow(diamonds),4000)] <- NA
# diamonds$cut[sample.int(nrow(diamonds),20000)] <- NA
# 
# diamonds$carat2 <- factor(diamonds$carat)
# diamonds$price2 <- factor(diamonds$price)
# 
# diamonds$expensive <- diamonds$price >= 10000

# multiply x times and store as ffdf
n <- nrow(diamonds)
N <- 20L * n

diamondsff <- as.ffdf(diamonds)
nrow(diamondsff) <- N

for (i in chunk(from=1, to=N, by=n)) diamondsff[i,] <- diamonds

save.ffdf(diamondsff, dir="d:/temp2/diamonds", overwrite=TRUE)


save.ffdf(diamondsff, dir="d:/temp2/diamonds2", overwrite=TRUE)


tp <- tablePrepare(diamondsff, dir="d:/temp2/diamonds_prepare")
savePrepare(tp, dir="d:/temp2/diamonds_prepare", overwrite=T)



tableplot(tp, subset=cut=='Fair')
tableplot(tp, subset=price < 5000 & cut=='Ideal')

savePrepare(tp, dir="d:/temp2/diamonds_prepare2", overwrite=TRUE)
tp2 <- loadPrepare("d:/temp2/d2")


load("d:/temp2/diamonds4/tp.Rdata")



