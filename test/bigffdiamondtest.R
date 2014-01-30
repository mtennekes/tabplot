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
N <- 2000L * n

diamondsff <- as.ffdf(diamonds)
nrow(diamondsff) <- N

for (i in chunk(from=1, to=N, by=n)) diamondsff[i,] <- diamonds


## timings:
system.time(
  tab <- tablePrepare(diamondsff)
)

## reference: full dataset
system.time(
	t2 <- tableplot(tab, sortCol=depth, sample=FALSE, decreasing=FALSE, sampleBinSize=1e3, plot=FALSE)
)
plot(t2)

## timings of sample
system.time(
	t <- tableplot(tab, sortCol=1, sample=TRUE, sampleBinSize=1e3, plot=FALSE)
)
plot(t)


## test Mchuck_approx
for (i in 1:10) {
system.time(
	t <- tableplot(tab, sortCol=i, sample=TRUE, sampleBinSize=1.3e3, plot=FALSE)
)}

