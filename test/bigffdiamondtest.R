library(ffbase)
options(fftempdir = "d:/temp")

library(ggplot2); data(diamonds)

## create missings, and high cardinality categorical variables
diamonds$carat[sample.int(nrow(diamonds),4000)] <- NA
diamonds$cut[sample.int(nrow(diamonds),20000)] <- NA

diamonds$carat2 <- factor(diamonds$carat)
diamonds$price2 <- factor(diamonds$price)

diamonds$expensive <- diamonds$price >= 10000

# multiply x times and store as ffdf
n <- nrow(diamonds)
N <- 10L * n

diamondsff <- as.ffdf(diamonds)
nrow(diamondsff) <- N

for (i in chunk(from=1, to=N, by=n)) diamondsff[i,] <- diamonds


## timings:
system.time(
  tab <- tablePrepare(diamondsff)
)
tableplot(tab, decreasing=T)
tableplot(tab, decreasing=F)

tab2 <- tablePrepare(diamonds)
tableplot(tab2, decreasing=T)
tableplot(tab2, decreasing=F)



system.time(
	t <- tableplot(tab, maxN=1e4, plot=FALSE)
)
plot(t)

system.time(
	t <- tableplot(tab, maxN=1e5, plot=FALSE)
)
plot(t)

system.time(
	t <- tableplot(tab, maxN=1e5, plot=FALSE)
)
plot(t)

system.time(
	t <- tableplot(tab, maxN=nrow(diamondsff)-1, plot=FALSE)
)
plot(t)


system.time(
	t <- tableplot(tab, maxN=0, plot=FALSE)
)
plot(t)
