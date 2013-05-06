library(ffbase)
options(fftempdir = "d:/temp")

library(ggplot2); data(diamonds)

diamonds$carat[sample.int(nrow(diamonds),4000)] <- NA
diamonds$cut[sample.int(nrow(diamonds),20000)] <- NA

diamonds$carat2 <- factor(diamonds$carat)
diamonds$price2 <- factor(diamonds$price)

diamonds$expensive <- diamonds$price >= 10000



n <- nrow(diamonds)
N <- 100L * n

# Create data set
diamondsff <- as.ffdf(diamonds)
nrow(diamondsff) <- N

# fill with identical data
for (i in chunk(from=1, to=N, by=n)){
  diamondsff[i,] <- diamonds
}

#library(tabplot)

# and timing
system.time(
  tab <- tablePrepare(diamondsff)
)


system.time(
	tableplot(tab, maxN=1e4, plot=FALSE)
)

system.time(
	tableplot(tab, maxN=1e5, plot=FALSE)
)

system.time(
	tableplot(tab, maxN=1e6, plot=FALSE)
)

system.time(
	tableplot(tab, maxN=0, plot=FALSE)
)
