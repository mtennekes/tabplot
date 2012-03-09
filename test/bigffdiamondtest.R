library(ff)
options(fftempdir = "d:/temp")

library(ggplot2); data(diamonds)
n <- nrow(diamonds)
N <- 1000L * n

# Create data set
diamondsff <- as.ffdf(diamonds)
nrow(diamondsff) <- N

# fill with identical data
for (i in chunk(from=1, to=N, by=n)){
  diamondsff[i,] <- diamonds
}

library(tabplot)

# and timing
system.time(
  tab <- tableplot(diamondsff)
)
