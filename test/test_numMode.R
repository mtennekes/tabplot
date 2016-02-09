library(ggplot2)
data(diamonds)

# add some NA's and derive negative x
diamonds[sample(which(diamonds$cut=="Ideal" | diamonds$carat>1), 10000), c("clarity", "x", "y", "table")] <- NA
diamonds$x_neg <- -diamonds$x

# old mode: bar with missing values are brighter
tableplot(diamonds, numMode="MB-ML")

# mean bars with sd bars and mean lines
tableplot(diamonds, numMode="mb-sdb-ml")

# sd bars and mean lines
tableplot(diamonds, numMode="sdb-ml")

# mean bars with sd lines and mean lines
tableplot(diamonds, numMode="mb-sdl-ml")
