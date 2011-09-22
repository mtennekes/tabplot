library(devtools)

load_all("pkg")

library(ggplot2)

data(diamonds)
tableplot(diamonds)

build("pkg")
check("pkg")
