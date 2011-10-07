library(devtools)

load_all("pkg")

library(ggplot2)


tableplot(diamonds)


build("pkg")
check("pkg")

document("pkg")



