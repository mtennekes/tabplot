library(devtools)

load_all("pkg")

library(ggplot2)



build("pkg")
check("pkg")
