library(devtools)
load_all("../pkg")

library(gWidgets)
library(gWidgetsWWW)

install.packages("gWidgetsWWW2", repos="http://R-Forge.R-project.org")


library(gWidgetsWWW2)


localServerOpen("buildGUI.R", pacakge="gWidgetsWWW")

require(grid)
vp <- viewport(width=unit(4, "inches"), height=unit(2, "inches"))
pushViewport(vp)
tableplot(iris)


