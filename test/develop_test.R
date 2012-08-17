load_all()

### test installed package
library(tabplot)
library(tabplotGTK)


### test
library(ggplot2)
data(diamonds)
str(diamonds)

diamonds$carat[sample.int(nrow(diamonds),1000)] <- NA
diamonds$cut[sample.int(nrow(diamonds),2000)] <- NA

diamonds$carat2 <- factor(diamonds$carat)
diamonds$price2 <- factor(diamonds$price)

diamonds$expensive <- diamonds$price >= 10000

tab <- tableplot(diamonds)
tab <- tableplot(diamonds, max_levels=50, recycle_palette=20, max_print_levels=15) # default values

tab <- tableplot(diamonds, max_levels=50, recycle_palette=10, max_print_levels=5, legend.lines=16)



library(ff)
dFF <- as.ffdf(diamonds)
tableplot(dFF)


Rprof(tmp <- tempfile())
Rprof(); summaryRprof(tmp); unlink(tmp)

