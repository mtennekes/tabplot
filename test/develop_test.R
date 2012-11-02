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
tab <- tableplot(diamonds, max_levels=20, change_palette_type_at=15, max_print_levels=15) # default values

tab <- tableplot(diamonds, max_levels=50, change_palette_type_at=10, max_print_levels=12, legend.lines=12)


diamonds30 <- diamonds[1:30,]
tab30 <- tableplot(diamonds30, sortCol=1, subset=cut=="Premium", nbins=3)

tab <- tableplot(diamonds, sortCol=3, subset=cut=="Fair")



library(ff)
dFF <- as.ffdf(diamonds)
tableplot(dFF)


Rprof(tmp <- tempfile())
Rprof(); summaryRprof(tmp); unlink(tmp)

