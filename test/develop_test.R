
### test installed package
library(tabplot)

### test
library(ggplot2)
data(diamonds)
str(diamonds)

diamonds$carat[sample.int(nrow(diamonds),4000)] <- NA
diamonds$cut[sample.int(nrow(diamonds),20000)] <- NA

diamonds$carat2 <- factor(diamonds$carat)
diamonds$price2 <- factor(diamonds$price)

diamonds$expensive <- diamonds$price >= 10000


p <- tablePrepare(diamonds)
tableplot(p, decreasing=TRUE)

dFF <- as.ffdf(diamonds)
pFF <- tablePrepare(dFF)
tableplot(pFF, decreasing=TRUE)



tableplot(p, maxN=1e2, from=40, to=45)


tab <- tableplot(diamonds)
tab <- tableplot(diamonds, max_levels=20, change_palette_type_at=15, max_print_levels=15) # default values

tab <- tableplot(diamonds, max_levels=100, change_palette_type_at=15, max_print_levels=12, legend.lines=12)


tab <- tableplot(diamonds, subset=cut)
tab <- tableplot(diamonds, subset=cut, limitsX=list(price=c(0,20000)))


tab <- tableplot(diamonds, sortCol=depth, nCols=6, li mitsX=list(price=c(0,20000)))
tab <- tableplot(diamonds, sortCol=3, subset_string="cut=='Fair'")

tab <- tableplot(diamonds, sortCol=3, subset=cut, plot=FALSE)


tableplot(diamonds, select=c(price,carat,color,cut,clarity,table,z,y,x), 
		  pals=list(cut="Set3(4)", color="Greens", clarity="Paired(4)"),
		  numPals=c(x="Greys", y="Greens"),
		  limitsX=list(carat=c(0, 5), table=c(50,60)),
		  scales=c(carat="lin", price="lin"))

tableplot(diamonds, select=c(carat, price))


p <- prepare(diamonds)
tableplot(p, select=c(y,x), sortCol=x)

tableplot(p, from=0, to=75)
tableplot(p, from=0, to=77)


diamonds$price <- diamonds$price * 1e6
diamonds$table <- diamonds$table + 1e4
diamonds$x <- diamonds$x / 1e4
diamonds$y <- diamonds$y / 1e4
diamonds$z <- diamonds$z * -1e7 -1e8
tableplot(diamonds, scales=c(price="lin"))


