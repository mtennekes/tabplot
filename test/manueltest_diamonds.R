require(ggplot2)
data(diamonds)
## add some NA's
is.na(diamonds$price) <- diamonds$cut=="Ideal"
is.na(diamonds$cut) <- (runif(nrow(diamonds))>0.8)

tableplot(diamonds)

tableplot(diamonds, colNames=c("carat", "price", "cut", "color", "clarity"), sortCol="price")

tableplot(diamonds, colNames=c("carat", "price", "cut", "color", "clarity"), sortCol="price", from=0, to=5)

tableplot(diamonds, pals=list(7, "col_blind_friendly(2)", rainbow(8)))


diamonds <- transform(diamonds, pricecarat=price/carat)
diamonds$ppc <- num2fac(diamonds$pricecarat)

diamonds$p <- num2fac(diamonds$price)

tableplot(diamonds, colNames=c("carat", "price", "pricecarat", "p", "ppc"), sortCol="carat")