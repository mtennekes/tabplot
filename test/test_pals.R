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

CBS <- c(rgb(255 / 255, 213 / 255,   7 / 255)
	   , rgb(177 / 255, 211 / 255,  72 / 255)
	   , rgb(249 / 255, 166 / 255,  28 / 255)
	   , rgb( 29 / 255, 188 / 255, 197 / 255)
	   , rgb(187 / 255, 133 / 255,  48 / 255)
	   , rgb(239 / 255,  65 / 255,  86 / 255)
	   , rgb(  0 / 255, 125 / 255, 182 / 255)
	   , rgb(128 / 255,  42 / 255, 142 / 255))
	
tableplot(diamonds, pals=list(CBS))


tableplot(diamonds, pals=list("CBL"))
tableplot(diamonds, pals=list("HCL1", "HCL2", "HCL3"))
tableplot(diamonds, colNames=c("carat", "cut", "color", "clarity", "cut", "color", "clarity"), pals=list("Set1", "Set2", "Set3", "Set4", "Set5", "Set6"))