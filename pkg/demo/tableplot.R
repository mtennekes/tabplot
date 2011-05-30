if (require(ggplot2)){   
	# simulate missing data
	diamondsNA <- diamonds
	is.na(diamondsNA$price) <- diamondsNA$cut == "Ideal"
	
	tableplot(diamondsNA)
	
	# correlation between carat and price 
	diamondsNA <-	transform(diamondsNA, pricePerCarat = price/carat, carat_cat= num2fac(carat))
	tableplot(diamondsNA, colNames=c("carat", "carat_cat", "cut", "price", "pricePerCarat"))
	
	# closer look at the top 5 percent most expensive diamonds
	tableplot(diamondsNA, colNames=c("carat", "carat_cat", "cut", "color", "clarity", "price", "pricePerCarat"), sortCol="price", from=0, to=5)
	
}

irisNA <- iris
# simulate missing data
is.na(irisNA$Sepal.Width) <- sample(1:nrow(iris), 30)
is.na(irisNA$Species) <- sample(1:nrow(iris), 15)

tableplot(irisNA)
