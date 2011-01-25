if (require(ggplot2)){   
   diamondsNA <- diamonds
   # simulate missing data
   is.na(diamondsNA$price) <- diamondsNA$cut == "Ideal"

   tableplot(diamondsNA)
   
   # add categorical price variable
   diamondsNA$price5 <- num2fac(diamondsNA$price, method="kmeans", num_scale="log", n=5)
   diamondsNA <- diamondsNA[c(1:7,11,8:10)]
   
   tableplot(diamondsNA)

   # zoom in
   tableplot(diamondsNA, scales="lin", from = 0, to = 10)
}

irisNA <- iris
# simulate missing data
is.na(irisNA$Sepal.Width) <- sample(1:nrow(iris), 30)
is.na(irisNA$Species) <- sample(1:nrow(iris), 15)

tableplot(irisNA)
