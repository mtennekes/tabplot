if (require(ggplot2)){   
   diamondsNA <- diamonds
   # simulate missing data
   is.na(diamondsNA$price) <- diamondsNA$cut == "Ideal"

   tableplot(diamondsNA)
}

irisNA <- iris
# simulate missing data
is.na(irisNA$Sepal.Width) <- sample(1:nrow(iris), 30)
is.na(irisNA$Species) <- sample(1:nrow(iris), 15)

tableplot(irisNA)
