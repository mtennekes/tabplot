if (require(ggplot2)){   
   diamondsNA <- diamonds
   is.na(diamondsNA$price) <- diamondsNA$cut == "Ideal"
}

irisNA <- iris
is.na(iris$Sepal.Width) <- sample(1:nrow(iris), 30)

tableGUI()