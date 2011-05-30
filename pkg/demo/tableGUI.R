if (require(ggplot2)){   
   # load diamonds dataset from ggplot2 and add NA's
   diamondsNA <- diamonds
   is.na(diamondsNA$price) <- diamondsNA$cut == "Ideal"
}

# load iris dataset and add NA's 
irisNA <- iris
is.na(iris$Sepal.Width) <- sample(1:nrow(iris), 30)

# start GUI 
tableGUI()