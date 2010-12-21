data(iris)
irisNA <- iris
# simulate missing data
is.na(irisNA$Sepal.Width) <- sample(1:nrow(iris), 30)
is.na(irisNA$Species) <- sample(1:nrow(iris), 15)

library(ff)
irisNA <- as.ffdf(irisNA)
tableplot(irisNA)

#tableplot(diamondsNA)