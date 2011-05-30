data(iris)
irisNA <- iris

# simulate missing data
is.na(irisNA$Sepal.Width) <- sample(1:nrow(iris), 30)
is.na(irisNA$Species) <- sample(1:nrow(iris), 15)

library(ff)
irisNA <- as.ffdf(irisNA)
tableplot(irisNA)


#now a bigger example, blow the data set up to 600,000 records
cat("Constructing big iris ffdf dataset...")
dup <- function(ffx, power){
	len <- nrow(ffx)
	nrow(ffx) <- 2^(power) * len
	while (2*len <= nrow(ffx)){
		for (i in chunk(ffx, from=len+1, to=2*len)){
		   ffx[i,] <- ffx[i-len,]
		}
		len <- 2*len
	}
	ffx
}
irisNA <- dup(irisNA, 12)
cat(": number of rows=", nrow(irisNA), "\n")
cat("Make a tableplot...")
#and plot
tableplot(irisNA)
cat("\n")
