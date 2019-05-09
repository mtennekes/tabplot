library(ff)
library(ggplot2)
data(diamonds)


# get ff temp dir
tmp <- options()$fftempdir

# function to count the number of ff files 
count_ff_files <- function() length(list.files(path = tmp, pattern = "\\.ff$"))	


count_ff_files() # returns 0

# create tableplot
tableplot(diamonds)

count_ff_files() # returns 21

gc()

count_ff_files() # returns 0


# create 'prepare' object (list of 2 ffdfs)
p <- tablePrepare(diamonds)

count_ff_files() # returns 20

close(p)

count_ff_files() # returns 20

gc()

count_ff_files() # returns 20

rm(p)
gc()

count_ff_files() # returns 0


# create 'prepare' object again
p <- tablePrepare(diamonds)

tableplot(p)

count_ff_files() # returns 20

close(p) # returns FALSE (why?)

count_ff_files() # returns 20

gc()

count_ff_files() # returns 20

rm(p)
gc()

count_ff_files() # returns 0




library(ff)
test <- function(){
	x <- ff(1:10)
	print(list.files(pattern = "ff", options()$fftempdir, full.names = TRUE))
	close(x)
	return(TRUE)
}
test()
list.files(pattern = "ff", options()$fftempdir, full.names = TRUE)
gc()
list.files(pattern = "ff", options()$fftempdir, full.names = TRUE)



require(ff)
x <- list()
for(i in 1:100000){
	print(i)
	x[[i]] <- ff(rnorm(10))
	open(x[[i]] )
	#close(x[[i]])
}


library(ff)
list.files(pattern = "ff", options()$fftempdir, full.names = TRUE)
x <- ff(1:10)

close(x)

list.files(pattern = "ff", options()$fftempdir, full.names = TRUE)

y <- x
x





