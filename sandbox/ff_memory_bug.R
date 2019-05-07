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

