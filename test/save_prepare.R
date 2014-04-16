library(ffbase)
options(fftempdir = "d:/temp")

library(ggplot2); data(diamonds)

## create missings, and high cardinality categorical variables
# diamonds$carat[sample.int(nrow(diamonds),4000)] <- NA
# diamonds$cut[sample.int(nrow(diamonds),20000)] <- NA
# 
# diamonds$carat2 <- factor(diamonds$carat)
# diamonds$price2 <- factor(diamonds$price)
# 
# diamonds$expensive <- diamonds$price >= 10000

# multiply x times and store as ffdf
n <- nrow(diamonds)
N <- 20L * n

diamondsff <- as.ffdf(diamonds)
nrow(diamondsff) <- N

for (i in chunk(from=1, to=N, by=n)) diamondsff[i,] <- diamonds

save.ffdf(diamondsff, dir="d:/temp2/d")
load.ffdf()


tp <- tablePrepare(diamondsff)

tableplot(tp)


savePrepare(tp, dir="d:/temp2/d2",overwrite=TRUE)
tp2 <- loadPrepare("d:/temp2/d2")


load("d:/temp2/diamonds4/tp.Rdata")

loadPrepare <- function(dir) {
	if (!isTRUE(file.exists(dir))) {
		stop("directory '", dir, "' does not exist")
	}
	load(file.path(dir, "/.Rdata"))
	tp
}

savePrepare <- function(tp, dir, overwrite=FALSE) {
#	tpname <- deparse(substitute(tp))

	dir <- file.path(dir)
	dir.create(dir, showWarnings = FALSE, recursive = TRUE)
	
	if (!isTRUE(overwrite) && file.exists(file.path(dir, ".Rdata"))) {
		stop("Directory '", dir, "' contains existing '.Rdata' file. \n          To force saving use 'overwrite=TRUE'")
	}
	
	a <- attributes(tp)
	
	name <- a$name
	dirs <- file.path(dir, a$names)
	
	data <- tp$data
	ordered <- tp$ordered
	save.ffdf(data, dir=dirs[1], overwrite=overwrite) 
	save.ffdf(ordered, dir=dirs[2], overwrite=overwrite)
	save(tp, file=file.path(dir, ".Rdata"))
}

