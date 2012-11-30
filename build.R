require(devtools)

## Create palletes and vignette...
setwd("../")
source("./build/createpalette.R")
source("./build/vignette.R")
setwd("./pkg")

## documentation
source("../roxygen.R")

## check
if (check()) {
	unlink( '../output', TRUE)
	dir.create("../output", showWarnings=FALSE)
	build(path= "../output")
}
