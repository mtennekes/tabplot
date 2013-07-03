require(devtools)

## Create palletes, vignette and documentation...
setwd("../build")
source("createpalette.R")
source("vignette.R")
source("roxygen.R")
setwd("../pkg")


++++++## check
check(doc_clean=FALSE) 

	
unlink( '../output', TRUE)
dir.create("../output", showWarnings=FALSE)
build(path= "../output")
