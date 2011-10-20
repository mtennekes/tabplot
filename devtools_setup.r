library(devtools)

load_all("pkg")


build("pkg")
check("pkg")

document("pkg")

build_vignettes("pkg")



