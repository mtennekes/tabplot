library(knitr)

setwd('../vignette')
knit2pdf("tabplot-vignette.Rnw", "tabplot-vignette.tex")

unlink("./pgk/inst/doc/tabplot-vignette.pdf")
shell("copy tabplot-vignette.pdf ..\\pkg\\inst\\doc")
setwd('../build')
