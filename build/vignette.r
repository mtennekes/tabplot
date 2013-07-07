library(knitr)

setwd('../vignette')
knit2pdf("tabplot-vignette.Rnw", "tabplot-vignette.tex")
knit2pdf("tabplot-timings.Rnw", "tabplot-timings.tex")

unlink("./pgk/inst/doc/tabplot-vignette.pdf")
unlink("./pgk/inst/doc/tabplot-timings.pdf")
shell("copy tabplot-vignette.pdf ..\\pkg\\inst\\doc")
shell("copy tabplot-timings.pdf ..\\pkg\\inst\\doc")
setwd('../build')
