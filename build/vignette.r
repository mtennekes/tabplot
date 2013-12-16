library(knitr)

setwd('../vignette')
knit2pdf("tabplot-vignette.Rnw", "tabplot-vignette.tex")
knit2pdf("tabplot-timings.Rnw", "tabplot-timings.tex")

unlink("./pgk/vignettes/tabplot-vignette.pdf")
unlink("./pgk/vignettes/tabplot-timings.pdf")
shell("copy tabplot-vignette.pdf ..\\pkg\\vignettes")
shell("copy tabplot-timings.pdf ..\\pkg\\vignettes")
setwd('../build')
