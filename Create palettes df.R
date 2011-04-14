## this script creates a list of qualitative palettes
require(RColorBrewer)
require(colorspace)

require(ggplot2)

######################################################
####### ColorBrewer palettes
######################################################
brewerInfo <- brewer.pal.info[brewer.pal.info$category=="qual", 1, drop=FALSE]
brewerInfo$name <- row.names(brewerInfo)
brewerInfo <- brewerInfo[,c(2,1)]


brewer <- mapply(FUN=function(x,y){brewer.pal(y, x)}, brewerInfo$name, brewerInfo$maxcolors)
# remove color red (needed for NA's)
brewer$Set1 <- brewer$Set1[-1]

brewer <- c(list(default=c(brewer$Set1, brewer$Set2)), brewer)


######################################################
####### colorspace HCL palettes
######################################################
df <- data.frame(x=letters[1:12])
p <- qplot(data=df, x, 1, geom="bar", stat="identity", fill=x) +
opts(
legend.position = "none",
axis.title.x = theme_blank(),
axis.title.y = theme_blank(),
axis.text.x = theme_blank(),
axis.text.y = theme_blank(),
axis.ticks = theme_blank(),
title="Default colours")

# hcl5 <- rainbow_hcl(5, start = 60, end = 300)
# p %+% df + scale_fill_manual (values=hcl5) + opts(title="dynamic")

# hcl8 <- rainbow_hcl(8, start = 45, end = 300)
# p %+% df + scale_fill_manual (values=hcl8) + opts(title="dynamic")


hclCircle <- function(n, start=0, step=10) {
	rainbow_hcl(n, start = start, end = start+step*(n-1), c=80, l=70)
}

#from http://jfly.iam.u-tokyo.ac.jp/color/ and  http://wiki.stdout.org/rcookbook/Graphs/Colors%20%28ggplot2%29
col_blind_friendly = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p %+% df + scale_fill_manual (values=col_blind_friendly) + opts(title="dynamic")


hcl5 <- hclCircle(5, start = 60, step = 144)
p %+% df + scale_fill_manual (values=hcl5) + opts(title="dynamic")



hcl8 <- hclCircle(8, start=60, step= 135)
#c(rainbow_hcl(8, start = 0, end = (360/8)*3*7))
p %+% df + scale_fill_manual (values=hcl8) + opts(title="dynamic")


hcl12 <- hclCircle(12, start = 45, step = 150)
p %+% df + scale_fill_manual (values=hcl12) + opts(title="dynamic")

otherPals <- list(hcl5=hcl5, hcl8=hcl8, hcl12=hcl12, col_blind_friendly=col_blind_friendly)

######################################################
####### combine palettes
######################################################

tabplotPalettes <- c(brewer, otherPals)
save("tabplotPalettes", file="/tabplot/data/tabplotPalettes.Rdata")


