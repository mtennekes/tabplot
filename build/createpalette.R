## this script creates a list of qualitative palettes
require(RColorBrewer)
require(colorspace)
require(dichromat)
require(ggplot2)

######################################################
####### ColorBrewer palettes
######################################################
brewerInfo <- brewer.pal.info[, 1, drop=FALSE]
brewerInfo$name <- row.names(brewerInfo)
brewerInfo <- brewerInfo[,c(2,1)]


brewer <- mapply(FUN=function(x,y){brewer.pal(y, x)}, brewerInfo$name, brewerInfo$maxcolors)


hcl24 <- rainbow_hcl(24, start = 0 + 15, end=360*(23/24) + 15, l=65, c=100)

getHcl <- function(n, start) {
	sq <- round(seq(start, 24 + start, length.out=n+1))[1:n]
	sq[sq>24] <- sq[sq>24] - 24
	return(hcl24[sq])
}


tabplotPalettes <- list(
	div = list(
		OrBu = c(brewer$PuOr[1:6], brewer$RdBu[7:11]),
		OrPu = brewer$PuOr,
		PRGn = brewer$PRGn,
		BrBG = brewer$BrBG,
		RdBu = brewer$RdBu,
		RdYlBu = brewer$RdYlBu,
		RdYlGn = brewer$RdYlGn),
	qual = list(
		Set1 = c("#D55E00", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2","#CC79A7"),
		Set2=brewer$Set1[-1], 
		Set3=brewer$Dark2,
		Set4=brewer$Accent[c(1,5,2,6,3,7,4,8)],
		Set5=brewer$Set2[c(1:6,8,7)],
		Set6=brewer$Set3[c(1:4, 9, 5:8, 12, 9:11)],
		Set7=colorschemes$Categorical.12[c(5, 2, 7, 4, 9, 6, 11, 8, 1, 10, 3)],
		Set8= rgb(red=c(92, 92, 255, 170, 255, 255, 145, 193, 92, 201, 255, 173, 227, 226, 204, 87),
				  green=c(107, 203, 177, 97, 255, 137, 101, 193, 229, 255, 224, 45, 196, 212, 241, 142),
				  blue=c(247, 92, 17, 187, 95, 235, 62, 193, 214, 135, 204, 92, 239, 149, 255, 82),
				  maxColorValue=255), # second color (255, 89, 89) left out because of NA color
		Paired=brewer$Paired[c(1:4, 7:12)],
		HCL1 = getHcl(n=8, start=10)[c(1, 4, 7, 2, 5, 8, 3, 6)],
		HCL2 = getHcl(n=8, start=17)[c(1, 4, 7, 2, 5, 8, 3, 6)],
		HCL3 = getHcl(n=8, start=24)[c(1, 4, 7, 2, 5, 8, 3, 6)]))

	
	
save("tabplotPalettes", file="../pkg/R/sysdata.rda")

