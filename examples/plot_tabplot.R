# load diamonds dataset from ggplot2
data(diamonds, package="ggplot2")

tab <- tableplot(diamonds)
plot(tab, title="Shine on you Crazy Diamond!!!",
	 fontsize=12,
	 legend.lines=7,
	 fontsize.title=16)

