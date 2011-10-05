\dontrun{
	# load diamonds dataset from ggplot2
	require(ggplot2)
	data(diamonds)
	
	# default tableplot
	tab <- tableplot(diamonds)
	
	# save tableplot
	tableSave(tab, filename="diamonds.png")
}