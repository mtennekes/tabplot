# load diamonds dataset from ggplot2:

# Not run:
	require(ggplot2)
	data(diamonds)
	
	# default tableplot
	tab <- tableplot(diamonds)
	
	# save tableplot
	tableSave(tab, filename="diamonds.png")
# End(Not run)
	
