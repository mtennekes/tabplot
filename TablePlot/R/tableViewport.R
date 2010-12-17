tableViewport <- function(n, from, to) {
	#############################
	## Make row selection of the data
	#############################
	
	# make rowselection of the data
	iFrom <- round(n * from * 0.01) + 1 
	iTo <- round(n * to * 0.01)
	m <- (iTo - iFrom) + 1

	# does selection consist of at least two rows?
	if (m < 2) stop("Selection does not consist of at least two rows")

	return(list(m=m, iFrom=iFrom, iTo=iTo))
}
	