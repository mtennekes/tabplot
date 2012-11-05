subset_data <- function(p, cols, subset_string, sortCol) {
	# can be made a lot shorter
	s_data <- p$data[cols]
	s_ordered <- p$ordered[cols]
	if (!missing(subset_string)) {
		i <- ffwhich(p$data, parse(text=subset_string))
		s_data <- s_data[i,]
		
		# HACK for the moment (other ordered indices are not ok)
		nrow(s_ordered) <- length(i)
		s_ordered[[sortCol]] <- fforder(s_data[[sortCol]])
	}
	
	structure(
		list( data = s_data
			, ordered = s_ordered
		    )
		, class="prepared"
	)
}