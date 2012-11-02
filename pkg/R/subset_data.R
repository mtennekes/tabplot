subset_data <- function(p, cols, subset_string, sortCol) {
	x <- p$data[cols]
	o <- p$ordered[[sortCol]]
	if (!missing(subset_string)) {
		e <- parse(text=subset_string)
		r <- bit(nrow(x))
		for (i in chunk(x)) {
			log <- eval(e, x[i,])
			r[i] <- log & !is.na(log)
		}
		x <- subset.ffdf(x, r)
		o <- fforder(x[[sortCol]])
	}
	
	listo <- lapply(p$ordered, function(o) NULL)
	listo[[sortCol]] <- o
	
	structure(
		list( data = x
			  , ordered = listo
		)
		, class="prepared"
	)
}