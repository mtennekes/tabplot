library(data.table)

bin_data_df <- function(p, sortCol=1L, cols=seq_along(p$data), from=0, to=1
					   , nbins=100L
   				       , decreasing = FALSE
){
	cols <- names(p)[]
	p <- as.data.table(p)
	sort_col <- cols[sortCol]
	
	setorderv(p, sort_col, ifelse(decreasing, -1, 1))
	is_numeric <- sapply(p, is.numeric)
	num_cols <- cols[is_numeric]
	other_cols <- cols[!is_numeric]
	# add bin column (TODO, descreasing)
	
	f <- p[, (.I - 1)/.N]
	p <- p[ f >= from & f <= to, ]
	nbins <- min(nbins, nrow(p))
	bin <- p[, 1L + as.integer(nbins * (.I - 1)/.N)]
	
	num_means <- p[, lapply(.SD, mean, na.rm = TRUE), .SDcols=num_cols, by = bin]
	num_sd <- p[, lapply(.SD, sd, na.rm = TRUE), .SDcols=num_cols, by = bin]
	num_na <- p[, lapply(.SD, function(x) {mean(is.na(x))}), .SDcols=num_cols, by = bin]
	num_count <- p[, lapply(.SD, function(x) {sum(!is.na(x))}), .SDcols=num_cols, by = bin]
	
	# list in same order as data.frame
	res <- sapply(cols, function(x){NULL})
	
	res[num_cols] <- 
		sapply(num_cols, function(num_col){
			cbind( count    = num_count[[num_col]]
				 , mean     = num_means[[num_col]]
				 , sd       = num_sd[[num_col]]
				 , complete = 1 - num_na[[num_col]]
			     )
		}, simplify = FALSE
		)
	
	res[other_cols] <- 
		sapply(other_cols, function(x){
			tab <- table(bin, p[[x]], useNA = "always")
			tab[1:nbins,]
		}, simplify = FALSE
		)
	res
}


data("diamonds", package="ggplot2")
system.time({
	bd <- bin_data_df(diamonds)
})

bd
