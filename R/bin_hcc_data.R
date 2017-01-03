#' Bin high cardinality data
#' 
#' @param bd binned dataset (result of \code{\link{bin_data}})
#' @param max_levels maximum number of levels. Each column in \code{bd} that has more than \code{max_levels} categories is rebinned to \code{max_levels} categories.
#' @export
bin_hcc_data <- 
function(bd, max_levels) {
	nlev <- max_levels + 1 #including NA
	nRow <- nrow(bd[[1]])
	bdF <- lapply(bd, function(col){
		nCol <- ncol(col)
		# check whether nCol, the number is categories ('na' excluded), exceeds max_levels
		# numerical variables are out of scope since nCol = 3 << max_levels
		if (nCol > max_levels) {
			mapping <- as.numeric(cut(seq.int(nCol-1), breaks=max_levels))
			mapping_na <- c(mapping, nlev)
			
			to <- c(which(mapping[-(nCol-1)] - mapping[-1]!=0), (nCol-1))
			from <- c(0, to[-max_levels]) + 1
			lvls <- colnames(col)[-ncol(col)]
			new_lvls <- ifelse(from==to, 
							   lvls[from], 
							   paste0(lvls[from], "...", lvls[to]))
		
			ffcol <- ff(col, vmode="double")
			ffmapping <- ff(rep(seq(from=0, by=nlev, length.out=nRow), times=nCol) + rep(mapping_na, each=nRow), vmode="integer")
			
			temp <- binned_sum.ff(ffcol, ffmapping, nRow * nlev)
			
			col2 <- t(matrix(temp[,2], nrow=nlev, 
						   dimnames=list(c(new_lvls, "<NA>"),rownames(col))))

			col2
		} else col
	})
}