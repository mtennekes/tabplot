#' Loads a prepared object
#' 
#' Loads a prepared object that has been saved with \code{\link{savePrepare}}.
#' 
#' @param dir directory of the prepared object
#' @return the prepared object
#' @import ffbase
#' @export
loadPrepare <- function(dir) {
	tp <- NULL
	if (!isTRUE(file.exists(dir))) {
		stop("directory '", dir, "' does not exist")
	}
	load(file.path(dir, "/.Rdata"))
	tp
}

#' Saves a prepared object
#' 
#' Saves a prepared object that has been created by \code{\link{tablePrepare}}. If \code{\link{tablePrepare}} is called with \code{dir} specified, then it is already saved using this function.
#' 
#' @param tp the prepared object (created by \code{\link{tablePrepare}})
#' @param dir directory of the prepared object
#' @param overwrite logical. If \code{dir} already contains files of a prepared object, should they be overwritten?
#' @import ffbase
#' @export
savePrepare <- function(tp, dir, overwrite=FALSE) {
	#	tpname <- deparse(substitute(tp))
	
	dir <- file.path(dir)
	dir.create(dir, showWarnings = FALSE, recursive = TRUE)
	if (!overwrite && file.exists(file.path(dir, ".Rdata"))) {
		stop("Directory '", dir, "' contains existing '.Rdata' file. \n          To force saving use 'overwrite=TRUE'")
	}
	
	subdirs <- file.path(dir, attr(tp, "names"))
	if (!overwrite && any(file.exists(subdirs))) {
		stop("Subdirectories data and ordered already exist. \n          To force saving use 'overwrite=TRUE'")
	}
	
	data <- tp$data
	ordered <- tp$ordered
	save.ffdf(data, dir=subdirs[1], overwrite=overwrite) 
	save.ffdf(ordered, dir=subdirs[2], overwrite=overwrite)
	save(tp, file=file.path(dir, ".Rdata"))
}