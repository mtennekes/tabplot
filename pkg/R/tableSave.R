#' Save a tableplot
#'
#' Save a tableplot in pdf, eps, svg, wmf, png, jpg, bmp, or tiff format.
#'
#' @aliases tableSave
#' @param tab a \link{tabplot-object}
#' @param filename filename with extention (pdf, eps, svg, wmf, png, jpg, bmp, or tiff)
#' @param device device, automatically extracted from filename extension 
#' @param path path to save to
#' @param scale scaling factor
#' @param width width (in inches)
#' @param height height (in inches)
#' @param dpi dpi to use for raster graphics
#' @param fontsize the (maximum) fontsize
#' @param legend.lines the number of lines preserved for the legend
#' @param title title of the plot (shown if \code{showTitle==TRUE})
#' @param showTitle show the title
#' @param ... other arguments passed to graphics device
#' @export
#' @keywords save tableplot
#' @example ../examples/tableSave.R
tableSave <- function (tab, filename = paste(tab$dataset, ".pdf", sep = ""), 
    device = default_device(filename), path = NULL, scale = 1, 
    width = par("din")[1], height = par("din")[2], dpi = 300, 
	fontsize = 8, legend.lines = 8, title = tab$dataset, showTitle = FALSE, ...) 
{
    if (class(tab) != "tabplot") 
        stop("plot should be a tabplot object")
    
    pdf <- function(..., version = "1.4") grDevices::pdf(..., 
    													 version = version)
    eps <- ps <- function(..., width, height) grDevices::postscript(..., 
        width = width, height = height, horizontal=FALSE,
        paper = "special")
    svg <- function(...) grDevices::svg(...) 
    wmf <- function(..., width, height) grDevices::win.metafile(..., 
        width = width, height = height)
    png <- function(..., width, height) grDevices::png(..., width = width, 
        height = height, res = dpi, units = "in")
    jpg <- jpeg <- function(..., width, height) grDevices::jpeg(..., 
        width = width, height = height, res = dpi, units = "in")
    bmp <- function(..., width, height) grDevices::bmp(..., width = width, 
        height = height, res = dpi, units = "in")
    tiff <- function(..., width, height) grDevices::tiff(..., 
        width = width, height = height, res = dpi, units = "in")
    default_name <- function(tab) {
        paste(tab$dataset, ".pdf", sep = "")
    }
    default_device <- function(filename) {
        pieces <- strsplit(filename, "\\.")[[1]]
        ext <- tolower(pieces[length(pieces)])
        if (!(ext %in% c("eps", "pdf", "svg", "wmf", "png", "jpg", "jpeg", "bmp", "tiff"))) 
        	stop("missing or unknown extension")
        match.fun(ext)
    }
    if (missing(width) || missing(height)) {
        message("Saving ", prettyNum(width * scale, digits = 3), 
            "\" x ", prettyNum(height * scale, digits = 3), "\" tableplot")
    }
    width <- width * scale
    height <- height * scale
    if (!is.null(path)) {
        filename <- file.path(path, filename)
    }
    device(file = filename, width = width, height = height, ...)
    plot(tab, fontsize = fontsize, legend.lines = legend.lines, title = title, showTitle = showTitle)
    on.exit(capture.output(dev.off()))
    invisible()
}