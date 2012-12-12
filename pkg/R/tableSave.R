#' Save a tableplot
#'
#' Save a tableplot in pdf, eps, svg, wmf, png, jpg, bmp, or tiff format.
#'
#' @aliases tableSave
#' @param tab a \link{tabplot-object}, or a list of \link{tabplot-object}s, which are either stacked horizontally or put on multiple pages (for pdf only)
#' @param filename filename with extention (pdf, eps, svg, wmf, png, jpg, bmp, or tiff)
#' @param device device, automatically extracted from filename extension 
#' @param path path to save to
#' @param scale scaling factor
#' @param width width (in inches)
#' @param height height (in inches)
#' @param dpi dpi to use for raster graphics
#' @param onePage if true, multiple tab objects are stacked horizontally, else they are printed on multiple pages
#' @param ... other arguments passed to graphics device
#' @export
#' @keywords save tableplot
#' @example ../examples/tableSave.R
tableSave <- function (tab, filename = paste(tab$dataset, ".pdf", sep = ""), 
    device = default_device(filename), path = NULL, scale = 1, 
    width = par("din")[1], height = par("din")[2], dpi = 300, 
	onePage = TRUE,...) 
{
    if (is.list(tab) && !inherits(tab, "tabplot")) {
        if (!all(sapply(tab, class)=="tabplot")) stop(paste(deparse(substitute(tab)), "is not a list of tabplot-objects"))
    } else {
        if (!inherits(tab, "tabplot")) stop(paste(deparse(substitute(tab)), "is not a tabplot-object"))
    }
        
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
    if (is.list(tab) && !inherits(tab, "tabplot")) {
        nl <- length(tab)
        if (onePage) {
            stackvp <- function(x) viewport(layout.pos.row=x,layout.pos.col=1)
            grid.newpage()
            pushViewport(viewport(layout=grid.layout(nl, 1)))
            
            lapply(1:nl, FUN=function(i){
                plot(tab[[i]], vp=stackvp(i), ...)
            })
        } else {
            lapply(1:nl, FUN=function(i){
                plot(tab[[i]], ...)
            })
        }
    } else plot(tab, ...)
    
    on.exit(capture.output(dev.off()))
    invisible()
}