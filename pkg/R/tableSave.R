tableSave <- function (filename = paste(tab$dataset, ".pdf", sep = ""), tab, 
    device = default_device(filename), path = NULL, scale = 1, 
    width = par("din")[1], height = par("din")[2], dpi = 300, ...) 
{
    if (class(tab) != "tabplot") 
        stop("plot should be a tabplot object")
    eps <- ps <- function(..., width, height) grDevices::postscript(..., 
        width = width, height = height, onefile = FALSE, horizontal = FALSE, 
        paper = "special")
    tex <- function(..., width, height) grDevices::pictex(..., 
        width = width, height = height)
    pdf <- function(..., version = "1.4") grDevices::pdf(..., 
        version = version)
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
        match.fun(ext)
    }
    if (missing(width) || missing(height)) {
        message("Saving ", prettyNum(width * scale, digits = 3), 
            "\" x ", prettyNum(height * scale, digits = 3), "\" image")
    }
    width <- width * scale
    height <- height * scale
    if (!is.null(path)) {
        filename <- file.path(path, filename)
    }
    device(file = filename, width = width, height = height, ...)
    plot(tab)
    on.exit(capture.output(dev.off()))
    invisible()
}