#' Summary of a tabplot object.
#'
#' Summary of a tabplot object that is created with \code{\link{tableplot}(..., plot=FALSE)}.
#'
#' @aliases summary.tabplot
#' @param object tabplot object
#' @param digits integer, used for number formatting with \code{\link{format}()}
#' @param ... arguments passed to other methods
#' @export
summary.tabplot <- 
function(object, digits = max(3, getOption("digits") - 
    3), ...) {

    ncw <- function(x) {
        z <- nchar(x, type = "w")
        if (any(na <- is.na(z))) {
            z[na] <- nchar(encodeString(z[na]), "b")
        }
        z
    }
	
	general <- list(
					variables=object$n,
					objects=object$rows$m,
					bins=object$nBins,
					from=paste(object$rows$from, "%", sep=""),
					to=paste(object$rows$to, "%", sep=""))
	class(general) <- c("summaryDefault", "table")
	
	summary_cat <- function(col){
		L <- list(name=col$name, type="categorical", sort=ifelse(col$sort=="", "NA", col$sort), categories=length(col$categories))
		class(L) <- c("summaryDefault", "table")
		L
	}
	
	summary_num <- function(col) {
		L <- list(name=col$name, type="numeric", sort=ifelse(col$sort=="", "NA", col$sort), scale_init=col$scale_init, scale_final=col$scale_final)
		class(L) <- c("summaryDefault", "table")
		L
	}
	
	z <- c(list(general=general), lapply(object$columns, FUN=function(col) {
			if (col$isnumeric)
				return(summary_num(col))
			else
				return(summary_cat(col))
		}))
		
	names(z)[-1] <- paste("variable", 1:object$n, sep="")
	
	## code below is borrowed from summary.data.frame
    nv <- length(z)
    nm <- names(z)
	
    lw <- numeric(nv)
    nr <- max(unlist(lapply(z, NROW)))
    for (i in 1L:nv) {
        sms <- z[[i]]
        if (is.matrix(sms)) {
            cn <- paste(nm[i], gsub("^ +", "", colnames(sms), 
                useBytes = TRUE), sep = ".")
            tmp <- format(sms)
            if (nrow(sms) < nr) 
                tmp <- rbind(tmp, matrix("", nr - nrow(sms), 
                  ncol(sms)))
            sms <- apply(tmp, 1L, function(x) paste(x, collapse = "  "))
            wid <- sapply(tmp[1L, ], nchar, type = "w")
            blanks <- paste(character(max(wid)), collapse = " ")
            wcn <- ncw(cn)
            pad0 <- floor((wid - wcn)/2)
            pad1 <- wid - wcn - pad0
            cn <- paste(substring(blanks, 1L, pad0), cn, substring(blanks, 
                1L, pad1), sep = "")
            nm[i] <- paste(cn, collapse = "  ")
            z[[i]] <- sms
        }
        else {
            lbs <- format(names(sms))
            sms <- paste(lbs, ":", format(sms, digits = digits), 
                "  ", sep = "")
            lw[i] <- ncw(lbs[1L])
            length(sms) <- nr
            z[[i]] <- sms
        }
    }
    z <- unlist(z, use.names = TRUE)
    dim(z) <- c(nr, nv)
    if (any(is.na(lw))) 
        warning("probably wrong encoding in names(.) of column ", 
            paste(which(is.na(lw)), collapse = ", "))
    blanks <- paste(character(max(lw, na.rm = TRUE) + 2L), collapse = " ")
    pad <- floor(lw - ncw(nm)/2)
    nm <- paste(substring(blanks, 1, pad), nm, sep = "")
    dimnames(z) <- list(rep.int("", nr), nm)
    attr(z, "class") <- c("table")
    z
}