tableGUI_init_data <- function(e) {
#	browser()
	with(e, {		
	
		## Internal function to receive all loaded data.frames
		lsDF <- function(envir=.GlobalEnv) {
			varNames <- ls(envir=envir)
			dfs <- sapply(varNames, function(i) inherits(get(i,envir=envir),c("data.frame", "ffdf"), which=FALSE)[1])
			if (length(dfs)==0) {
				return(character(0))
			} else {
				return(varNames[dfs])
			}
		}
		
		## Internal function to receive the column names from all loaded data.frames
		lsColnames <- function() {
			sapply(lsDF(),FUN=function(i) names(get(i)), simplify=FALSE)
		}

		## function to get classes
		getClasses <- function(vars) {
			n <- length(vars)
			if (class(get(currentDF,envir=.GlobalEnv))=="ffdf") {
				dfTypes <- character(n)
				tmp <- get(currentDF,envir=.GlobalEnv)
				ind <- sapply(vars, FUN=function(x,y){which(x==y)}, names(tmp))
				for (i in 1:n) {
					tmp <- get(currentDF,envir=.GlobalEnv)[[ind[i]]]
					dfTypes[i] <- ifelse(is.null(ramclass(tmp)), "numeric", ramclass(tmp)[1])
				}
			} else {
				dfTypes <- sapply(get(currentDF,envir=.GlobalEnv)[vars], FUN=function(x) class(x)[1])
			}
			return(dfTypes)
		}

		## receive list of loaded data.frames and its column names
		datlist <- lsDF()
		if (length(datlist)==0) stop("No data.frames loaded.")
		allCols <- lsColnames()
		
		## setup GUI administration
		currentDF <- datlist[1]
		dfNames <- names(get(currentDF, envir=.GlobalEnv))
		dfClasses <- getClasses(dfNames)
		dfLevels <- sapply(dfNames, FUN=function(x){nlevels(get(currentDF,envir=.GlobalEnv)[[x]])})
		
		dfTypes <- mapply(dfClasses, dfLevels, FUN=function(x,y) {
				ifelse(x=="factor", paste("categorical (", y, ")", sep=""),
					ifelse(x=="logical", "categorical (2)",
						ifelse(x %in% c("numeric", "integer"), "numeric", "unknown")))
			})
		
		varTbl <- data.frame(Variable=dfNames, Class=dfClasses, Levels=dfLevels, Type=dfTypes, Scale="", Sort="", Pal="Default", PalInitNr = 0, Selected = FALSE, New = FALSE, stringsAsFactors=FALSE)
		
	})
}