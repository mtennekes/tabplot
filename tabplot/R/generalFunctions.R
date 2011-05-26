##  function to receive all loaded data.frames
lsDF <- function(envir=.GlobalEnv) {
	varNames <- ls(envir=envir)
	dfs <- sapply(varNames, function(i) inherits(get(i,envir=envir),c("data.table", "data.frame", "ffdf"), which=FALSE)[1])
	if (length(dfs)==0) {
		return(character(0))
	} else {
		return(varNames[dfs])
	}
}

##  function to receive the column names from all loaded data.frames
lsColnames <- function() {
	sapply(lsDF(),FUN=function(i) names(get(i)), simplify=FALSE)
}

## function to get classes
getClasses <- function(vars, DF) {
	n <- length(vars)
	classDF <- class(get(DF,envir=.GlobalEnv))[1]
	if (classDF=="ffdf") {
		dfTypes <- character(n)
		tmp <- get(DF,envir=.GlobalEnv)
		ind <- sapply(vars, FUN=function(x,y){which(x==y)}, names(tmp))
		for (i in 1:n) {
			tmp <- get(DF,envir=.GlobalEnv)[[ind[i]]]
			dfTypes[i] <- ifelse(is.null(ramclass(tmp)), "numeric", ramclass(tmp)[1])
		}
	} else if (classDF=="data.table") {
		dfTypes <- sapply(get(DF,envir=.GlobalEnv)[, vars, with=FALSE], FUN=function(x) class(x)[1])
	} else {
		dfTypes <- sapply(get(DF,envir=.GlobalEnv)[vars], FUN=function(x) class(x)[1])
	}
	return(dfTypes)
}
