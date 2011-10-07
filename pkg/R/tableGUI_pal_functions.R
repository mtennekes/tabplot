tableGUI_initPal <- function(e) {
	with(e, {
		visible(wdw_pal) <- TRUE
		enabled(wdw) <- FALSE

		selectVars <- tbl2[, "Variable"]
		if (is.na(selectVars)[1]) selectVars <- character(0)

		varData <- tableGUI_getTbl2(vars=selectVars, cols=c("Variable", "Type", "Levels", "Palette"), e=e)

		varData <- varData[varData$Levels!=0,]

		levelNames <- lapply(varData$Variable, FUN=function(x) {
				lvs <- levels(get(tableGUI_getCurrentDFname(e), envir=.GlobalEnv)[[x]])
				if (is.null(lvs)) lvs <- c("TRUE", "FALSE")
				return(lvs)
			})
		
		varData <- as.list(varData)
		varData$levelNames <- levelNames
		
		assign("varData", varData, e)
		
		#not working: blockHandler(cmb_pal1)
		blockHandler_cmb_pal1 <- TRUE
		cmb_pal1[] <- varData$Variable
		#not working: unblockHandler(cmb_pal1)
		blockHandler_cmb_pal1 <- FALSE
		
		svalue(cmb_pal1) <- palVarName
		
	})
 }
 
tableGUI_updatePal <- function(e) {
	with(e, {
		if (blockHandler_pal) return()
		varName <- svalue(cmb_pal1)
		if (is.null(varName)) {
			# if (cairoLoaded) {
				# visible(tbl_pal[1:8,2]) <- TRUE
				# grid.rect(gp=gpar(col=NA, fill="grey95"))

			# } else {
				for (i in 1:8) {
					svalue(tbl_pal[i,2]) <- ""
				}
			# }

			for (i in 1:8) {
				svalue(tbl_pal[i,1]) <- ""
			}

		} else {
			varIndex <- which(varData$Variable==varName)
			printLev <- varData$levelNames[[varIndex]][1:8]
			emptySlots <- is.na(printLev)
			printLev[emptySlots] <- ""
			
			palName <- svalue(cmb_pal2)
			startColor <- svalue(spb_col)
			
			
			if (palName=="custom") {
				colPal <- customPals[[varName]]
				colPal <- ifelse(nchar(colPal)==9, substr(colPal, 1, 7), colPal)
				svalue(spb_col) <- 1
				enabled(spb_col) <- FALSE
			} else {
				colPal <- tabplotPalettes$qual[[palName]]
				enabled(spb_col) <- TRUE
				spb_col[] <- seq(1, length(colPal), by=1)
			}
			
			colInd <- startColor:length(colPal)
			if (startColor!=1) colInd <- c(colInd, 1:(startColor-1))
			
			drawFill <- rep(colPal[colInd], length.out=8)
			drawFill[emptySlots] <- NA
			
			drawCol <- ifelse(emptySlots, "white", "black")

			printCol <- paste("****color", rep(colInd, length.out=8), "****", sep="")
			printCol[emptySlots] <- ""
			
			# if (cairoLoaded) {
				#tbl_pal[1,2] <- ggraphics(width = 75 * 0.5, height = 75 * 3, dpi = 75, ps=8, container=tbl_pal)
				
				# visible(tbl_pal[1:8,2]) <- TRUE
				# grid.rect(gp=gpar(col=NA, fill="grey95"))
				# grid.rect(y=seq(15/16,1/16,length.out=8), height=1/12, width=0.9, gp=gpar(col=NA, fill=drawFill))
				
			# } else {
				for (i in 1:8) {
					svalue(tbl_pal[i,2]) <- printCol[i]
					font(tbl_pal[i,2])  <- c(color=drawFill[i], style="bold")
					
				}
			# }

			for (i in 1:8) {
				svalue(tbl_pal[i,1]) <- printLev[i]
			}
			
			##save pal info
			varData$Palette[[varIndex]] <- tableGUI_setPalInfo(palName, startColor)
		}
	})
}
 
tableGUI_savePalette <- function(e) {
	with(e, {
		tableGUI_setVarTbl(varData$Variable, "Palette", varData$Palette, e)
	})
}