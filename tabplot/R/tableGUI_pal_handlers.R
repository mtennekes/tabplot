tableGUI_pal_handlers <- function(e) {
	with(e, {	
		addHandlerChanged(cmb_pal1, handler = function(h,...) {
			# obtain variable name and palette (name)
			varName <- svalue(h$obj)
			varIndex <- which(varData$Variable==varName)
			colPalFullName <- varData$Palette[varIndex]
			colPalInfo <- tableGUI_getPalInfo(colPalFullName)
			colPalName <- colPalInfo$palName
			colPalStartCol <- colPalInfo$palStartCol

		
			# 
			blockHandler(cmb_pal2)
			blockHandler(spb_col)

			if (varName %in% names(customPals)) {
				cmb_pal2[] <- c(names(tabplotPalettes), "custom")
			} else {
				cmb_pal2[] <- names(tabplotPalettes)
			}

			if (colPalName=="custom") {
				enabled(spb_col) <- FALSE
			} else {
				enabled(spb_col) <- TRUE
				spb_col[] <- seq(1, length(tabplotPalettes[[colPalName]]), by=1)
			}
			
			changed <- (svalue(cmb_pal2)==colPalName)
			if (length(changed)==0) changed <- TRUE
			svalue(cmb_pal2) <- colPalName
			svalue(spb_col) <- colPalStartCol
			unblockHandler(cmb_pal2)
			unblockHandler(spb_col)
			if (changed) tableGUI_updatePal(e=e)

		})
		
		addHandlerChanged(cmb_pal2, handler = function(h,...) {
			varName <- svalue(cmb_pal1)
			tableGUI_updatePal(e=e)
		})

		addHandlerChanged(spb_col, handler = function(h,...) {
			tableGUI_updatePal(e=e)
		})

		addHandlerClicked(btn_ok, handler = function(h,...) {
			tableGUI_savePalette(e=e)
			tbl2[] <- tableGUI_getTbl2(vars=tbl2[,1], e=e)

			visible(wdw_pal) <- FALSE
			enabled(wdw) <- TRUE
		})

		addHandlerClicked(btn_cancel, handler = function(h,...) {
			visible(wdw_pal) <- FALSE
			enabled(wdw) <- TRUE
		})
		
		## close window
		addHandlerUnrealize(wdw_pal, handler = function(h,...) {
			visible(wdw_pal) <- FALSE
			enabled(wdw) <- TRUE

			return(TRUE)
		})

		
	})
}