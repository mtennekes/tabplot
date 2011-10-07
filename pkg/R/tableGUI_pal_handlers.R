tableGUI_pal_handlers <- function(e) {
	with(e, {	
		addHandlerChanged(cmb_pal1, handler = function(h,...) {
			if (blockHandler_cmb_pal1) return()

			# obtain variable name and palette (name)
			varName <- svalue(h$obj)
			varIndex <- which(varData$Variable==varName)
			colPalFullName <- varData$Palette[varIndex]
			colPalInfo <- tableGUI_getPalInfo(colPalFullName)
			colPalName <- colPalInfo$palName
			colPalStartCol <- colPalInfo$palStartCol

		
			assign("blockHandler_pal", TRUE, envir=e)
			
			if (varName %in% names(customPals)) {
				cmb_pal2[] <- c(names(tabplotPalettes$qual), "custom")
			} else {
				cmb_pal2[] <- names(tabplotPalettes$qual)
			}

			if (colPalName=="custom") {
				enabled(spb_col) <- FALSE
			} else {
				enabled(spb_col) <- TRUE
				spb_col[] <- seq(1, length(tabplotPalettes$qual[[colPalName]]), by=1)
			}
			
			
			svalue(cmb_pal2) <- colPalName
			svalue(spb_col) <- colPalStartCol

			assign("blockHandler_pal", FALSE, envir=e)

			tableGUI_updatePal(e=e)

		})
		
		addHandlerChanged(cmb_pal2, handler = function(h,...) {
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