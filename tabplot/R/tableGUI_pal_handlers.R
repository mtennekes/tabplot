tableGUI_pal_handlers <- function(e) {
#	browser()
	with(e, {	
		addHandlerChanged(cmb_pal1, handler = function(h,...) {
			#browser()
			# obtain variable name and palette (name)
			varName <- svalue(h$obj)
			varIndex <- which(varData$Variable==varName)
			colPalName <- varData$PaletteName[varIndex]
			colPalStartCol <- varData$PaletteStartCol[varIndex]
			colPal <- palettes[[varIndex]]
			
			# 
			browser()
			svalue(spb_col) <- colPalStartCol
			
			if (colPalName=="custom") {
				pals$custom <- colPal
				enabled(spb_col) <- FALSE
			} else {
				enabled(spb_col) <- TRUE
			}
			
			changed <- (svalue(cmb_pal2)==colPalName)
			svalue(cmb_pal2) <- colPalName
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
			visible(wdw_pal) <- FALSE
			enabled(wdw) <- TRUE

		})

		
	})
}