tableGUI_pal_handlers <- function(e) {
#	browser()
	with(e, {	
		addHandlerChanged(cmb_pal1, handler = function(h,...) {
			varName <- svalue(h$obj)
			colPal <- varData$Palette[varData$Variable==varName]
#browser()
			
			## todo palette(11) ?gsub
			bracketPos <- regexpr("(", colPal, fixed=TRUE)
			
			svalue(cmb_pal2) <- substr(colPal, 1, bracketPos-1)
			
			tableGUI_updatePal(e=e)

		})
		
		addHandlerChanged(cmb_pal2, handler = function(h,...) {
			varName <- svalue(cmb_pal1)
			#browser()
			tableGUI_updatePal(e=e)
		})

	})
}