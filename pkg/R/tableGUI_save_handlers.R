tableGUI_save_handlers <- function(e) {
	with(e, {	

		addHandlerChanged(grad_save, handler = function(h,...) {
			if (svalue(h$obj, index=TRUE)==1) {
				enabled(lbl1_save) <- TRUE
				enabled(cmb_save) <- TRUE
			} else {
				enabled(lbl1_save) <- FALSE
				enabled(cmb_save) <- FALSE
			}
		})
		
		addHandlerClicked(btn_save, handler = function(h,...) {
			filename <- svalue(edt_save)
			filenameExt <- paste(filename, ".", svalue(cmb_save), sep="")
			if (svalue(grad_save, index=TRUE)==1) {
				tableSave(tab, filenameExt)
			} else {
				assign(filename, tab, envir=.globalEnv())
			}
		})

		
		addHandlerClicked(btn_cancelsave, handler = function(h,...) {
			visible(wdw_pal) <- FALSE
			enabled(wdw) <- TRUE
		})
		
		## close window
		addHandlerUnrealize(wdw_save, handler = function(h,...) {
			visible(wdw_save) <- FALSE
			enabled(wdw) <- TRUE

			return(TRUE)
		})

		
	})
}