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
		
		addHandlerKeystroke(edt_save, handler = function(h,...) {
			value <- svalue(h$obj)
			if (value=="") {
				enabled(btn_save) <- FALSE		
			} else {
				enabled(btn_save) <- TRUE		
			}
		})
		
		addHandlerClicked(btn_save, handler = function(h,...) {
			filename <- svalue(edt_save)
			filenameExt <- paste(filename, ".", svalue(cmb_save), sep="")
			if (svalue(grad_save, index=TRUE)==1) {
				tableSave(tab, filenameExt)
				savedToMessage <- paste("Tableplot saved to ", getwd(), "/", filenameExt, ".", sep="")
			} else {
				assign(filename, tab, envir=.GlobalEnv)
				savedToMessage <- paste("Tabplot-object loaded in workspace as: ", filename, ".", sep="")
			}
			svalue(sbr) <- savedToMessage
			visible(wdw_save) <- FALSE
			enabled(wdw) <- TRUE
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