tableGUI_save_layout <- function(e) {
	with(e, {

		wdw_save <- gwindow("Save", width=150, height=100, parent=wdw, visible=FALSE)
		gpan_save <- gpanedgroup(cont=wdw_save)
		
		grp0_save <- ggroup(horizontal = FALSE, cont = gpan_save, expand=FALSE)
		
		grp1_save <- ggroup(horizontal = TRUE, cont = grp0_save, expand=FALSE)
		grad_save <- gradio(c("Save to file", "Save to workspace"), cont=grp1_save)
		
		grp2_save <- ggroup(horizontal = TRUE, cont = grp0_save, expand=FALSE)
		formats <- c("pdf", "eps", "wmf", "png", "jpg", "bmp","tiff")
		
		edt_save <- gedit(text="", cont=grp2_save)
		lbl1_save <- glabel(".", cont=grp2_save)
		cmb_save <- gcombobox(formats, cont=grp2_save)

		grp2b_save <- ggroup(horizontal = TRUE, cont = grp0_save, expand=FALSE)
		lbl2_save <- glabel("width (inch):", cont=grp2b_save)
		spn_width <- gspinbutton(from=1, to=20, by=0.5, value=8, cont=grp2b_save)
		lbl3_save <- glabel(" height (inch):", cont=grp2b_save)
		spn_height <- gspinbutton(from=1, to=20, by=0.5, value=5, cont=grp2b_save)

		
		grp3_save <- ggroup(horizontal = TRUE, cont = grp0_save, expand=FALSE)
		btn_cancelsave <- gbutton("Cancel", container = grp3_save)
		btn_save <- gbutton("Save", container = grp3_save)
		enabled(btn_save) <- FALSE		
		
	})
}