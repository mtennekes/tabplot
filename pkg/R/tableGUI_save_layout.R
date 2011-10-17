tableGUI_save_layout <- function(e) {
	with(e, {

		wdw_save <- gwindow("Save", width=150, height=100, parent=wdw, visible=FALSE)
		gpan_save <- gpanedgroup(cont=wdw_save)
		
		grp0_save <- ggroup(horizontal = FALSE, cont = gpan_save, expand=FALSE)
		
		grp1_save <- ggroup(horizontal = TRUE, cont = grp0_save, expand=FALSE)
		grad_save <- gradio(c("Save to file", "Save to workspace"), cont=grp1_save)
		
		grp2_save <- ggroup(horizontal = TRUE, cont = grp0_save, expand=FALSE)
		formats <- c("pdf", "eps", "svg", "wmf", "png", "jpg", "bmp","tiff")
		
		edt_save <- gedit(text="", cont=grp2_save)
		lbl1_save <- glabel(".", cont=grp2_save)
		cmb_save <- gcombobox(formats, cont=grp2_save)

		grp3_save <- ggroup(horizontal = TRUE, cont = grp0_save, expand=FALSE)
		btn_cancelsave <- gbutton("Cancel", container = grp3_save)
		btn_save <- gbutton("Save", container = grp3_save)
		enabled(btn_save) <- FALSE		
		
		
		frm_pal2 <- gframe(text="Color palette", horizontal = FALSE, cont = grp_pal1) 
		
		grp_pal3 <- ggroup(horizontal = TRUE, cont = frm_pal2, expand=TRUE)


		grp_pal5 <- ggroup(horizontal = FALSE, cont = grp_pal3, expand=FALSE)
		
		tbl_pal <- glayout(container=grp_pal5, spacing = 2)
	
		# make 8,2 layout for palette
		tblCol1 <- list()
		for (i in 1:8) {
			tbl_pal[i,1] <- tblCol1[[i]] <- glabel(text = "", container =tbl_pal)
		}

		# cairoLoaded <- require(cairoDevice)
		# if (cairoLoaded) {
			# tbl_pal[1:8,2] <- ggraphics(width = 75 * 0.5, height = 75 * 3, dpi = 75, ps=8, container=tbl_pal)
		# } else {
			tblCol2 <- list()
			for (i in 1:8) {
				tbl_pal[i,2] <- tblCol2[[i]] <- glabel(text = "", container =tbl_pal)
			}
		# }

		grp_pal4 <- ggroup(horizontal = FALSE, cont = grp_pal3, expand=FALSE)
		cmb_pal2 <- gcombobox(names(tabplotPalettes$qual), cont=grp_pal4)

		grp_pal6 <- ggroup(horizontal = TRUE, cont = grp_pal4, expand=FALSE)
		lbl_pal <- glabel("start color:", cont=grp_pal6)
		spb_col <- gspinbutton(1, 16, by = 1, cont=grp_pal6, expand=FALSE)
		svalue(spb_col) <- 1

		addSpring(grp_pal4)
		
		btn_show <- gbutton("Show Palettes", container = grp_pal4)
		btn_cancel <- gbutton("Cancel", container = grp_pal4)
		btn_ok <- gbutton("OK", container = grp_pal4)
		
		
		
	})
}