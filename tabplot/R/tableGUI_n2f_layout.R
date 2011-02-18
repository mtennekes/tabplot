tableGUI_n2f_layout <- function(e) {
	with(e, {
		wdw2 <- gwindow("As Categorical", parent=wdw, width=200, height=100, visible=FALSE)
		g2 <- gpanedgroup(cont=wdw2)
		
		grp16 <- ggroup(horizontal = TRUE, cont=g2)

		grp11 <- ggroup(horizontal = FALSE, cont=grp16)

		grp15 <- ggroup(horizontal = TRUE, cont=grp11)
		lbl4 <- glabel("Variable name:", cont=grp15)
		lbl6 <- glabel(text="", width=13, cont=grp15)
		
		grp12 <- ggroup(horizontal = TRUE, cont=grp11)

		n2f.method <- c("fixed", "pretty", "kmeans", "discrete")
		frm4 <- gframe(text="Method", cont=grp12)
		rad2 <- gradio(n2f.method, cont=frm4)

		n2f.scale <- c("lineair", "logarithmic", "automatic")
		frm3 <- gframe(text="Scale", cont=grp12)
		rad <- gradio(n2f.scale, cont=frm3)

		grp17 <- ggroup(horizontal = FALSE, cont=grp16)

		grp13 <- ggroup(horizontal = TRUE, cont=grp17)
		lbl2 <- glabel(text="Number of Categories:", cont=grp13)
		cmb2 <- gcombobox(c("auto", 2:9), cont=grp13)
		#edt <- gedit(text="", width=3, cont=grp13)

		grp14 <- ggroup(horizontal = TRUE, cont=grp17)
		frm6 <- gframe("Breaks", cont=grp14)
		lay2 <- glayout(container=frm6, spacing=0)
		for (i in 1:3) {
			for (j in 1:3) {
				lay2[j, i] <- gedit(text="", width=7)
			}
		}
		enabled(lay2) <- FALSE
		grp18 <- ggroup(horizontal = TRUE, cont=grp17)
		btnOK <- gbutton("OK", cont=grp18, expand=TRUE)
		btnCancel <- gbutton("Cancel", cont=grp18, expand=TRUE)
		name <- character(0)
		
	})
}