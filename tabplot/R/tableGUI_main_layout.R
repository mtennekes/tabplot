tableGUI_main_layout <- function(e) {
#	browser()
	with(e, {		
		######################################################
		## create GUI
		######################################################
		## create window
		wdw <- gwindow("Tableplot",visible=FALSE)
		sbr <- gstatusbar("Preparing...", cont=wdw)
		g <- gpanedgroup(cont=wdw)

		## create source frame
		ggg <- ggroup(horizontal = TRUE, cont = g, expand=TRUE)
		frm2 <- gframe(text="Source",horizontal = FALSE, cont = ggg) 
		size(frm2) <- c(350,400)
		grp4 <- ggroup(horizontal = FALSE, cont = frm2, expand=TRUE)
		grp9 <- ggroup(horizontal = TRUE, cont = grp4, expand=FALSE)
		lbl3 <- glabel("Data.frame:", cont=grp9)
		cmb <- gcombobox(datlist, cont=grp9)
		
		#addSpring(grp9)
		btnReload <- gbutton("Reload", cont=grp9, expand=FALSE)

		######## temp
		btnTemp <- gbutton("varTbl", cont=grp9, expand=FALSE)

		addHandlerClicked(btnTemp, function(h,...) {
			print(e$varTbl)
		})
		########
		
		
		## fill table 1
		
		tbl1 <- gtable(tableGUI_getTbl1(e=environment()), multiple=TRUE, cont=grp4, expand=TRUE)

		grp10 <- ggroup(horizontal = TRUE, cont = grp4, expand=FALSE)
		lbl4 <- glabel("Number of Objects:", cont=grp10)
		lbl5 <- glabel(nrow(get(svalue(cmb), envir=.GlobalEnv)), cont=grp10) 

		## create transfer button
		grp8 <- ggroup(horizontal = FALSE, cont = ggg, anchor=c(-1, -1),expand=TRUE)
		addSpring(grp8)
		btnTransfer <- gbutton(">", cont=grp8, expand=TRUE); enabled(btnTransfer) <- FALSE
		addSpace(grp8, 100, horizontal=FALSE)

		## create config frame
		frm <- gframe(text="Tableplot Configuration",horizontal = FALSE, cont = g) 
		size(frm) <- c(350,400)
		grp6 <- ggroup(horizontal = FALSE, cont = frm, expand=TRUE) 
		#lbl3 <- glabel("Columns", cont=grp6)

		tbl2 <- gtable(tableGUI_getTbl2(e=environment()), multiple=TRUE, cont=grp6, expand=TRUE)

		grp7 <- ggroup(horizontal = TRUE, cont = grp6, expand=FALSE) 
		btnUp <- gbutton("Up", cont=grp7, expand=TRUE); enabled(btnUp) <- FALSE
		btnDown <- gbutton("Down", cont=grp7, expand=TRUE); enabled(btnDown) <- FALSE
		btnScale <- gbutton("Scale", cont=grp7, expand=TRUE); enabled(btnScale) <- FALSE
		btnSort <- gbutton("Sort", cont=grp7, expand=TRUE); enabled(btnSort) <- FALSE
		btnAsCategory <- gbutton("As Categorical", cont=grp7, expand=TRUE); enabled(btnAsCategory) <- FALSE
		btnPal <- gbutton("Palette", cont=grp7, expand=TRUE); enabled(btnPal) <- FALSE

		#lbl10 <- glabel("Rows", cont=grp6)
		
		grp2 <- ggroup(horizontal = TRUE, cont = grp6) 
		cbx <- gcheckbox(text="Zoom in", checked = FALSE, cont= grp2)
		lbl7 <- glabel("from", cont=grp2)
		spbBinsFrom <- gspinbutton(0, 100, by = 10, cont=grp2, expand=FALSE)
		svalue(spbBinsFrom) <- 0
		
		lbl8 <- glabel("percent to", cont=grp2)
		spbBinsTo <- gspinbutton(0, 100, by = 10, cont=grp2, expand=FALSE)
		svalue(spbBinsTo) <- 100
		lbl9 <- glabel("percent", cont=grp2)
		enabled(lbl7) <- FALSE
		enabled(spbBinsFrom) <- FALSE
		enabled(lbl8) <- FALSE
		enabled(spbBinsTo) <- FALSE
		enabled(lbl9) <- FALSE
			
		grp1 <- ggroup(horizontal = TRUE, cont = grp6) 
		lbl1 <- glabel("Number of Row Bins:", cont=grp1)
		spbBins <- gspinbutton(0, 1000, by = 10, cont=grp1, expand=TRUE)
		svalue(spbBins) <- 100
		btnRun <- gbutton("Run", cont=grp1, expand=TRUE); enabled(btnRun) <- nrow(tbl2[,]!=0)
	})
}