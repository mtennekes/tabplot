tableGUI_pal_layout <- function(e) {
	with(e, {
		
		#### load color palettes
		brewer_pals_info <- brewer.pal.info[brewer.pal.info$category=="qual",]
		
		pals <- mapply(FUN=function(x,y){brewer.pal(y, x)}, row.names(brewer_pals_info), brewer_pals_info$maxcolors)
		
		# remove color red (needed for NA's)
		pals$Set1 <- brewer_pals$Set1[-1]
		
		# create list with palettes: first the default one (Brewer_Set1+Brewer_Set2), then the brewer palettes
		pals <- c(list(Default=c(pals$Set1, pals$Set2)), pals)

		wdw_pal <- gwindow("Color palette", parent=wdw, width=200, height=100)
		#wdw_pal <- gwindow("Color palette", width=200, height=100)
		gpan_pal <- gpanedgroup(cont=wdw_pal)
		
		grp_pal1 <- ggroup(horizontal = FALSE, cont = gpan_pal, expand=TRUE)
		frm_pal1 <- gframe(text="Categorical Variable", horizontal = FALSE, cont = grp_pal1) 


		grp_pal3 <- ggroup(horizontal = TRUE, cont = grp_pal2, expand=TRUE)
		
		lbl_pal <- glabel("Categorical variable:", cont=grp_pal3)
		
		selectCatVars <- substr(tbl2[,"Type"],1,3)=="cat"
		
		# retrieve categorical variables
		catlist <- tbl2[selectCatVars, "Variable"]
		nLevels <- as.integer(substr(tbl2[selectCatVars, "Type"],14,14))
		
		cmb_pal1 <- gcombobox(catlist, cont=grp_pal3)

		frm_pal2 <- gframe(text="Color palette", horizontal = FALSE, cont = grp_pal1) 

		grp_pal4 <- ggroup(horizontal = TRUE, cont = frm_pal2, expand=TRUE)
		lbl_pal <- glabel("Palette:", cont=grp_pal4)
		cmb_pal1 <- gcombobox(names(brewer_pals), cont=grp_pal4)
		
		frm_pal2 <- gframe(text="Color mapping", horizontal = FALSE, cont = grp_pal1) 
	
		tbl_pal <- glayout(container =frm_pal2)
		tbl_pal[1,1] <- "levels"
		tbl_pal[1,2] <- "colours"
		
		
		testbutton <- gbutton("####", container=g3)
		
		
		
		
		
		font(testbutton) <- c(color="red", style="bold")
		
		if (require(cairoDevice)) {
			ggraphics(ps=8, container=g3)
		}
		
		
		
		k <- length(brewer_pals)
		ncols <- max(brewer_pals_info$maxcolors)
		pushViewport(viewport(layout=grid.layout(k+1, ncols+1)))

		for (i in 1:ncols) {
			pushViewport(viewport(layout.pos.col=i+1, layout.pos.row=1))		
			grid.text(i)
			popViewport()
		}
		for (j in 1:k) {
			pushViewport(viewport(layout.pos.col=1, layout.pos.row=j+1))		
			grid.text(names(brewer_pals)[j])
			popViewport()

			for (i in 1:ncols) {
				pushViewport(viewport(layout.pos.col=i+1, layout.pos.row=j+1))		
				grid.rect(height=0.5, gp=gpar(col=NA,fill=brewer_pals[[j]][i]))
				popViewport()
			}
		}
		
	})
}