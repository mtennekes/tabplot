tableGUI_initPal <- function(e) {
	with(e, {
		visible(wdw_pal) <- TRUE
		enabled(wdw) <- FALSE

		selectVars <- tbl2[, "Variable"]
		if (is.na(selectVars)[1]) selectVars <- character(0)

		varData <- tableGUI_getTbl2(vars=selectVars, cols=c("Variable", "Type", "Levels", "Palette"), e=e)

		varData <- varData[varData$Levels!=0,]

		levelNames <- lapply(varData$Variable, FUN=function(x) {
				lvs <- levels(get(tableGUI_getCurrentDFname(e), envir=.GlobalEnv)[[x]])
				if (is.null(lvs)) lvs <- c("TRUE", "FALSE")
				return(lvs)
			})
		
		varData <- as.list(varData)
		varData$levelNames <- levelNames
		
		assign("varData", varData, e)
		
		cmb_pal1[] <- varData$Variable
		svalue(cmb_pal1) <- varData$Variable[1]
		
	})
 }
 
tableGUI_updatePal <- function(e) {
	with(e, {
		varName <- svalue(cmb_pal1)
		if (is.null(varName)) {
			if (cairoLoaded) {
				visible(tbl_pal[1:8,2]) <- TRUE
				grid.rect(gp=gpar(col=NA, fill="grey95"))

			} else {
				for (i in 1:8) {
					svalue(tbl_pal[i,2]) <- ""
				}
			}

			for (i in 1:8) {
				svalue(tbl_pal[i,1]) <- ""
			}

		} else {
			printLev <- varData$levelNames[[which(varData$Variable==varName)]][1:8]
			emptySlots <- is.na(printLev)
			printLev[emptySlots] <- ""
			
			printCol <- paste("**** colour", 1:8, "****")
			printCol[emptySlots] <- ""
			
			drawFill <- rep(pals[[svalue(cmb_pal2)]], length.out=8)
			drawFill[emptySlots] <- NA
			
			drawCol <- ifelse(emptySlots, "white", "black")
			
			if (cairoLoaded) {
				#tbl_pal[1,2] <- ggraphics(width = 75 * 0.5, height = 75 * 3, dpi = 75, ps=8, container=tbl_pal)
				
				visible(tbl_pal[1:8,2]) <- TRUE
				grid.rect(gp=gpar(col=NA, fill="grey95"))
				grid.rect(y=seq(15/16,1/16,length.out=8), height=1/12, width=0.9, gp=gpar(col=NA, fill=drawFill))
				
			} else {
				for (i in 1:8) {
					svalue(tbl_pal[i,2]) <- printCol[i]
					font(tbl_pal[i,2])  <- c(color=pals[[svalue(cmb_pal2)]][i], style="bold")
					
				}
			}

			for (i in 1:8) {
				svalue(tbl_pal[i,1]) <- printLev[i]
			}
		}
	})
}
 
tableGUI_showAll <- function(e) {
	if (require(cairoDevice)) {
		ggraphics(width = 75 * 6, height = 75 * 4, dpi = 75, ps=8, container=g3)
	} else {
		dev.new(width=6, height=4, rescale="fixed")
	}
	
	
	k <- length(pals)
	ncols <- max(sapply(pals,FUN=length))

	pushViewport(viewport(layout=grid.layout(k+1, ncols+1)))
	for (i in 1:ncols) {
		pushViewport(viewport(layout.pos.col=i+1, layout.pos.row=1))		
		grid.text(i)
		popViewport()
	}
	for (j in 1:k) {
		pushViewport(viewport(layout.pos.col=1, layout.pos.row=j+1))		
		grid.text(names(pals)[j])
		popViewport()

		for (i in 1:ncols) {
			pushViewport(viewport(layout.pos.col=i+1, layout.pos.row=j+1))		
			grid.rect(height=0.66, gp=gpar(col=NA,fill=pals[[j]][i]))
			popViewport()
		}
	}
}