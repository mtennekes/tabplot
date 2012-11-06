tableplot_checkChangePalType <- function(change_palette_type_at, palettes) {
	if (length(change_palette_type_at)!=1 || !is.numeric(change_palette_type_at))
		stop("<change_palette_type_at> is not correct")
	if (change_palette_type_at < (mx <- max(sapply(palettes, length)))) {
		warning(paste("<change_palette_type_at> is less than", mx, "the number of colors in the largest palette. <change_palette_type_at> changed to", mx))
		change_palette_type_at <- mx
	}
	change_palette_type_at
}
