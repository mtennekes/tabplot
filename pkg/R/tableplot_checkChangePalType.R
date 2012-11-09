tableplot_checkChangePalType <- function(change_palette_type_at, max_pal_length) {
	if (length(change_palette_type_at)!=1 || !is.numeric(change_palette_type_at))
		stop("<change_palette_type_at> is not correct")
	if (change_palette_type_at < max_pal_length) {
		warning(paste("<change_palette_type_at> is less than", max_pal_length, "the number of colors in the largest palette. <change_palette_type_at> changed to", mx))
		change_palette_type_at <- max_pal_length
	}
	change_palette_type_at
}
