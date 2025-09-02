save_plots <- function(plots, pathname, filenames = NULL, 
                       dpi = 600, width = 10, height = 6) {
   # Ensure output directory exists
   if (!dir.exists(pathname)) {
      dir.create(pathname, recursive = TRUE)
   }
   
   # If no filenames given, create default ones
   if (is.null(filenames)) {
      filenames <- paste0("plot_", seq_along(plots), ".png")
   }
   
   # Safety check
   if (length(filenames) != length(plots)) {
      stop("Length of filenames must match number of plots.")
   }
   
   # Save each plot
   for (i in seq_along(plots)) {
      ggsave(
         filename = file.path(pathname, filenames[i]),
         plot     = plots[[i]],
         dpi      = dpi,
         width    = width,
         height   = height
      )
   }
   
   invisible(filenames) # return filenames invisibly
}