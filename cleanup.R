# Cleanup: Remove all objects, clear plots, and reset libraries

# Clear the console
cat("\014")

# Clear all plots
if (!is.null(dev.list())) dev.off()

# Detach all non-base libraries
loaded_packages <- setdiff(loadedNamespaces(), base::loadedNamespaces())
lapply(loaded_packages, function(pkg) {
  try(detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE), silent = TRUE)
})

# Clear all objects from the environment
rm(list = ls())
