library(tidyverse)
library(brms)

# Convenience functions
  # Paste operator
  # Saves space when writing folder paths
`%p0%` <- function(x, y)paste0(x, y)

  # Fit & save model
  # Checks if filename exists, if not, fit model using fitfun with formula_ and save to disk
save_fit <- function(fitname, fitfun, formula_, ...) {
  if(file.exists("out/fits/" %p0% fitname %p0% ".rds"))stop("File already exists")
  saveRDS(object = fitfun(formula_, ...), file = "out/fits/" %p0% fitname %p0% ".rds")
}

  # Pre-defined models
source("code/models.R")