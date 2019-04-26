library(tidyverse)
library(brms)

# Modifying package functions
  # Tell reloo to also leave out the trials passed via stanvars
tmp.fun <- brms:::reloo.loo
body(tmp.fun)[[12]][[4]][7:9] <- body(tmp.fun)[[12]][[4]][6:8]
body(tmp.fun)[[12]][[4]][[6]] <- substitute(
  if(!is.null(fit_j$stanvars$trials)){
    fit_j$stanvars$trials$sdata <- fit_j$stanvars$trials$sdata[-omitted]
    fit_j$stanvars$trials$scode <- "  int trials[639];"
  })
assignInNamespace("reloo.loo", value = tmp.fun, ns = "brms")


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