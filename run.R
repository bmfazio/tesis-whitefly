# Config
options(encoding = "utf8")
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
options(mc.cores = parallel::detectCores())
packrat::set_opts(vcs.ignore.src = TRUE, vcs.ignore.lib = TRUE)

# Run
source("code/setup.R")
source("code/load.R")