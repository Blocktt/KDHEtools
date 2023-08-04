# Package Creation Helper Script
# Erik.Leppo@tetratech.com
# 2023-08-01
#
# Borrowed from BCGcalc package
# Most things can be done in RStudi.
# But easier to run a single script prior to upload to GitHub.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Packages ----
library(devtools)

# Global ----
myPkg <- "KDHEtools"

# Remove all files in "Results" folder ----
# Triggered here so can run different files
path_results <- file.path("inst", "shiny-examples", "KDHEtools", "results")
fn_results <- list.files(path_results
                         , full.names = TRUE
                         , include.dirs = FALSE
                         , recursive = TRUE)
file.remove(fn_results) # ok if no files
# Copy file to ensure directory not empty
path_shiny <- file.path("inst", "shiny-examples", "BCGcalc")
fn_copy    <- "remove.txt"
path_from  <- file.path(path_shiny, "data", fn_copy)
path_to    <- file.path(path_shiny, "results", fn_copy)
file.copy(path_from, path_to)


# NEWS----
# Render then Copy NEWS so picked up in help
rmarkdown::render("NEWS.rmd", "all")
file.copy("NEWS.md", "NEWS", overwrite = TRUE)
file.remove("NEWS.html")
#file.remove("NEWS.md")

# Documentation ----
#setwd(paste0("./",myPkg))  # will fail as already in this directory
devtools::document()

# Install New Package (locally) ----
setwd("..") # return to root directory first
devtools::install(myPkg, quick = FALSE, reload = TRUE, build_vignettes = FALSE)

# Reload library
library(myPkg, character.only = TRUE)
# change wd back to package
setwd(paste0("./",myPkg))

# Open Help ----
## so can test examples
help(package = (myPkg))
