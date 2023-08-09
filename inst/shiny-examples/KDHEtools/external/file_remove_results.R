# Remove files in "Results" folder
# Erik.Leppo@tetratech.com
# 2023-08-08
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Add as a script so can reference multiple times



# Remove all files in "Results" folder
# Triggered here so can run from different locations
fn_results <- list.files(path_results
                         , full.names = TRUE
                         , include.dirs = FALSE
                         , recursive = TRUE)
message(paste0("Files in 'results' folder (before removal) = "
               , length(fn_results)))
file.remove(fn_results) # ok if no files
# QC, repeat
fn_results2 <- list.files(path_results
                          , full.names = TRUE
                          , include.dirs = FALSE
                          , recursive = TRUE)
message(paste0("Files in 'results' folder (after removal [should be 0]) = "
               , length(fn_results2)))
