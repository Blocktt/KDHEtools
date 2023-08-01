#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Source Pages ####

# Load files for individual screens

tab_Background <- source("external/tab_Background.R", local = TRUE)$value
tab_Instructions <- source("external/tab_Instructions.R", local = TRUE)$value
tab_FileBuilder <- source("external/tab_FileBuilder.R", local = TRUE)$value
tab_Calculator <- source("external/tab_Calculator.R", local = TRUE)$value

# Define UI
shinyUI(navbarPage(theme = shinytheme("yeti")
                   , paste0("KDHE Macroinvertebrate IBI Calculator ", pkg_ver)
                   , tab_Background()
                   , tab_Instructions()
                   , tab_FileBuilder()
                   , tab_Calculator()
          )## navbarPage~END
)## shinyUI~END
