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
tab_Calculator <- source("external/tab_Calculator.R", local = TRUE)$value
tab_MetAdjFact <- source("external/tab_MetAdjFact.R", local = TRUE)$value

# Define UI
shinyUI(navbarPage(theme = shinytheme("yeti"), "KDHE Macroinvertebrate IBI Calculator v0.0.1.9010"
                   ,tab_Background()
                   ,tab_Instructions()
                   ,tab_Calculator()
                   ,tab_MetAdjFact()
          )## navbarPage~END
)## shinyUI~END
