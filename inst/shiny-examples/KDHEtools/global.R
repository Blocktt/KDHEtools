# Shiny Global File

# Version ----
pkg_ver <- "0.1.2.9003"

# Packages----
library(shiny)
library(DT)
library(readxl)       # no references
library(reshape2)     # no references
library(dplyr)        # no references
library(BioMonTools)
library(knitr)
library(rmarkdown)
library(tidyr)        # no references
library(shinyjs)
library(stringr)      # no references
library(shinythemes)  # no references
library(randomForest) # no references

# Add to DESCRIPTION
library(utils)        # no references
library(markdown)     # no references
library(magrittr)     # no references
library(shinyBS)

# Add from DESCRIPTION
library(flextable)    # no references
library(kableExtra)   # no references

# Possible delete
library(ggplot2) # possible delete  # no references
library(maps)    # possible delete  # no references
library(leaflet) # possible delete  # no references
library(mapview) # possible delete  # no references

# Remove from global.R and DESCRIPTION
# library(capture) # possible delete

# File Size
# By default, the file size limit is 5MB.
# It can be changed by setting this option.
options(shiny.maxRequestSize = 25 * 1024^2)

# define which metrics to keep in indices

BugMetrics <- c("nt_EPT"
                , "nt_habit_climbcling"
                , "nt_volt_semi"
                , "pt_BCG_att1i234b"
                , "x_HBI"
                , "ni_total"
                , "nt_Coleo"
                , "nt_Ephem"
                , "nt_Odon"
                , "nt_Trich"
                , "pi_airbreath"
                , "pi_Cheu"
                , "pi_CorixPhys"
                , "pi_dom01"
                , "pi_dom02"
                , "pt_Chiro"
                , "pt_oneind"
                , "x_Shan_2")# END BugMetrics

# info for randomForest ####
predictors < -c("Al2O3Ws", "CFS", "ClayWs", "ElevCat", "Fe2O3Cat", "K2OWs"
                , "L3Eco", "LONG", "MgOCat", "NWs", "PermWs", "PrecipCat"
                , "PrecipWs", "SandWs", "SWs", "TmeanCat", "WetIndexWs"
                , "WsAreaSqKm", "WtDepWs")

# std_Parameters<-read.csv("./data/standardization.parameters.csv",row.names=1)

decreasers < -c("nt_habit_climbcling_RFadj","nt_volt_semi_RFadj", "nt_EPT_RFadj"
              ,"pt_BCG_att1i234b_RFadj")

increasers <- c("x_HBI_RFadj")

# standardizeIncreasers <- function(x) {
#   standardizedIncreasers<-100*(std_Parameters["ninetififth",i] - x)/(std_Parameters["ninetififth",i] - std_Parameters["fifth",i])
# }
#
# standardizeDecreasers <- function(x) {
#   standardizedDecreasers<-100*(x - std_Parameters["fifth",i])/(std_Parameters["ninetififth",i] - std_Parameters["fifth",i])
# }

# rf models for adjusted metrics
# habit_model <- load("./data/nt_habit_climbcling_RFmod_final0517.Rdata")
# HBI_model <- load("./data/x_HBI_RFmod_final0517.Rdata")
# BCG_model < -load("./data/pt_BCG_att1i234b_RFmod_final0517.Rdata")
# semiv_model <- load("./data/nt_volt_semi_RFmod_final0517.Rdata")
# EPT_model <- load("./data/nt_EPT_RFmod_final0517.Rdata")
