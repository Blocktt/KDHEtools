# Shiny Global File

# Packages
library(shiny)
library(DT)
library(ggplot2) # possible delete
library(readxl)
library(reshape2)
library(dplyr)
library(utils)
library(BioMonTools)
library(knitr)
library(maps) # possible delete
library(rmarkdown)
library(markdown)
library(tidyr)
library(leaflet) # possible delete
library(shinyjs)
library(mapview) # possible delete
library(stringr)
library(shinythemes)
library(capture) # possible delete
library(randomForest)
library(magrittr)


# File Size
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10MB.
options(shiny.maxRequestSize = 25*1024^2)

# define which metrics to keep in indices

BugMetrics <- c("nt_EPT"
                , "nt_habit_climbcling"
                , "nt_volt_semi"
                , "pt_BCG_att1i234b"
                , "x_HBI")# END BugMetrics

# info for randomForest ####
predictors<-c("Al2O3Ws", "CFS", "ClayWs", "ElevCat", "Fe2O3Cat", "K2OWs", "L3Eco"
              , "LONG", "MgOCat", "NWs", "PermWs", "PrecipCat", "PrecipWs"
              , "SandWs", "SWs", "TmeanCat", "WetIndexWs", "WsAreaSqKm", "WtDepWs")

# std_Parameters<-read.csv("./data/standardization.parameters.csv",row.names=1)

decreasers<-c("nt_habit_climbcling_RFadj","nt_volt_semi_RFadj", "nt_EPT_RFadj"
              ,"pt_BCG_att1i234b_RFadj")

increasers<-c("x_HBI_RFadj")

standardizeIncreasers <- function(x) {
  standardizedIncreasers<-100*(std_Parameters["ninetififth",i] - x)/(std_Parameters["ninetififth",i] - std_Parameters["fifth",i])
}

standardizeDecreasers <- function(x) {
  standardizedDecreasers<-100*(x - std_Parameters["fifth",i])/(std_Parameters["ninetififth",i] - std_Parameters["fifth",i])
}

# rf models for adjusted metrics
# habit_model<-load("./data/nt_habit_climbcling_RFmod_final0517.Rdata")
# HBI_model<-load("./data/x_HBI_RFmod_final0517.Rdata")
# BCG_model<-load("./data/pt_BCG_att1i234b_RFmod_final0517.Rdata")
# semiv_model<-load("./data/nt_volt_semi_RFmod_final0517.Rdata")
# EPT_model<-load("./data/nt_EPT_RFmod_final0517.Rdata")
