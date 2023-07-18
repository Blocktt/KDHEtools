# Shiny Global File

# Packages
library(shiny)
library(DT)
library(ggplot2)
library(readxl)
library(reshape2)
library(dplyr)
library(utils)
library(BioMonTools)
library(knitr)
library(maps)
library(rmarkdown)
library(markdown)
library(tidyr)
library(leaflet)
library(shinyjs) # used for download button enable
library(mapview) # used to download leaflet map
library(stringr)
library(shinythemes)
library(capture)
library(randomForest)
library(magrittr)


# File Size
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10MB.
options(shiny.maxRequestSize = 25*1024^2)

# define which metrics to keep in indices

DiatomMetrics <- c("pi_BC_12"
                   ,"pt_SALINITY_34"
                   ,"wa_POLL_TOL"
                   ,"nt_DIATAS_TN_2"
                   ,"pt_TROPHIC_12"
                   ,"pt_TROPHIC_56"
                   ,"pt_O_4")# END DiatomMetrics

# info for randomForest ####
predictors<-c("BFIWs","Elevation","KffactWs","PrecipWs"
              ,"RckDepWs","SWs","TmaxWs","TmeanWs")

# std_Parameters<-read.csv("./data/standardization.parameters.csv",row.names=1)

decreasers<-c("pt_T_WDEQ_12_RFadj","nt_Diatas_TN_2_RFadj","BC_12.pa")

increasers<-c("WA_Salinity_USGS","pt_O_WDEQ_4","pt_H_WDEQ_34_RFadj","pt_T_WDEQ_56_RFadj")

standardizeIncreasers <- function(x) {
  standardizedIncreasers<-100*(std_Parameters["ninetififth",i] - x)/(std_Parameters["ninetififth",i] - std_Parameters["fifth",i])
}

standardizeDecreasers <- function(x) {
  standardizedDecreasers<-100*(x - std_Parameters["fifth",i])/(std_Parameters["ninetififth",i] - std_Parameters["fifth",i])
}

# metric names to use in raw format (will be standardized though)
raw<-c("WA_Salinity_USGS","pt_O_WDEQ_4","BC_12.pa")

# metric names to use in adjusted format (will be adjusted and standardized)
toAdjust<-c("pt_H_WDEQ_34","pt_T_WDEQ_56","nt_Diatas_TN_2","pt_T_WDEQ_12")

# # rf models for adjusted metrics
# H34_model<-load("./data/pt_H_WDEQ_34_RFmod02162022.Rdata")
# T56_model<-load("./data/pt_T_WDEQ_56_RFmod02162022.Rdata")
# DiatasTN2_model<-load("./data/nt_Diatas_TN_2_RFmod02162022.Rdata")
# T12_model<-load("./data/pt_T_WDEQ_12_RFmod02162022.Rdata")
