# Shiny Global File

# Version ----
pkg_ver <- "0.1.2.9016"

# Packages----
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinythemes)
library(shinyalert)
library(DT)
library(BioMonTools)
library(knitr)
library(rmarkdown)
library(StreamCatTools)
library(lubridate)
library(dplyr)
library(zip)

# Remove from global.R and DESCRIPTION
# library(capture) # possible delete

# File Size
# By default, the file size limit is 5MB.
# It can be changed by setting this option.
options(shiny.maxRequestSize = 25 * 1024^2)

# Default Import File Sep
sep_default <- ","

# Folders----
path_data <- file.path("data")
path_results <- file.path("results")

# ensure results folder exists
if (dir.exists(path_results) == FALSE) {
  dir.create(path_results)
} else {
  message(paste0("Directory already exists; ", path_data))
}## IF ~ dir.exists

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

# Random Forest Info----
predictors <- c("Al2O3Ws", "CFS", "ClayWs", "ElevCat", "Fe2O3Cat", "K2OWs"
                , "L3Eco", "LONG", "MgOCat", "NWs", "PermWs", "PrecipCat"
                , "PrecipWs", "SandWs", "SWs", "TmeanCat", "WetIndexWs"
                , "WsAreaSqKm", "WtDepWs")

# std_Parameters <- read.csv("data/standardization.parameters.csv", row.names = 1)

decreasers <- c("nt_habit_climbcling_RFadj","nt_volt_semi_RFadj", "nt_EPT_RFadj"
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


# EPSG ----
epsg_wgs84    <- 4326
epsg_nad83_na <- 4269
epsg_default  <- epsg_nad83_na

# Supporting Files ----
## Predictors
fn_support_pred <- "KS_StreamCat_PredictorTable_BenB_20230721.xlsx"
sheet_support_pred <- "Predictors_StreamCat"
path_support_pred <- file.path("data", fn_support_pred)
## Taxa Translator
fn_support_taxa_trans <- "KS_TaxaTranslator_20230717.csv"
path_support_taxa_trans <- file.path("data", "taxa_trans"
                                     , fn_support_taxa_trans)
col_taxaid_official_match <- "TaxaID_orig"
col_taxaid_official_all <- c("OTU_MMI", "OTU_MMI_genus", "OTU_BCG")
col_taxaid_official_mmi <- c("OTU_MMI", "OTU_MMI_genus")
col_taxaid_official_mmi_default <- "OTU_MMI"
col_taxaid_official_mmi_drop <- col_taxaid_official_all[!col_taxaid_official_all
                                          %in% col_taxaid_official_mmi_default]
#
fn_support_taxa_trans_meta <- "KS_TaxaTranslator_Metadata_20230717.csv"
path_support_taxa_trans_meta <- file.path("data", "taxa_trans"
                                     , fn_support_taxa_trans_meta)
## Taxa Attributes
fn_support_taxa_attr <- "KS_Attributes_20230717.csv"
path_support_taxa_attr <- file.path("data", "taxa_trans", fn_support_taxa_attr)
col_taxaid_attr <- "TaxaID"
#
fn_support_taxa_attr_meta <- "KS_Attributes_Metadata_20230717.csv"
path_support_taxa_attr_meta <- file.path("data", "taxa_trans"
                                         , fn_support_taxa_attr_meta)


