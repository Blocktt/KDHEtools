#### R script that builds Metric Adjustment Factors for WDEQ R Shiny app
#
# Ben.Block@tetratech.com, Date completed: 2022-06-02
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.1.2 (2021-11-01) -- "Bird Hippie"

#clear global environment
rm(list=ls())

# load libraries
library(tidyverse)

# Declare directories ####
wd <- getwd()
input.dir <- "StreamCat"
fn.data1 <- "BFI_WY.csv"
fn.data2 <- "GeoChemPhys1_WY.csv"
fn.data3 <- "Kffact_WY.csv"
fn.data4 <- "PRISM_1981_2010_WY.csv"
fn.data5 <- "STATSGO_Set2_WY.csv"
myDate <- format(Sys.Date(), "%Y%m%d")

# Read data files ####
df_BFI <- read_csv(file.path(wd,input.dir, fn.data1)
                     , na = c("NA",""), trim_ws = TRUE, skip = 0
                     , col_names = TRUE, guess_max = 100000)
df_GeoChemPhys1 <- read_csv(file.path(wd,input.dir, fn.data2)
                   , na = c("NA",""), trim_ws = TRUE, skip = 0
                   , col_names = TRUE, guess_max = 100000)
df_Kffact <- read_csv(file.path(wd,input.dir, fn.data3)
                   , na = c("NA",""), trim_ws = TRUE, skip = 0
                   , col_names = TRUE, guess_max = 100000)
df_PRISM <- read_csv(file.path(wd,input.dir, fn.data4)
                   , na = c("NA",""), trim_ws = TRUE, skip = 0
                   , col_names = TRUE, guess_max = 100000)
df_STATSGO <- read_csv(file.path(wd,input.dir, fn.data5)
                   , na = c("NA",""), trim_ws = TRUE, skip = 0
                   , col_names = TRUE, guess_max = 100000)

# Trim datasets ####
df_BFI_trim <- df_BFI %>%
  select(COMID, BFIWs)

df_GeoChemPhys1_trim <- df_GeoChemPhys1 %>%
  select(COMID, SWs)

df_Kffact_trim <- df_Kffact %>%
  select(COMID, KffactWs)

df_PRISM_trim <- df_PRISM %>%
  select(COMID, PrecipWs, TmaxWs, TmeanWs)

df_STATSGO_trim <- df_STATSGO %>%
  select(COMID, RckDepWs)

# Combine datasets ####
df_combine <- left_join(df_BFI_trim, df_GeoChemPhys1_trim
                        , by = "COMID") %>%
  left_join(., df_Kffact_trim, by = "COMID") %>%
  left_join(., df_PRISM_trim, by = "COMID") %>%
  left_join(., df_STATSGO_trim, by = "COMID")

# Mark COMIDs with missing data ####
df_final <- df_combine %>%
  mutate(Missing_Data = case_when((is.na(BFIWs)
                                   |is.na(SWs)
                                   |is.na(KffactWs)
                                   |is.na(PrecipWs)
                                   |is.na(TmaxWs)
                                   |is.na(TmeanWs)
                                   |is.na(RckDepWs)) ~ TRUE
                                  , (!is.na(BFIWs)
                                     & !is.na(SWs)
                                     & !is.na(KffactWs)
                                     & !is.na(PrecipWs)
                                     & !is.na(TmaxWs)
                                     & !is.na(TmeanWs)
                                     & !is.na(RckDepWs)) ~ FALSE)) %>%
  select(COMID, Missing_Data, everything())

# Export data ####
write.table(df_final, file.path(wd, input.dir
                                   , paste0("WY_StreamCat_MetAdjFactors_"
                                            , myDate,".csv"))
            , sep = ",", row.names = FALSE, col.names = TRUE, na = "")
