#### R script that trims master taxa list and traits for WDEQ R Shiny app
#
# Ben.Block@tetratech.com, Date completed: 2022-05-14
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.1.2 (2021-11-01) -- "Bird Hippie"

#clear global environment
rm(list=ls())

# load libraries
library(tidyverse)
library(readxl)

# Declare directories ####
wd <- getwd()
input.dir <- "TaxaTraits"
fn.data1 <- "TaxaMasterPeriphyton_FullTraits_20220531.xlsx"
myDate <- format(Sys.Date(), "%Y%m%d")

# Read data files ####
df_msl <- read_excel(file.path(wd,input.dir, fn.data1)
                           , na = c("NA",""), trim_ws = TRUE, skip = 0
                           , col_names = TRUE, guess_max = 100000)
df_fields <- read_excel(file.path(wd,input.dir, fn.data1), sheet = "Keep_Fields"
                        , na = c("NA",""), trim_ws = TRUE, skip = 0
                        , col_names = TRUE, guess_max = 100000)

# trim by fields ####
fields_keep <- df_fields %>%
  filter(Keep == "Yes") %>%
  pull(ColName)

df_msl_trim <- df_msl %>%
  select(one_of(fields_keep))

# mutate to fit needs ####
df_msl_new <- df_msl_trim %>%
  mutate(BC_USGS = case_when((COMMON_NAME == "Achnanthes ploenensis"
                              |COMMON_NAME == "Karayevia ploenensis")~"BC_3; BC_4"
                             , (COMMON_NAME == "Nitzschia sociabilis") ~ "BC_4; BC_5"
                             , (BC_1 == 1) ~ "BC_1"
                             , (BC_2 == 1) ~ "BC_2"
                             , (BC_3 == 1) ~ "BC_3"
                             , (BC_4 == 1) ~ "BC_4"
                             , (BC_5 == 1) ~ "BC_5")
         , SALINITY_USGS = case_when((COMMON_NAME == "Staurosira construens v. binodis")
                                     ~ "SALINITY_2; SALINITY_3"
                                     , (SALINITY_1 == 0 & SALINITY_2 == 0
                                        & SALINITY_3 == 0 & SALINITY_4 == 0)
                                     ~ "SALINITY_0"
                                     , (SALINITY_1 == 1) ~ "SALINITY_1"
                                     , (SALINITY_2 == 1) ~ "SALINITY_2"
                                     , (SALINITY_3 == 1) ~ "SALINITY_3"
                                     , (SALINITY_4 == 1) ~ "SALINITY_4")
         , SALINITY_USGS_NUM = case_when((SALINITY_USGS == "SALINITY_2; SALINITY_3")
                                         ~ 2.5
                                         , (SALINITY_USGS == "SALINITY_0") ~0
                                         , (SALINITY_USGS == "SALINITY_1") ~1
                                         , (SALINITY_USGS == "SALINITY_2") ~2
                                         , (SALINITY_USGS == "SALINITY_3") ~3
                                         , (SALINITY_USGS == "SALINITY_4") ~4)) %>%
  select(-c(BC_1,BC_2,BC_3,BC_4,BC_5
            ,SALINITY_1,SALINITY_2,SALINITY_3,SALINITY_4))

# Export data ####
write.table(df_msl_new, file.path(wd, input.dir
                                , paste0("TaxaMasterPeriphyton_TrimTraits_"
                                         , myDate,".csv"))
            , sep = ",", row.names = FALSE, col.names = TRUE, na = "")
