#### R script that builds calibration input file for WDEQ R Shiny app
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
fn.data1 <- "TaxaMasterPeriphyton_TrimTraits_20220603.csv"
fn.data2 <- "WDEQ_Taxa_MP_ni_20220514.xlsx"
fn.data3 <- "WDEQ_SitesData_GIS.xlsx"
fn.data4 <- "WDEQ_DiatomMetrics_BenJ.csv"
myDate <- format(Sys.Date(), "%Y%m%d")

# Read data files ####
df_msl <- read_csv(file.path(wd,input.dir, fn.data1)
                     , na = c("NA",""), trim_ws = TRUE, skip = 0
                     , col_names = TRUE, guess_max = 100000)

df_Indivs <- read_excel(file.path(wd,input.dir, fn.data2)
                        , na = c("NA",""), trim_ws = TRUE, skip = 0
                        , col_names = TRUE, guess_max = 100000)

df_GIS <- read_excel(file.path(wd,input.dir, fn.data3)
                     , na = c("NA",""), trim_ws = TRUE, skip = 0
                     , col_names = TRUE, guess_max = 100000)

# df_BenJResults <- read_csv(file.path(wd,input.dir, fn.data4)
#                              , na = c("NA",""), trim_ws = TRUE, skip = 0
#                              , col_names = TRUE, guess_max = 100000)

# BenJ samp counts ####

# df_BenJ_trim <- df_BenJResults %>%
#   select(SampID, SampType, RepNum, ni_total) %>%
#   filter(SampType == "Primary" |SampType == "Revisit") %>%
#   filter(RepNum == 0) %>%
#   rename(ni_total_BenJ = ni_total)
#
# df_BenJ_trim$SampID <- as.character(df_BenJ_trim$SampID)

# test <- df_Indivs %>%
#   filter(RepNum == 0) %>%
#   group_by(SampID) %>%
#   summarize(ni_total_BenB = sum(Count)) %>%
#   left_join(., df_BenJ_trim, by = "SampID") %>%
#   mutate(Diff = (ni_total_BenB - ni_total_BenJ))

# Trim datasets ####
df_Indivs_trim <- df_Indivs %>%
  filter(RepNum == 0) %>%
  select(STATION_CD, SampID, CollDate, Count, FinalID) %>%
  rename(STATIONID = STATION_CD
         , SAMPLEID = SampID
         , COLLDATE = CollDate
         , N_TAXA = Count
         , TAXAID = FinalID)

df_GIS_trim <- df_GIS %>%
  select(StationCode, Latitude, Longitude, Elevation
         , BFIWs, SWs, KffactWs, PrecipWs, TmaxWs, TmeanWs, RckDepWs) %>%
  rename(STATIONID = StationCode
         , LAT = Latitude
         , LONG = Longitude)

df_msl_trim <- df_msl %>%
  select(-c(TaxaID, Taxon_forUSGS_traits
            , Taxon_forTt_traits, SALINITY_USGS)) %>%
  rename(TAXAID = COMMON_NAME
         , ORDER = Order
         , FAMILY = Family)

# Combine data ####
df_combine1 <- left_join(df_Indivs_trim, df_msl_trim, by = "TAXAID")
df_combine2 <- left_join(df_combine1, df_GIS_trim, by = "STATIONID")


# Extras
df_combine3 <- df_combine2 %>%
  rename(DIATAS_TN = DIATASTN) %>%
  mutate(INDEX_REGION = "Statewide"
         , INDEX_NAME = "WY_DiatomIBI_2022"
         , EXCLUDE = FALSE
         , NONTARGET = FALSE) %>%
  select(INDEX_NAME, INDEX_REGION, STATIONID, SAMPLEID, COLLDATE, N_TAXA
         , TAXAID, EXCLUDE, NONTARGET, everything())

# Export data ####
write.table(df_combine3, file.path(wd, input.dir
                                  , paste0("WDEQ_Cal_Input_4RShiny_"
                                           , myDate,".csv"))
            , sep = ",", row.names = FALSE, col.names = TRUE, na = "")
