#### R script that tests metric calculation and adjustment for WDEQ R Shiny app
#
# Ben.Block@tetratech.com, Date completed: 2022-05-27
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.1.2 (2021-11-01) -- "Bird Hippie"

#clear global environment
rm(list=ls())

# load libraries
library(tidyverse)
library(randomForest)

# Declare directories ####
wd <- getwd()
input.dir <- "TaxaTraits"
fn.data1 <- "WDEQ_Cal_Input_4RShiny_20220603.csv"
myDate <- format(Sys.Date(), "%Y%m%d")

# Read data files ####
df_input <- read_csv(file.path(wd,input.dir, fn.data1)
                     , na = c("NA",""), trim_ws = TRUE, skip = 0
                     , col_names = TRUE, guess_max = 100000)

df_input <- as.data.frame(df_input)

# Reformat dataset ####
df_reformat <- df_input %>%
  mutate(SALINITY_USGS = case_when((H_WDEQ == 1) ~ "SALINITY_1"
                                   , (H_WDEQ == 2) ~ "SALINITY_2"
                                   , (H_WDEQ == 3) ~ "SALINITY_3"
                                   , (H_WDEQ == 4) ~ "SALINITY_4")) %>%
  select(-c(H_WDEQ)) %>%
  mutate(TROPHIC_USGS = case_when((T_WDEQ == 1) ~ "TROPHIC_1"
                                  , (T_WDEQ == 2) ~ "TROPHIC_2"
                                  , (T_WDEQ == 3) ~ "TROPHIC_3"
                                  , (T_WDEQ == 4) ~ "TROPHIC_4"
                                  , (T_WDEQ == 5) ~ "TROPHIC_5"
                                  , (T_WDEQ == 6) ~ "TROPHIC_6"
                                  , (T_WDEQ == 7) ~ "TROPHIC_7")) %>%
  select(-c(T_WDEQ)) %>%
  mutate(O_USGS = case_when((O_WDEQ == 1) ~ "O_1"
                            , (O_WDEQ == 2) ~ "O_2"
                            , (O_WDEQ == 3) ~ "O_3"
                            , (O_WDEQ == 4) ~ "O_4"
                            , (O_WDEQ == 5) ~ "O_5")) %>%
  select(-c(O_WDEQ)) %>%
  rename(POLL_TOL = SALINITY_USGS_NUM)

# metric calc ####
# QC, Required Fields
col.req <- c("INDEX_REGION","SAMPLEID","TAXAID","EXCLUDE","NONTARGET"
             ,"N_TAXA","PHYLUM","ORDER","FAMILY","GENUS","BC_USGS"
             ,"TROPHIC_USGS","SAP_USGS","PT_USGS","O_USGS","SALINITY_USGS"
             ,"BAHLS_USGS","P_USGS","N_USGS","HABITAT_USGS","N_FIXER_USGS"
             ,"MOTILITY_USGS","SIZE_USGS","HABIT_USGS","MOTILE2_USGS"
             ,"TOLVAL","DIATOM_ISA","DIAT_CL","POLL_TOL","BEN_SES"
             ,"DIATAS_TP","DIATAS_TN","DIAT_COND","DIAT_CA","MOTILITY"
             ,"NF")
col.req.missing <- col.req[!(col.req %in% toupper(names(df_reformat)))]

# Add missing fields
df_reformat[,col.req.missing] <- NA

# calculations
DiatomMetrics <- c("pi_BC_12"
                   ,"pt_SALINITY_34"
                   ,"wa_POLL_TOL"
                   ,"nt_DIATAS_TN_2"
                   ,"pt_TROPHIC_12"
                   ,"pt_TROPHIC_56"
                   ,"pt_O_4")# END DiatomMetrics

keep_cols <- c("STATIONID", "COLLDATE", "LAT", "LONG","Elevation"
               , "BFIWs", "SWs", "KffactWs", "PrecipWs", "TmaxWs", "TmeanWs"
               , "RckDepWs")

df_metval <- BioMonTools::metric.values(fun.DF = df_reformat
                , fun.Community = "algae"
                , fun.MetricNames = DiatomMetrics
                , fun.cols2keep= keep_cols
                , boo.Shiny = FALSE)

df_metval2 <- df_metval %>%
  rename(pt_H_WDEQ_34 = pt_SALINITY_34
         , WA_Salinity_USGS = wa_POLL_TOL
         , nt_Diatas_TN_2 = nt_DIATAS_TN_2
         , pt_T_WDEQ_12 = pt_TROPHIC_12
         , pt_T_WDEQ_56 = pt_TROPHIC_56
         , pt_O_WDEQ_4 = pt_O_4
         , BFIWs = BFIWS
         , Elevation = ELEVATION
         , SWs = SWS
         , KffactWs = KFFACTWS
         , PrecipWs = PRECIPWS
         , TmaxWs = TMAXWS
         , TmeanWs = TMEANWS
         , RckDepWs = RCKDEPWS) %>%
  mutate(BC_12.pa = pi_BC_12/100) %>% # return to proportion values
  select(-c(pi_BC_12))

df_metval2$SAMPLEID <- as.character(df_metval2$SAMPLEID)


## adjust metrics ####
std_Parameters<-read.csv("./inst/shiny-examples/WDEQtools/data/standardization.parameters.csv",row.names=1)

H34_model<-load("./inst/shiny-examples/WDEQtools/data/pt_H_WDEQ_34_RFmod02162022.Rdata")
pt_H_WDEQ_34_pred<-predict(rFmodel,df_metval2[,c(predictors)])				##### use forest to predict pt_H_WDEQ_34
pt_H_WDEQ_34_RFadj<-df_metval2[,"pt_H_WDEQ_34"] - pt_H_WDEQ_34_pred				##### calculate residual
df_metval2$pt_H_WDEQ_34_RFadj<-pt_H_WDEQ_34_RFadj

T56_model<-load("./inst/shiny-examples/WDEQtools/data/pt_T_WDEQ_56_RFmod02162022.Rdata")
pt_T_WDEQ_56_pred<-predict(rFmodel,df_metval2[,c(predictors)])				##### pt_T_WDEQ_56_RFadj
pt_T_WDEQ_56_RFadj<-df_metval2[,"pt_T_WDEQ_56"] - pt_T_WDEQ_56_pred
df_metval2$pt_T_WDEQ_56_RFadj<-pt_T_WDEQ_56_RFadj

DiatasTN2_model<-load("./inst/shiny-examples/WDEQtools/data/nt_Diatas_TN_2_RFmod02162022.Rdata")
nt_Diatas_TN_2_pred<-predict(rFmodel,df_metval2[,c(predictors)])				##### nt_Diatas_TN_2_RFadj
nt_Diatas_TN_2_RFadj<-df_metval2[,"nt_Diatas_TN_2"] - nt_Diatas_TN_2_pred
df_metval2$nt_Diatas_TN_2_RFadj<-nt_Diatas_TN_2_RFadj

T12_model<-load("./inst/shiny-examples/WDEQtools/data/pt_T_WDEQ_12_RFmod02162022.Rdata")
pt_T_WDEQ_12_pred<-predict(rFmodel,df_metval2[,c(predictors)])				##### pt_T_WDEQ_12_RFadj
pt_T_WDEQ_12_RFadj<-df_metval2[,"pt_T_WDEQ_12"] - pt_T_WDEQ_12_pred
df_metval2$pt_T_WDEQ_12_RFadj<-pt_T_WDEQ_12_RFadj

## standardize metrics ####
# Decreasers
metricsDecreasers<-df_metval2[,c("SAMPLEID", decreasers)]

metricsDecreasers2<-data.frame(matrix(ncol = 5, nrow = dim(df_metval2)[1]))
colnames(metricsDecreasers2) <- c("SAMPLEID",paste0(decreasers,"_std"))

metricsDecreasers2[,1]<-metricsDecreasers$SAMPLEID
metricsDecreasers2[,2]<-100*(std_Parameters["ninetififth",names(metricsDecreasers)[2]] - metricsDecreasers$WA_Salinity_USGS)/(std_Parameters["ninetififth",names(metricsDecreasers)[2]] - std_Parameters["fifth",names(metricsDecreasers)[2]])
metricsDecreasers2[,3]<-100*(std_Parameters["ninetififth",names(metricsDecreasers)[3]] - metricsDecreasers$pt_O_WDEQ_4)/(std_Parameters["ninetififth",names(metricsDecreasers)[3]] - std_Parameters["fifth",names(metricsDecreasers)[3]])
metricsDecreasers2[,4]<-100*(std_Parameters["ninetififth",names(metricsDecreasers)[4]] - metricsDecreasers$pt_H_WDEQ_34_RFadj)/(std_Parameters["ninetififth",names(metricsDecreasers)[4]] - std_Parameters["fifth",names(metricsDecreasers)[4]])
metricsDecreasers2[,5]<-100*(std_Parameters["ninetififth",names(metricsDecreasers)[5]] - metricsDecreasers$pt_T_WDEQ_56_RFadj)/(std_Parameters["ninetififth",names(metricsDecreasers)[5]] - std_Parameters["fifth",names(metricsDecreasers)[5]])

# Increasers
metricsIncreasers<-df_metval2[,c("SAMPLEID", increasers)]

metricsIncreasers2<-data.frame(matrix(ncol = 4, nrow = dim(df_metval2)[1]))
colnames(metricsIncreasers2) <- c("SAMPLEID",paste0(increasers,"_std"))

metricsIncreasers2[,1]<-metricsIncreasers$SAMPLEID
metricsIncreasers2[,2]<-100*(metricsIncreasers$pt_T_WDEQ_12_RFadj - std_Parameters["fifth",names(metricsIncreasers)[2]])/(std_Parameters["ninetififth",names(metricsIncreasers)[2]] - std_Parameters["fifth",names(metricsIncreasers)[2]])
metricsIncreasers2[,3]<-100*(metricsIncreasers$nt_Diatas_TN_2_RFadj - std_Parameters["fifth",names(metricsIncreasers)[3]])/(std_Parameters["ninetififth",names(metricsIncreasers)[3]] - std_Parameters["fifth",names(metricsIncreasers)[3]])
metricsIncreasers2[,4]<-100*(metricsIncreasers$BC_12.pa - std_Parameters["fifth",names(metricsIncreasers)[4]])/(std_Parameters["ninetififth",names(metricsIncreasers)[4]] - std_Parameters["fifth",names(metricsIncreasers)[4]])

# combine and truncate at 0 and 100
metrics_std <- left_join(metricsDecreasers2, metricsIncreasers2
                         , by = "SAMPLEID") %>%
  mutate_if(is.numeric, funs(ifelse(.>100,100,.))) %>%
  mutate_if(is.numeric, funs(ifelse(.<0,0,.)))

## calculate index ####
metrics_std <- metrics_std %>%
  mutate(Index_Score = rowMeans(select(., ends_with("_std")), na.rm = TRUE))

## Final Table ####
df_final <- left_join(df_metval2, metrics_std
                      , by = "SAMPLEID")
# export data ####
write.table(df_final
            , file.path(wd, input.dir
                        , paste0("WY_Diatom_BenB_IBI_Calc_",myDate, ".csv"))
            , sep = ",", row.names = FALSE, col.names = TRUE, na = "")
