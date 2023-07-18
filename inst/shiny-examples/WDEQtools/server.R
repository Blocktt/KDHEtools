#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # modal dialog ####
  myModal <- modalDialog(
    title = "Greetings!"
    ,paste("Welcome to the Wyoming Diatom IBI Calculator!")
    ,br()
    ,paste("WDEQtools was developed to calculate the benthic diatom"
           ,"Index of Biotic Integrity (IBI) for streams in Wyoming."
           ,"This app was developed by Ben Block (Ben.Block@tetratech.com),"
           ,"with underlying R code written by Ben Block, Diane Allen (Diane.Allen@tetratech.com),"
           ,"and Erik W. Leppo (Erik.Leppo@tetratech.com)."
           ,"Please contact Eric Hargett (eric.hargett@wyo.gov)"
           ,"or Jeremy Zumberge (jeremy.zumberge@wyo.gov) should any issues or questions arise.")
    ,br()
    ,HTML('<center><img src="WDEQ_logo.png" height="100"></center>')
    ,easyClose = T)

  # Show the model on start up
  showModal(myModal)

    # Misc Names ####
    output$fn_input_display <- renderText({input$fn_input}) ## renderText~END


    # df_import ####
    output$df_import_DT <- renderDT({
        # input$df_import will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.


        inFile <- input$fn_input

        shiny::validate(
            need(inFile != "", "Please upload a data set") # used to inform the user that a data set is required
        )

        if (is.null(inFile)){
            return(NULL)
        }##IF~is.null~END

        # Read user imported file
        df_input <- read.csv(inFile$datapath, header = TRUE,
                             sep = input$sep,
                             quote = input$quote, stringsAsFactors = FALSE)

        required_columns <- c("INDEX_NAME"
                              ,"INDEX_REGION"
                              ,"STATIONID"
                              ,"SAMPLEID"
                              ,"COLLDATE"
                              ,"N_TAXA"
                              ,"EXCLUDE"
                              ,"NONTARGET"
                              ,"TAXAID"
                              ,"ORDER"
                              ,"FAMILY"
                              ,"H_WDEQ"
                              ,"O_WDEQ"
                              ,"T_WDEQ"
                              ,"DIATAS_TN"
                              ,"BC_USGS"
                              ,"SALINITY_USGS_NUM"
                              ,"LAT"
                              ,"LONG"
                              ,"Elevation"
                              ,"BFIWs"
                              ,"SWs"
                              ,"KffactWs"
                              ,"PrecipWs"
                              ,"TmaxWs"
                              ,"TmeanWs"
                              ,"RckDepWs")

        column_names <- colnames(df_input)

        # QC Check for column names
        col_req_match <- required_columns %in% column_names
        col_missing <- required_columns[!col_req_match]

        shiny::validate(
            need(all(required_columns %in% column_names)
                 , paste0("Error\nChoose correct data separator; otherwise, you may have missing required columns\n"
                          , paste("Required columns missing from the data:\n")
                          , paste("* ", col_missing, collapse = "\n")))
        )##END ~ validate() code

        ########################### MAP and PLOT Observer
        observe({
          inFile<- input$fn_input
          if(is.null(inFile))
            return(NULL)

          df_input

          updateSelectInput(session, "siteid.select"
                            , choices = as.character(
                              sort(unique(df_input[, "SAMPLEID"]))))
        }) ## observe~END



        # Add "Results" folder if missing
        boo_Results <- dir.exists(file.path(".", "Results"))
        if(boo_Results==FALSE){
            dir.create(file.path(".", "Results"))
        }

        # Remove all files in "Results" folder
        fn_results <- list.files(file.path(".", "Results"), full.names=TRUE)
        file.remove(fn_results)

        # Write to "Results" folder - Import as TSV
        fn_input <- file.path(".", "Results", "data_import.tsv")
        write.table(df_input, fn_input, row.names=FALSE
                    , col.names=TRUE, sep="\t")

        # Copy to "Results" folder - Import "as is"
        file.copy(input$fn_input$datapath
                  , file.path(".", "Results", input$fn_input$name))

        return(df_input)

    }##expression~END
    , filter="top", options=list(scrollX=TRUE)

    )##output$df_import_DT~END

    # b_Calc ####
    # Calculate IBI (metrics and scores) from df_import
    # add "sleep" so progress bar is readable
    observeEvent(input$b_Calc, {
        shiny::withProgress({
            #
            # Number of increments
            n_inc <- 8

            # sink output
            file_sink <- file(file.path(".", "Results", "results_log.txt")
                              , open = "wt")
            sink(file_sink, type = "output", append = TRUE)
            sink(file_sink, type = "message", append = TRUE)

            # Log
            message("Results Log from WDEQtools Shiny App")
            message(Sys.time())
            inFile <- input$fn_input
            message(paste0("file = ", inFile$name))

            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Data, Initialize")
            Sys.sleep(0.25)

            # Read in saved file (known format)
            df_data <- NULL  # set as null for IF QC check prior to import
            fn_input <- file.path(".", "Results", "data_import.tsv")
            df_data <- read.delim(fn_input, stringsAsFactors = FALSE, sep="\t")

            # QC, FAIL if TRUE
            if (is.null(df_data)){
                return(NULL)
            }

            # QC, N_TAXA = 0
            N_Taxa_zeros <- sum(df_data$N_TAXA == 0, na.rm = TRUE)
            if(N_Taxa_zeros>0){
                message("Some taxa in your dataset have a count (N_TAXA) of zero. Values for TAXAID with N_TAXA = 0 will be removed before calculations.")
            }

            # QC, predictors = NA
            Elevation_NA <- sum(is.na(df_data$Elevation))
            BFIWs_NA <- sum(is.na(df_data$BFIWs))
            SWs_NA <- sum(is.na(df_data$SWs))
            KffactWs_NA <- sum(is.na(df_data$KffactWs))
            PrecipWs_NA <- sum(is.na(df_data$PrecipWs))
            TmaxWs_NA <- sum(is.na(df_data$TmaxWs))
            TmeanWs_NA <- sum(is.na(df_data$TmeanWs))
            RckDepWs_NA <- sum(is.na(df_data$RckDepWs))

            Predictor_NA <- sum(Elevation_NA, BFIWs_NA, SWs_NA, KffactWs_NA
                                , PrecipWs_NA, TmaxWs_NA, TmeanWs_NA, RckDepWs_NA)
            if(Predictor_NA>0){
              message("Some sites in your dataset have missing predictor values. Index scores will be incorrect without COMPLETE predictor values.")
            }

            # QC, Exclude as TRUE/FALSE
            Exclude.T <- sum(df_data$EXCLUDE==TRUE, na.rm=TRUE)
            if(Exclude.T==0){##IF.Exclude.T.START
                message("EXCLUDE column does not have any TRUE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
            }##IF.Exclude.T.END

            # QC, NonTarget as TRUE/FALSE
            NonTarget.F <- sum(df_data$NONTARGET==FALSE, na.rm=TRUE)
            if(NonTarget.F==0){##IF.Exclude.T.START
                message("NONTARGET column does not have any FALSE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
            }##IF.Exclude.T.END


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Calculate, Metrics (takes ~ 30-45s)")
            Sys.sleep(0.5)

            # convert Field Names to UPPER CASE
            names(df_data) <- toupper(names(df_data))

            # Reformat dataset ####
            df_reformat <- df_data %>%
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
            warning(paste("Metrics related to the following fields are invalid:"
                          , paste(paste0("   ", col.req.missing), collapse="\n"), sep="\n"))

            # calculate values and scores in two steps using BioMonTools
            # save each file separately

           # columns to keep
            keep_cols <- c("STATIONID", "COLLDATE", "LAT", "LONG","Elevation"
                           , "BFIWs", "SWs", "KffactWs", "PrecipWs", "TmaxWs", "TmeanWs"
                           , "RckDepWs")

            # metric calculation ####

            df_metval <- suppressWarnings(
              BioMonTools::metric.values(fun.DF = df_reformat
                            , fun.Community = "algae"
                            , fun.MetricNames = DiatomMetrics
                            , fun.cols2keep= keep_cols
                            , boo.Shiny = TRUE))

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

            std_Parameters<-read.csv("./data/standardization.parameters.csv",row.names=1)

            H34_model<-load("./data/pt_H_WDEQ_34_RFmod02162022.Rdata")
            pt_H_WDEQ_34_pred<-predict(rFmodel,df_metval2[,c(predictors)])				##### use forest to predict pt_H_WDEQ_34
            pt_H_WDEQ_34_RFadj<-df_metval2[,"pt_H_WDEQ_34"] - pt_H_WDEQ_34_pred				##### calculate residual
            df_metval2$pt_H_WDEQ_34_RFadj<-pt_H_WDEQ_34_RFadj

            T56_model<-load("./data/pt_T_WDEQ_56_RFmod02162022.Rdata")
            pt_T_WDEQ_56_pred<-predict(rFmodel,df_metval2[,c(predictors)])				##### pt_T_WDEQ_56_RFadj
            pt_T_WDEQ_56_RFadj<-df_metval2[,"pt_T_WDEQ_56"] - pt_T_WDEQ_56_pred
            df_metval2$pt_T_WDEQ_56_RFadj<-pt_T_WDEQ_56_RFadj

            DiatasTN2_model<-load("./data/nt_Diatas_TN_2_RFmod02162022.Rdata")
            nt_Diatas_TN_2_pred<-predict(rFmodel,df_metval2[,c(predictors)])				##### nt_Diatas_TN_2_RFadj
            nt_Diatas_TN_2_RFadj<-df_metval2[,"nt_Diatas_TN_2"] - nt_Diatas_TN_2_pred
            df_metval2$nt_Diatas_TN_2_RFadj<-nt_Diatas_TN_2_RFadj

            T12_model<-load("./data/pt_T_WDEQ_12_RFmod02162022.Rdata")
            pt_T_WDEQ_12_pred<-predict(rFmodel,df_metval2[,c(predictors)])				##### pt_T_WDEQ_12_RFadj
            pt_T_WDEQ_12_RFadj<-df_metval2[,"pt_T_WDEQ_12"] - pt_T_WDEQ_12_pred
            df_metval2$pt_T_WDEQ_12_RFadj<-pt_T_WDEQ_12_RFadj


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Metrics have been calculated!")
            Sys.sleep(1)

            #
            # Save
            fn_metval <- file.path(".", "Results", "results_metval.csv")
            write.csv(df_metval2, fn_metval, row.names = FALSE)

            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Adjust, Metrics")

            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Calculate, Scores")
            Sys.sleep(0.50)

            # metric scoring ####
            # Increasers
            metricsIncreasers<-df_metval2[,c("SAMPLEID", increasers)]

            metricsIncreasers2<-data.frame(matrix(ncol = 5, nrow = dim(df_metval2)[1]))
            colnames(metricsIncreasers2) <- c("SAMPLEID",paste0(increasers,"_std"))

            metricsIncreasers2[,1]<-metricsIncreasers$SAMPLEID
            metricsIncreasers2[,2]<-100*(std_Parameters["ninetififth",names(metricsIncreasers)[2]] - metricsIncreasers$WA_Salinity_USGS)/(std_Parameters["ninetififth",names(metricsIncreasers)[2]] - std_Parameters["fifth",names(metricsIncreasers)[2]])
            metricsIncreasers2[,3]<-100*(std_Parameters["ninetififth",names(metricsIncreasers)[3]] - metricsIncreasers$pt_O_WDEQ_4)/(std_Parameters["ninetififth",names(metricsIncreasers)[3]] - std_Parameters["fifth",names(metricsIncreasers)[3]])
            metricsIncreasers2[,4]<-100*(std_Parameters["ninetififth",names(metricsIncreasers)[4]] - metricsIncreasers$pt_H_WDEQ_34_RFadj)/(std_Parameters["ninetififth",names(metricsIncreasers)[4]] - std_Parameters["fifth",names(metricsIncreasers)[4]])
            metricsIncreasers2[,5]<-100*(std_Parameters["ninetififth",names(metricsIncreasers)[5]] - metricsIncreasers$pt_T_WDEQ_56_RFadj)/(std_Parameters["ninetififth",names(metricsIncreasers)[5]] - std_Parameters["fifth",names(metricsIncreasers)[5]])

            # Decreasers
            metricsDecreasers<-df_metval2[,c("SAMPLEID", decreasers)]

            metricsDecreasers2<-data.frame(matrix(ncol = 4, nrow = dim(df_metval2)[1]))
            colnames(metricsDecreasers2) <- c("SAMPLEID",paste0(decreasers,"_std"))

            metricsDecreasers2[,1]<-metricsDecreasers$SAMPLEID
            metricsDecreasers2[,2]<-100*(metricsDecreasers$pt_T_WDEQ_12_RFadj - std_Parameters["fifth",names(metricsDecreasers)[2]])/(std_Parameters["ninetififth",names(metricsDecreasers)[2]] - std_Parameters["fifth",names(metricsDecreasers)[2]])
            metricsDecreasers2[,3]<-100*(metricsDecreasers$nt_Diatas_TN_2_RFadj - std_Parameters["fifth",names(metricsDecreasers)[3]])/(std_Parameters["ninetififth",names(metricsDecreasers)[3]] - std_Parameters["fifth",names(metricsDecreasers)[3]])
            metricsDecreasers2[,4]<-100*(metricsDecreasers$BC_12.pa - std_Parameters["fifth",names(metricsDecreasers)[4]])/(std_Parameters["ninetififth",names(metricsDecreasers)[4]] - std_Parameters["fifth",names(metricsDecreasers)[4]])

            # combine and truncate at 0 and 100
            metrics_std <- suppressWarnings(left_join(metricsDecreasers2, metricsIncreasers2
                                     , by = "SAMPLEID") %>%
              mutate_if(is.numeric, funs(ifelse(.>100,100,.))) %>%
              mutate_if(is.numeric, funs(ifelse(.<0,0,.))))

            ## calculate index
            metrics_std <- metrics_std %>%
              mutate(Index_Score = rowMeans(select(., ends_with("_std")), na.rm = TRUE))

            ## Final Table
            df_metsc <- left_join(df_metval2, metrics_std
                                  , by = "SAMPLEID")


            # Save
            fn_metsc <- file.path(".", "Results", "results_metsc.csv")
            write.csv(df_metsc, fn_metsc, row.names = FALSE)

            # Increment the progress bar, and update the detail text.
            # incProgress(1/n_inc, detail = "Create, summary report (~ 20 - 40 sec)")
            # Sys.sleep(0.75)

            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Ben's code is magical!")
            Sys.sleep(0.75)


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Create, Zip")
            Sys.sleep(0.50)

            # Create zip file
            fn_4zip <- list.files(path = file.path(".", "Results")
                                  , pattern = "^results_"
                                  , full.names = TRUE)
            zip(file.path(".", "Results", "results.zip"), fn_4zip)

            # enable download button
            shinyjs::enable("b_downloadData")

            # #
            # return(myMetric.Values)
            # end sink
            #flush.console()
            sink() # console
            sink() # message
            #
        }##expr~withProgress~END
        , message = "Calculating IBI"
        )##withProgress~END
    }##expr~ObserveEvent~END
    )##observeEvent~b_CalcIBI~END


    # Downloadable csv of selected dataset
    output$b_downloadData <- downloadHandler(
        # use index and date time as file name

        filename = function() {
            paste("WY_Diatom_IBI_Results_"
                  , format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip", sep = "")
        },
        content = function(fname) {##content~START

            # Create Zip file
            file.copy(file.path(".", "Results", "results.zip"), fname)

            #
        }##content~END
        #, contentType = "application/zip"
    )##downloadData~END

    # Metric Adjustment Factors ####

    ## df_import ####
    output$df_site_import_DT <- renderDT({
      # input$df_import will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.

      inFile <- input$fn_input_MetAdjFact

      # shiny::validate(
      #   need(inFile != "", "Please select a data set") # used to inform the user that a data set is required
      # )

      if (is.null(inFile)){
        return(NULL)
      }##IF~is.null~END

      # Read user imported file
      df_input_maf <- read.csv(inFile$datapath, header = TRUE,
                           sep = input$sep_sci,
                           quote = input$quote_sci, stringsAsFactors = FALSE)

      required_columns <- c("COMID"
                            ,"STATIONID")

      column_names <- colnames(df_input_maf)

      # QC Check for column names
      col_req_match <- required_columns %in% column_names
      col_missing <- required_columns[!col_req_match]

      shiny::validate(
        need(all(required_columns %in% column_names)
             , paste0("Error\nChoose correct data separator; otherwise, you may have missing required columns\n"
                      , paste("Required columns missing from the data:\n")
                      , paste("* ", col_missing, collapse = "\n")))
      )##END ~ validate() code

      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(".", "Results_MetAdjFact"))
      if(boo_Results==FALSE){
        dir.create(file.path(".", "Results_MetAdjFact"))
      }

      # Remove all files in "Results" folder
      fn_results <- list.files(file.path(".", "Results_MetAdjFact"), full.names=TRUE)
      file.remove(fn_results)

      # Write to "Results" folder - Import as TSV
      fn_input <- file.path(".", "Results_MetAdjFact", "data_import_MetAdjFact.tsv")
      write.table(df_input_maf, fn_input, row.names=FALSE
                  , col.names=TRUE, sep="\t")

      # Copy to "Results" folder - Import "as is"
      file.copy(input$fn_input_MetAdjFact$datapath
                , file.path(".", "Results_MetAdjFact", input$fn_input_MetAdjFact$name))

      return(df_input_maf)

    }##expression~END
    , filter="top", options=list(scrollX=TRUE)

    )##output$df_import_DT~END


    ## c_Calc ####
    observeEvent(input$c_Calc, {
      shiny::withProgress({
        #
        # Number of increments
        n_inc <- 4

        # sink output
        file_sink <- file(file.path(".", "Results_MetAdjFact", "results_log_maf.txt")
                          , open = "wt")
        sink(file_sink, type = "output", append = TRUE)
        sink(file_sink, type = "message", append = TRUE)

        # Log
        message("Results Log from WDEQtools Metric Adjustment Factors")
        message(Sys.time())
        inFile <- input$fn_input_MetAdjFact
        message(paste0("file = ", inFile$name))

        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Data, Initialize")
        Sys.sleep(0.25)

        # Read in saved file (known format)
        df_data <- NULL  # set as null for IF QC check prior to import
        fn_input <- file.path(".", "Results_MetAdjFact", "data_import_MetAdjFact.tsv")
        df_data <- read.delim(fn_input, stringsAsFactors = FALSE, sep="\t")

        # QC, FAIL if TRUE
        if (is.null(df_data)){
          return(NULL)
        }

        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Join with COMIDs")
        Sys.sleep(0.5)

        # convert Field Names to UPPER CASE
        names(df_data) <- toupper(names(df_data))

        # Read in saved COMID file
        df_COMIDs <- NULL  # set as null for IF QC check prior to import
        fn_input <- file.path(".", "Extras", "tables"
                              , "WY_StreamCat_MetAdjFactors_20220603.csv")
        df_COMIDs <- read.csv(fn_input, stringsAsFactors = FALSE, sep=",")


        ### Join tables ####

        df_combined <- dplyr::left_join(df_data, df_COMIDs
                                         , by = "COMID")


        ### QC Results ####

        N_MetAdjFact_NoData <- sum(df_combined$Missing_Data == TRUE, na.rm = TRUE)
        if(N_MetAdjFact_NoData>0){
          message(paste("Some COMIDs having missing StreamCat data.\n")
                  ,paste("Number of sites with missing data:", N_MetAdjFact_NoData,"\n")
                  ,paste("Please check that input COMIDs are correct.\n")
                  ,paste("If problem persists, estimate metric adjustment factor from nearby COMID.\n")
                  ,paste("Contact Ben Block (Ben.Block@tetratech.com) with any questions.")
          )# message~ END
        }#IF statement ~END

        N_MetAdjFact_NA <- sum(is.na(df_combined$Missing_Data))
        if(N_MetAdjFact_NA>0){
          message(paste("Some input COMIDs did not match with the metric adjustment factors table.\n")
                  ,paste("Number of mismatches:", N_MetAdjFact_NA,"\n")
                  ,paste("Please check that input COMIDs are correct.\n")
                  ,paste("Contact Ben Block (Ben.Block@tetratech.com) if problem persists.")
          )# message~ END
        }#IF statement ~END

        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Join Completed!")
        Sys.sleep(1)

        # Save
        fn_MetAdjFact <- file.path(".", "Results_MetAdjFact"
                               , "results_MetAdjFact.csv")
        write.csv(df_combined, fn_MetAdjFact, row.names = FALSE)

        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Create, Zip")
        Sys.sleep(0.50)

        # Create zip file
        fn_4zip <- list.files(path = file.path(".", "Results_MetAdjFact")
                              , pattern = "^results_"
                              , full.names = TRUE)
        zip(file.path(".", "Results_MetAdjFact", "results.zip"), fn_4zip)

        # enable download button
        shinyjs::enable("c_downloadData")

        sink() # console
        sink() # message
        #
      }##expr~withProgress~END
      )##withProgress~END
    }##expr~ObserveEvent~END
    )##observeEvent~C_CalcIBI~END


    ## Download dataset ####
    output$c_downloadData <- downloadHandler(
      # file name

      filename = function() {
        paste("Metric_Adjustment_Factor_Results_"
              , format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip", sep = "")
      },
      content = function(fname) {##content~START

        # Create Zip file
        file.copy(file.path(".", "Results_MetAdjFact", "results.zip"), fname)

        #
      }##content~END
    )##downloadData~END
})##shinyServer~END
