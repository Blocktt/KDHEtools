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
    ,paste("Welcome to the Macroinvertebrate Multi-Metric Index (MMI) Calculator for Wadeable Streams in Kansas!")
    ,br()
    ,paste("KDHEtools was developed to calculate the benthic macroinvertebrate"
           ,"Index of Biotic Integrity (IBI) for streams in Kansas."
           ,"This app was developed by Ben Block (Ben.Block@tetratech.com),"
           ,"with underlying R code written by Ben Block, Diane Allen (Diane.Allen@tetratech.com),"
           ,"and Erik W. Leppo (Erik.Leppo@tetratech.com)."
           ,"Please contact Tony Stahl (Anthony.Stahl@ks.gov) and Elizabeth"
           , "Smith (Elizabeth.Smith@ks.gov) should any issues or questions arise.")
    ,br()
    ,HTML('<center><img src="KDHE_logo.png" height="100"></center>')
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

        required_columns <- c("Index_Name", "SampleID", "CollDate", "CollMeth"
                              , "StationID", "Lat", "Long", "TaxaID"
                              , "N_Taxa", "Exclude", "Nontarget", "BCG_Attr"
                              , "Habit", "Life_Cycle", "TolVal", "Phylum"
                              , "Order", "Genus", "Al2O3Ws", "CFS", "ClayWs"
                              , "ElevCat", "Fe2O3Cat", "K2OWs", "L3Eco", "MgOCat"
                              , "NWs", "PermWs", "PrecipCat", "PrecipWs", "SandWs"
                              , "SWs", "TmeanCat", "WetIndexWs", "WsAreaSqKm"
                              , "WtDepWs")

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
            message("Results Log from KDHEtools Shiny App")
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

            # QC, Index Period
            QC_CollMonth <- df_data %>%
              mutate(CollMonth = lubridate::month(CollDate)) %>%
              pull(CollMonth)

            N_OutIndexPeriod <- sum(QC_CollMonth < 4 | QC_CollMonth >10
                                    , na.rm = TRUE)

            if(N_OutIndexPeriod>0){
              message("Some samples in your dataset were collected outside of the index period (April through October).")
            }

            # QC, predictors = NA
            Al2O3Ws_NA <- sum(is.na(df_data$Al2O3Ws))
            CFS_NA <- sum(is.na(df_data$CFS))
            ClayWs_NA <- sum(is.na(df_data$ClayWs))
            ElevCat_NA <- sum(is.na(df_data$ElevCat))
            Fe2O3Cat_NA <- sum(is.na(df_data$Fe2O3Cat))
            K2OWs_NA <- sum(is.na(df_data$K2OWs))
            L3Eco_NA <- sum(is.na(df_data$L3Eco))
            MgOCat_NA <- sum(is.na(df_data$MgOCat))
            NWs_NA <- sum(is.na(df_data$NWs))
            PermWs_NA <- sum(is.na(df_data$PermWs))
            PrecipCat_NA <- sum(is.na(df_data$PrecipCat))
            PrecipWs_NA <- sum(is.na(df_data$PrecipWs))
            SandWs_NA <- sum(is.na(df_data$SandWs))
            SWs_NA <- sum(is.na(df_data$SWs))
            TmeanCat_NA <- sum(is.na(df_data$TmeanCat))
            WetIndexWs_NA <- sum(is.na(df_data$WetIndexWs))
            WsAreaSqKm_NA <- sum(is.na(df_data$WsAreaSqKm))
            WtDepWs_NA <- sum(is.na(df_data$WtDepWs))

            Predictor_NA <- sum(Al2O3Ws_NA, CFS_NA, ClayWs_NA, ElevCat_NA
                                , Fe2O3Cat_NA, K2OWs_NA, L3Eco_NA, MgOCat_NA
                                , NWs_NA, PermWs_NA, PrecipCat_NA, PrecipWs_NA
                                , SandWs_NA, SWs_NA, TmeanCat_NA, WetIndexWs_NA
                                , WsAreaSqKm_NA, WtDepWs_NA)
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

            # QC, Required Fields
            col.req <- c("INDEX_NAME", "SAMPLEID", "COLLDATE", "COLLMETH"
                         , "STATIONID", "LAT", "LONG", "TAXAID"
                         , "N_TAXA", "EXCLUDE", "NONTARGET", "BCG_ATTR", "HABIT"
                         , "LIFE_CYCLE", "TOLVAL", "PHYLUM", "ORDER", "GENUS"
                         , "AL2O3WS", "CFS", "CLAYWS", "ELEVCAT", "FE2O3CAT"
                         , "K2OWS", "L3ECO", "MGOCAT", "NWS", "PERMWS"
                         , "PRECIPCAT", "PRECIPWS", "SANDWS", "SWS", "TMEANCAT"
                         , "WETINDEXWS", "WSAREASQKM", "WTDEPWS")

            col.req.missing <- col.req[!(col.req %in% names(df_data))]

            # Add missing fields
            df_data[,col.req.missing] <- NA
            warning(paste("Metrics related to the following fields are invalid:"
                          , paste(paste0("   ", col.req.missing), collapse="\n"), sep="\n"))

            # calculate values and scores in two steps using BioMonTools
            # save each file separately

           # columns to keep
            keep_cols <- c("COLLDATE", "COLLMETH", "STATIONID", "LAT", "LONG"
                           , "AL2O3WS", "CFS", "CLAYWS", "ELEVCAT"
                           , "FE2O3CAT", "K2OWS", "L3ECO", "MGOCAT", "NWS"
                           , "PERMWS", "PRECIPCAT", "PRECIPWS", "SANDWS", "SWS"
                           , "TMEANCAT", "WETINDEXWS", "WSAREASQKM", "WTDEPWS")

            # metric calculation ####

            df_metval <- suppressWarnings(
              BioMonTools::metric.values(fun.DF = df_data
                            , fun.Community = "bugs"
                            , fun.MetricNames = BugMetrics
                            , fun.cols2keep= keep_cols
                            , boo.Shiny = TRUE))

            df_metval2 <- df_metval %>%
              rename(Al2O3Ws = AL2O3WS
                     , ClayWs = CLAYWS
                     , ElevCat = ELEVCAT
                     , Fe2O3Cat = FE2O3CAT
                     , K2OWs = K2OWS
                     , L3Eco = L3ECO
                     , MgOCat = MGOCAT
                     , NWs = NWS
                     , PermWs = PERMWS
                     , PrecipCat = PRECIPCAT
                     , PrecipWs = PRECIPWS
                     , SandWs = SANDWS
                     , SWs = SWS
                     , TmeanCat = TMEANCAT
                     , WetIndexWs = WETINDEXWS
                     , WsAreaSqKm = WSAREASQKM
                     , WtDepWs = WTDEPWS)

            df_metval2$SAMPLEID <- as.character(df_metval2$SAMPLEID)
            df_metval2$L3Eco <- as.factor(df_metval2$L3Eco)

            ## adjust metrics ####

            std_Parameters<-read.csv("./data/StandardizationParameters0517.csv",row.names=1)

            habit_model<-load("./data/nt_habit_climbcling_RFmod_final0517.Rdata")
            nt_habit_climbcling_pred<-predict(rf.mod,df_metval2[,c(predictors)])	##### use forest to predict nt_habit_climbcling
            nt_habit_climbcling_RFadj<-df_metval2[,"nt_habit_climbcling"] - nt_habit_climbcling_pred		##### calculate residual
            nt_habit_climbcling_RFadj <- unname(unlist(nt_habit_climbcling_RFadj))
            df_metval2$nt_habit_climbcling_RFadj <- nt_habit_climbcling_RFadj

            HBI_model<-load("./data/x_HBI_RFmod_final0517.Rdata")
            x_HBI_pred<-predict(rf.mod,df_metval2[,c(predictors)])			##### use forest to predict
            x_HBI_RFadj<-df_metval2[,"x_HBI"] - x_HBI_pred				##### calculate residual
            x_HBI_RFadj <- unname(unlist(x_HBI_RFadj))
            df_metval2$x_HBI_RFadj<-x_HBI_RFadj

            BCG_model<-load("./data/pt_BCG_att1i234b_RFmod_final0517.Rdata")
            pt_BCG_att1i234b_pred<-predict(rf.mod,df_metval2[,c(predictors)])					##### use forest to predict
            pt_BCG_att1i234b_RFadj<-df_metval2[,"pt_BCG_att1i234b"] - pt_BCG_att1i234b_pred		##### calculate residual
            pt_BCG_att1i234b_RFadj <- unname(unlist(pt_BCG_att1i234b_RFadj))
            df_metval2$pt_BCG_att1i234b_RFadj<-pt_BCG_att1i234b_RFadj

            semiv_model<-load("./data/nt_volt_semi_RFmod_final0517.Rdata")
            nt_volt_semi_pred<-predict(rf.mod,df_metval2[,c(predictors)])	##### use forest to predict pt_H_WDEQ_34
            nt_volt_semi_RFadj<-df_metval2[,"nt_volt_semi"] - nt_volt_semi_pred				##### calculate residual
            nt_volt_semi_RFadj <- unname(unlist(nt_volt_semi_RFadj))
            df_metval2$nt_volt_semi_RFadj<-nt_volt_semi_RFadj

            EPT_model<-load("./data/nt_EPT_RFmod_final0517.Rdata")
            nt_EPT_pred<-predict(rf.mod,df_metval2[,c(predictors)])	##### use forest to predict pt_H_WDEQ_34
            nt_EPT_RFadj<-df_metval2[,"nt_EPT"] - nt_EPT_pred				##### calculate residual
            nt_EPT_RFadj <- unname(unlist(nt_EPT_RFadj))
            df_metval2$nt_EPT_RFadj<-nt_EPT_RFadj

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

            metricsIncreasers2<-data.frame(matrix(ncol = 2, nrow = dim(df_metval2)[1]))
            colnames(metricsIncreasers2) <- c("SAMPLEID",paste0(increasers,"_std"))

            metricsIncreasers2[,1]<-metricsIncreasers$SAMPLEID
            metricsIncreasers2[,2]<-100*(std_Parameters["ninetyfifths","x_HBI_RFadj"] - metricsIncreasers$x_HBI_RFadj)/(std_Parameters["ninetyfifths","x_HBI_RFadj"] - std_Parameters["fifths","x_HBI_RFadj"])

            # Decreasers
            metricsDecreasers<-df_metval2[,c("SAMPLEID", decreasers)]

            metricsDecreasers2<-data.frame(matrix(ncol = 5, nrow = dim(df_metval2)[1]))
            colnames(metricsDecreasers2) <- c("SAMPLEID",paste0(decreasers,"_std"))

            metricsDecreasers2[,1]<-metricsDecreasers$SAMPLEID
            metricsDecreasers2[,2]<-100*(metricsDecreasers$nt_habit_climbcling_RFadj - std_Parameters["fifths",names(metricsDecreasers)[2]])/(std_Parameters["ninetyfifths",names(metricsDecreasers)[2]] - std_Parameters["fifths",names(metricsDecreasers)[2]])
            metricsDecreasers2[,3]<-100*(metricsDecreasers$nt_volt_semi_RFadj - std_Parameters["fifths",names(metricsDecreasers)[3]])/(std_Parameters["ninetyfifths",names(metricsDecreasers)[3]] - std_Parameters["fifths",names(metricsDecreasers)[3]])
            metricsDecreasers2[,4]<-100*(metricsDecreasers$nt_EPT_RFadj - std_Parameters["fifths",names(metricsDecreasers)[4]])/(std_Parameters["ninetyfifths",names(metricsDecreasers)[4]] - std_Parameters["fifths",names(metricsDecreasers)[4]])
            metricsDecreasers2[,5]<-100*(metricsDecreasers$pt_BCG_att1i234b_RFadj - std_Parameters["fifths",names(metricsDecreasers)[5]])/(std_Parameters["ninetyfifths",names(metricsDecreasers)[5]] - std_Parameters["fifths",names(metricsDecreasers)[5]])

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
            paste("KS_Bug_IBI_Results_"
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
