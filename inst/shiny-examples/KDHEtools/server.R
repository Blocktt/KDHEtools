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
  # modal dialog----
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

    # Misc Names----
    output$fn_input_display <- renderText({input$fn_input}) ## renderText~END


    # df_import----
    output$df_import_DT <- DT::renderDT({
        # input$df_import will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.


        inFile <- input$fn_input

        shiny::validate(
            need(inFile != "", "Please upload a data set") # used to inform the user that a data set is required
        )

        if (is.null(inFile)) {
            return(NULL)
        }##IF~is.null~END

        # Read user imported file
        df_input <- read.csv(inFile$datapath, header = TRUE,
                             sep = input$sep,
                             quote = input$quote, stringsAsFactors = FALSE)

        required_columns <- c("SampleID", "CollDate", "CollMeth"
                              , "StationID", "Lat", "Long", "TaxaID"
                              , "N_Taxa", "Exclude", "Nontarget","Airbreather"
                              , "BCG_Attr", "Habit", "Life_Cycle", "TolVal", "Phylum"
                              , "Order", "Family", "Genus", "Al2O3Ws", "CFS", "ClayWs"
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
        if (boo_Results == FALSE) {
            dir.create(file.path(".", "Results"))
        }

        # Remove all files in "Results" folder
        fn_results <- list.files(file.path(".", "Results"), full.names = TRUE)
        file.remove(fn_results)

        # Write to "Results" folder - Import as TSV
        fn_input <- file.path(".", "Results", "data_import.tsv")
        write.table(df_input, fn_input, row.names = FALSE
                    , col.names = TRUE, sep = "\t")

        # Copy to "Results" folder - Import "as is"
        file.copy(input$fn_input$datapath
                  , file.path(".", "Results", input$fn_input$name))

        return(df_input)

    }##expression~END
    , filter = "top", options = list(scrollX = TRUE)

    )##output$df_import_DT~END

    # b_Calc----
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
            df_data <- read.delim(fn_input, stringsAsFactors = FALSE, sep = "\t")

            # QC, FAIL if TRUE
            if (is.null(df_data)) {
                return(NULL)
            }

            # QC, N_TAXA = 0
            N_Taxa_zeros <- sum(df_data$N_TAXA == 0, na.rm = TRUE)
            if (N_Taxa_zeros > 0) {
                message("Some taxa in your dataset have a count (N_TAXA) of zero. Values for TAXAID with N_TAXA = 0 will be removed before calculations.")
            }

            # QC, Index Period
            QC_CollMonth <- df_data %>%
              mutate(CollMonth = lubridate::month(CollDate)) %>%
              pull(CollMonth)

            N_OutIndexPeriod <- sum(QC_CollMonth < 4 | QC_CollMonth > 10
                                    , na.rm = TRUE)

            if (N_OutIndexPeriod > 0) {
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
            if (Predictor_NA > 0) {
              message("Some sites in your dataset have missing predictor values. Index scores will be incorrect without COMPLETE predictor values.")
            }

            # QC, Exclude as TRUE/FALSE
            Exclude.T <- sum(df_data$EXCLUDE == TRUE, na.rm = TRUE)
            if (Exclude.T == 0) {##IF.Exclude.T.START
                message("EXCLUDE column does not have any TRUE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
            }##IF.Exclude.T.END

            # QC, NonTarget as TRUE/FALSE
            NonTarget.F <- sum(df_data$NONTARGET == FALSE, na.rm = TRUE)
            if (NonTarget.F == 0) {##IF.Exclude.T.START
                message("NONTARGET column does not have any FALSE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
            }##IF.Exclude.T.END


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Calculate, Metrics (takes ~ 30-45s)")
            Sys.sleep(0.5)

            # convert Field Names to UPPER CASE
            names(df_data) <- toupper(names(df_data))

            # QC, Required Fields
            col.req <- c("SAMPLEID", "COLLDATE", "COLLMETH"
                         , "STATIONID", "LAT", "LONG", "TAXAID"
                         , "N_TAXA", "EXCLUDE", "NONTARGET", "AIRBREATHER", "BCG_ATTR"
                         , "HABIT", "LIFE_CYCLE", "TOLVAL", "PHYLUM", "ORDER"
                         , "FAMILY", "GENUS", "AL2O3WS", "CFS", "CLAYWS", "ELEVCAT"
                         , "FE2O3CAT", "K2OWS", "L3ECO", "MGOCAT", "NWS", "PERMWS"
                         , "PRECIPCAT", "PRECIPWS", "SANDWS", "SWS", "TMEANCAT"
                         , "WETINDEXWS", "WSAREASQKM", "WTDEPWS")

            col.req.missing <- col.req[!(col.req %in% names(df_data))]

            # Add missing fields
            df_data[,col.req.missing] <- NA
            warning(paste("Metrics related to the following fields are invalid:"
                          , paste(paste0("   ", col.req.missing), collapse = "\n"), sep = "\n"))

            # calculate values and scores in two steps using BioMonTools
            # save each file separately

           # columns to keep
            keep_cols <- c("COLLDATE", "COLLMETH", "STATIONID", "LAT", "LONG"
                           , "AL2O3WS", "CFS", "CLAYWS", "ELEVCAT"
                           , "FE2O3CAT", "K2OWS", "L3ECO", "MGOCAT", "NWS"
                           , "PERMWS", "PRECIPCAT", "PRECIPWS", "SANDWS", "SWS"
                           , "TMEANCAT", "WETINDEXWS", "WSAREASQKM", "WTDEPWS")

            ## metric calculation----

            df_metval <- suppressWarnings(
              BioMonTools::metric.values(fun.DF = df_data
                            , fun.Community = "bugs"
                            , fun.MetricNames = BugMetrics
                            , fun.cols2keep = keep_cols
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

            # adjust metrics----

            std_Parameters <- read.csv("./data/StandardizationParameters0517.csv", row.names = 1)

            habit_model <- load("./data/nt_habit_climbcling_RFmod_final0517.Rdata")
            nt_habit_climbcling_pred <- predict(rf.mod,df_metval2[,c(predictors)])	##### use forest to predict nt_habit_climbcling
            nt_habit_climbcling_RFadj <- df_metval2[,"nt_habit_climbcling"] - nt_habit_climbcling_pred		##### calculate residual
            nt_habit_climbcling_RFadj <- unname(unlist(nt_habit_climbcling_RFadj))
            df_metval2$nt_habit_climbcling_RFadj <- nt_habit_climbcling_RFadj

            HBI_model <- load("./data/x_HBI_RFmod_final0517.Rdata")
            x_HBI_pred <- predict(rf.mod,df_metval2[,c(predictors)])			##### use forest to predict
            x_HBI_RFadj <- df_metval2[,"x_HBI"] - x_HBI_pred				##### calculate residual
            x_HBI_RFadj <- unname(unlist(x_HBI_RFadj))
            df_metval2$x_HBI_RFadj <- x_HBI_RFadj

            BCG_model <- load("./data/pt_BCG_att1i234b_RFmod_final0517.Rdata")
            pt_BCG_att1i234b_pred <- predict(rf.mod,df_metval2[, c(predictors)])					##### use forest to predict
            pt_BCG_att1i234b_RFadj <- df_metval2[,"pt_BCG_att1i234b"] - pt_BCG_att1i234b_pred		##### calculate residual
            pt_BCG_att1i234b_RFadj <- unname(unlist(pt_BCG_att1i234b_RFadj))
            df_metval2$pt_BCG_att1i234b_RFadj <- pt_BCG_att1i234b_RFadj

            semiv_model <- load("./data/nt_volt_semi_RFmod_final0517.Rdata")
            nt_volt_semi_pred <- predict(rf.mod,df_metval2[, c(predictors)])	##### use forest to predict pt_H_WDEQ_34
            nt_volt_semi_RFadj <- df_metval2[,"nt_volt_semi"] - nt_volt_semi_pred				##### calculate residual
            nt_volt_semi_RFadj <- unname(unlist(nt_volt_semi_RFadj))
            df_metval2$nt_volt_semi_RFadj <- nt_volt_semi_RFadj

            EPT_model <- load("./data/nt_EPT_RFmod_final0517.Rdata")
            nt_EPT_pred <- predict(rf.mod,df_metval2[,c(predictors)])	##### use forest to predict pt_H_WDEQ_34
            nt_EPT_RFadj <- df_metval2[,"nt_EPT"] - nt_EPT_pred				##### calculate residual
            nt_EPT_RFadj <- unname(unlist(nt_EPT_RFadj))
            df_metval2$nt_EPT_RFadj < -nt_EPT_RFadj

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

            # metric scoring----
            # Increasers
            metricsIncreasers <- df_metval2[,c("SAMPLEID", increasers)]

            metricsIncreasers2 <- data.frame(matrix(ncol = 2, nrow = dim(df_metval2)[1]))
            colnames(metricsIncreasers2) <- c("SAMPLEID", paste0(increasers, "_std"))

            metricsIncreasers2[,1] <- metricsIncreasers$SAMPLEID
            metricsIncreasers2[,2] <- 100 * (std_Parameters["ninetyfifths","x_HBI_RFadj"] - metricsIncreasers$x_HBI_RFadj)/(std_Parameters["ninetyfifths","x_HBI_RFadj"] - std_Parameters["fifths","x_HBI_RFadj"])

            # Decreasers
            metricsDecreasers <- df_metval2[, c("SAMPLEID", decreasers)]

            metricsDecreasers2 <- data.frame(matrix(ncol = 5, nrow = dim(df_metval2)[1]))
            colnames(metricsDecreasers2) <- c("SAMPLEID", paste0(decreasers, "_std"))

            metricsDecreasers2[,1] <- metricsDecreasers$SAMPLEID
            metricsDecreasers2[,2] <- 100 * (metricsDecreasers$nt_habit_climbcling_RFadj - std_Parameters["fifths",names(metricsDecreasers)[2]])/(std_Parameters["ninetyfifths",names(metricsDecreasers)[2]] - std_Parameters["fifths",names(metricsDecreasers)[2]])
            metricsDecreasers2[,3] <- 100 * (metricsDecreasers$nt_volt_semi_RFadj - std_Parameters["fifths",names(metricsDecreasers)[3]])/(std_Parameters["ninetyfifths",names(metricsDecreasers)[3]] - std_Parameters["fifths",names(metricsDecreasers)[3]])
            metricsDecreasers2[,4] <- 100 * (metricsDecreasers$nt_EPT_RFadj - std_Parameters["fifths",names(metricsDecreasers)[4]])/(std_Parameters["ninetyfifths",names(metricsDecreasers)[4]] - std_Parameters["fifths",names(metricsDecreasers)[4]])
            metricsDecreasers2[,5] <- 100 * (metricsDecreasers$pt_BCG_att1i234b_RFadj - std_Parameters["fifths",names(metricsDecreasers)[5]])/(std_Parameters["ninetyfifths",names(metricsDecreasers)[5]] - std_Parameters["fifths",names(metricsDecreasers)[5]])

            # combine and truncate at 0 and 100
            metrics_std <- suppressWarnings(left_join(metricsDecreasers2, metricsIncreasers2
                                     , by = "SAMPLEID") %>%
              mutate_if(is.numeric, funs(ifelse(. > 100, 100, .))) %>%
              mutate_if(is.numeric, funs(ifelse(. < 0, 0, .))))

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

    # File Builder, Taxa Translator ----

    # Input File, Display Name
    output$fn_input_display_taxatrans <- renderText({
      inFile <- input$fn_input

      if (is.null(inFile)) {
        return("..No file uploaded yet...")
      }##IF~is.null~END

      return(paste0("'", inFile$name, "'"))

    })## fn_input_display_taxatrans






    # File Builder, Predictors----

    # Input File, Display Name
    output$fn_input_display_predictors <- renderText({
      inFile <- input$fn_input_predictors

      if (is.null(inFile)) {
        return("..No file uploaded yet...")
      }##IF~is.null~END

      return(paste0("'", inFile$name, "'"))

    })## fn_input_display_predictors



    ## Predictors, FileWatch ----
    file_watch_predictors <- reactive({
      # trigger for df_import()
      input$fn_input_predictors
    })## file_watch

    ## Predictors, Import ----
    df_import_predictors <- eventReactive(file_watch_predictors(), {
      # use a multi-item reactive so keep on a single line (if needed later)

      # input$df_import_mf1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.

      inFile <- input$fn_input_predictors

      if (is.null(inFile)) {
        return(NULL)
      }##IF~is.null~END

      sep_user <- input$sep

      # Define file
      fn_inFile <- inFile$datapath

      #message(getwd())
      # message(paste0("Import, separator: '", input$sep,"'"))
      message(paste0("Import, file name: ", inFile$name))

browser()
      # Clean out "Results" folder
      source(file.path("external", "file_remove_results.R"))

      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))

      # Copy to "Results" folder - Import "as is"
      file.copy(inFile$datapath
                , file.path(path_results, inFile$name))

      # button, enable, calc
      shinyjs::enable("b_calc_predictors")

      # button, disable, download
      shinyjs::disable("b_download_predictors")

      # # activate tab Panel with table of imported data
      # updateTabsetPanel(session = getDefaultReactiveDomain()
      #                   , "MF_mp_tsp"
      #                   , selected = "tab_MF_1")

      # Return Value
      return(df_input)

    })##output$df_import_predictors~ END

    ## Predictors, Calc----
    observeEvent(input$b_calc_predictors, {
      shiny::withProgress({

        ### Calc, 00, Initialize ----
        prog_detail <- "Calculation, Predictors..."
        message(paste0("\n", prog_detail))

        # Number of increments
        prog_n <- 8
        prog_sleep <- 0.25

        ## Calc, 01, Import User Data ----
        prog_detail <- "Import Data, User"
        message(paste0("\n", prog_detail))
        # Increment the progress bar, and update the detail text.
        incProgress(1/prog_n, detail = prog_detail)
        Sys.sleep(prog_sleep)

        # button, disable, download
        shinyjs::disable("b_download_predictors")

        # Import data
        # data
        inFile <- input$fn_input_predictors
        fn_input_base <- tools::file_path_sans_ext(inFile$name)
        message(paste0("Import, file name, base: ", fn_input_base))
        df_sites <- read.delim(inFile$datapath
                               , header = TRUE
                               , sep = input$sep
                               , stringsAsFactors = FALSE)
        # QC, FAIL if TRUE
        if (is.null(df_sites)) {
          return(NULL)
        }


        ## Calc, 02, Gather and Test Inputs  ----
        prog_detail <- "QC Inputs"
        message(paste0("\n", prog_detail))
        # Increment the progress bar, and update the detail text.
        incProgress(1/prog_n, detail = prog_detail)
        Sys.sleep(prog_sleep)

        # Fun Param, Define
        #sel_col_sampid <- input$predictors_user_col_sampid
        sel_col_lat    <- input$predictors_user_col_lat
        sel_col_long   <- input$predictors_user_col_long
        sel_col_epsg   <- input$predictors_user_col_epsg

        # if (sel_col_sampid == "") {
        #   # end process with pop up
        #   msg <- "'SampleID' column name is missing!"
        #   shinyalert::shinyalert(title = "Predictors"
        #                          , text = msg
        #                          , type = "error"
        #                          , closeOnEsc = TRUE
        #                          , closeOnClickOutside = TRUE)
        #   validate(msg)
        # }## IF ~ sel_col_sampid

        if (sel_col_lat == "") {
          # end process with pop up
          msg <- "'Latitude' column name is missing!"
          shinyalert::shinyalert(title = "Predictors"
                                 , text = msg
                                 , type = "error"
                                 , closeOnEsc = TRUE
                                 , closeOnClickOutside = TRUE)
          validate(msg)
        }## IF ~ sel_col_sampid


        if (sel_col_long == "") {
          # end process with pop up
          msg <- "'Longitude' column name is missing!"
          shinyalert::shinyalert(title = "Predictors"
                                 , text = msg
                                 , type = "error"
                                 , closeOnEsc = TRUE
                                 , closeOnClickOutside = TRUE)
          validate(msg)
        }## IF ~ sel_col_sampid


        # need a value to evaluate in the next IF..
        if (sel_col_epsg == "") {
          epsg_user <- NA
        } else {
          epsg_user <- unique(df_sites[, sel_col_epsg])
        }## IF ~ sel_col_epsg

        # Define default EPSG if not provided
        if (sel_col_epsg == "") {
          # No Field selected
          # use default EPSG
          value_epsg <- epsg_default
        } else if (all(is.na(epsg_user))) {
          # All user values are NA
          # use default EPSG
          value_epsg <- epsg_default
        } else {
          # user provided value
          epsg_user <- unique(df_sites[, sel_col_epsg])
          value_epsg <- as.numeric(epsg_user[!is.na(epsg_user)])
        }## IF ~ sel_col_epsg

        msg <- paste0("EPSG = ", value_epsg)
        message(msg)

        # add EPSG to data (in case changed)
        df_sites[, "EPSG_CALC"] <- value_epsg


        ## Calc, 03, Generate COMID ----

        # existing COMID to COMID_user
        if ("COMID" %in% toupper(names(df_sites))) {
          # Rename to COMID_user
          names(df_sites)[toupper(names(df_sites)) %in% "COMID"] <- "COMID_user"
        }## IF ~ COMID

        # Add COMID based on Lat-Long
        # COMID
        comid <- StreamCatTools::sc_get_comid(df_sites
                                              , xcoord = sel_col_long
                                              , ycoord = sel_col_lat
                                              , crsys = value_epsg)

        # Add COMID to data
        df_sites[, "COMID"] <- strsplit(comid, ",")

        # END if COMID all NA
        comid_unique <- unique(df_sites[, "COMID"])
        if (length(comid_unique) == 1 & any(comid_unique == "NA")) {
          # end process with pop up
          m1 <- "'COMID' all NA!"
          m2 <- "Lat-Long and/or EPSG not valid."
          m3 <- "Or try again with existing Lat-Long with default EPSG (NAD83 NA)"
          msg <- paste(m1, m2, m3, sep = "\n")
          shinyalert::shinyalert(title = "Generate COMID"
                                 , text = msg
                                 , type = "error"
                                 , closeOnEsc = TRUE
                                 , closeOnClickOutside = TRUE)
          validate(msg)
        }## IF ~ sel_col_lat

        ## Calc, 04, Import Predictors----
        prog_detail <- "Import Predictors"
        message(paste0("\n", prog_detail))
        # Increment the progress bar, and update the detail text.
        incProgress(1/prog_n, detail = prog_detail)
        Sys.sleep(prog_sleep)

        # **BEN**
        # Should save the predictor file as an RDA file so is smaller and loads faster
        # Also, rename to a generic name
        # Create script in data-raw to transform file so can have the code when the file is updated
        # XLSX is 22.4 MB, CSV is 18.2 MB so not much savings, RDA will be < 2 MB

        fn_pred <- "KS_StreamCat_PredictorTable_BenB_20230721.xlsx"
        df_pred <- readxl::read_excel(file.path("data", fn_pred)
                                      , sheet = "Predictors_StreamCat"
                                      , guess_max = 10^5)

        ## Calc, 05, Merge ----
        prog_detail <- "Merge Predictors"
        message(paste0("\n", prog_detail))
        # Increment the progress bar, and update the detail text.
        incProgress(1/prog_n, detail = prog_detail)
        Sys.sleep(prog_sleep)

        # predictors based on COMID
        df_pred_merge <- merge(df_sites
                               , df_pred
                               , by.x = "COMID"
                               , by.y = "COMID"
                               , all.x = TRUE)


        ## Calc, 06, Save Results ----
        prog_detail <- "Save Results"
        message(paste0("\n", prog_detail))
        # Increment the progress bar, and update the detail text.
        incProgress(1/prog_n, detail = prog_detail)
        Sys.sleep(prog_sleep)

        # Save files

        # ## save, criteria
        # df_save <- df_pred
        # fn_part <- paste0("_predictors_", "0criteria", ".csv")
        # write.csv(df_save
        #           , file.path(path_results, paste0(fn_input_base, fn_part))
        #           , row.names = FALSE)
        # rm(df_save, fn_part)

        #**BEN**
        #Add criteria file as a hyperlink in the text on screen.
        # don't need to save 22 MB every time run operation

        ## save, results
        df_save <- df_pred_merge
        fn_part <- paste0("_predictors_", "1results", ".csv")
        write.csv(df_save
                  , file.path(path_results, paste0(fn_input_base, fn_part))
                  , row.names = FALSE)
        rm(df_save, fn_part)


        ## Calc, 07, Create Zip ----
        prog_detail <- "Create Zip File For Download"
        message(paste0("\n", prog_detail))
        # Increment the progress bar, and update the detail text.
        incProgress(1/prog_n, detail = prog_detail)
        Sys.sleep(prog_sleep)

        # Create zip file for download
        fn_4zip <- list.files(path = path_results
                              , full.names = TRUE)
        zip::zip(file.path(path_results, "results.zip"), fn_4zip)


        ## Calc, 08, Clean Up ----
        prog_detail <- "Calculate, Clean Up"
        message(paste0("\n", prog_detail))
        # Increment the progress bar, and update the detail text.
        incProgress(1/prog_n, detail = prog_detail)
        Sys.sleep(prog_sleep)

        # button, enable, download
        shinyjs::enable("b_download_predictors")

      }## expr ~ withProgress ~ END
      , message = "Calculating Predictors"
      )## withProgress

    }##expr ~ ObserveEvent

    )##observeEvent ~ b_calc_predictors

    ## b_download_predictors ----
    output$b_download_predictors <- downloadHandler(

      filename = function() {
        inFile <- input$fn_input
        fn_input_base <- tools::file_path_sans_ext(inFile$name)
        paste0(fn_input_base
               , "_Predictors_"
               , format(Sys.time(), "%Y%m%d_%H%M%S")
               , ".zip")
      } ,
      content = function(fname) {##content~START

        file.copy(file.path(path_results, "results.zip"), fname)

      }##content~END
      #, contentType = "application/zip"
    )##download ~ predictors


    ## Predictors, UI----

    # output$UI_predictors_user_col_sampid <- renderUI({
    #   str_col <- "Column, SampleID (unique station or sample identifier)"
    #   selectInput("predictors_user_col_sampid"
    #               , label = str_col
    #               , choices = c("", names(df_import_predictors()))
    #               , selected = "SampleID"
    #               , multiple = FALSE)
    # })## UI_colnames

    output$UI_predictors_user_col_lat <- renderUI({
      str_col <- "Column, Latitude (decimal degrees)"
      selectInput("predictors_user_col_lat"
                  , label = str_col
                  , choices = c("", names(df_import_predictors()))
                  , selected = "Latitude"
                  , multiple = FALSE)
    })## UI_colnames

    output$UI_predictors_user_col_long <- renderUI({
      str_col <- "Column, Longitude (decimal degrees)"
      selectInput("predictors_user_col_long"
                  , label = str_col
                  , choices = c("", names(df_import_predictors()))
                  , selected = "Longitude"
                  , multiple = FALSE)
    })## UI_colnames

    output$UI_predictors_user_col_epsg <- renderUI({
      m1 <- "Column, EPSG (datum), e.g., NAD83 North America is 4269."
      m2 <- "Column can be left blank and default of 4269 will be used."
      str_col <- paste(m1, m2, sep = "\n")
      selectInput("predictors_user_col_epsg"
                  , label = str_col
                  , choices = c("", names(df_import_predictors()))
                  , selected = "EPSG"
                  , multiple = FALSE)
    })## UI_colnames





    # File Builder, Merge Files ----

    ### Merge, Import, FileWatch ----
    file_watch_mf1 <- reactive({
      # trigger for df_import()
      input$fn_input_mf1
    })## file_watch

    file_watch_mf2 <- reactive({
      # trigger for df_import()
      input$fn_input_mf2
    })## file_watch

    ## Merge, Import, df_import_mf1 ----
    df_import_mf1 <- eventReactive(file_watch_mf1(), {
      # use a multi-item reactive so keep on a single line (if needed later)

      # input$df_import_mf1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.

      inFile <- input$fn_input_mf1

      if (is.null(inFile)) {
        return(NULL)
      }##IF~is.null~END

      sep_user <- input$sep

      # Define file
      fn_inFile <- inFile$datapath

      #message(getwd())
      # message(paste0("Import, separator: '", input$sep,"'"))
      message(paste0("Import, file name: ", inFile$name))

      # Move Results folder clean up to calc button

      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))

      # Copy to "Results" folder - Import "as is"
      file.copy(inFile$datapath
                , file.path(path_results, inFile$name))

      # button, enable, calc
      shinyjs::enable("b_calc_mergefiles")

      # activate tab Panel with table of imported data
      updateTabsetPanel(session = getDefaultReactiveDomain()
                        , "MF_mp_tsp"
                        , selected = "tab_MF_1")

      # Return Value
      return(df_input)

    })##output$df_import_mf1 ~ END


    ## Merge, Import, df_import_mf2----
    df_import_mf2 <- eventReactive(file_watch_mf2(), {
      # use a multi-item reactive so keep on a single line (if needed later)

      # input$df_import_mf1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.

      inFile <- input$fn_input_mf2

      if (is.null(inFile)) {
        return(NULL)
      }##IF~is.null~END

      # Define file
      fn_inFile <- inFile$datapath

      sep_user <- input$sep

      #message(getwd())
      #message(paste0("Import, separator: '", input$sep,"'"))
      message(paste0("Import, file name: ", inFile$name))

      # Move Results folder clean up to calc button

      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))

      # Copy to "Results" folder - Import "as is"
      file.copy(inFile$datapath
                , file.path(path_results, inFile$name))

      # button, enable, calc
      shinyjs::enable("b_calc_mergefiles")

      # activate tab Panel with table of imported data
      updateTabsetPanel(session = getDefaultReactiveDomain()
                        , "MF_mp_tsp"
                        , selected = "tab_MF_2")

      # Return Value
      return(df_input)

    })##output$df_import_mf2 ~ END

    ## Merge, Import, df_import_mf1_DT ----
    output$df_import_mf1_DT <- DT::renderDT({
      df_data <- df_import_mf1()
    }##expression~END
    , filter = "top"
    , caption = "Table. MergeFile 1 (Samples)."
    , options = list(scrollX = TRUE
                     , pageLength = 5
                     , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                     , autoWidth = TRUE)
    )##df_import_mf1_DT ~ END

    ## Merge, Import, df_import_mf2_DT ----
    output$df_import_mf2_DT <- DT::renderDT({
      df_data <- df_import_mf2()
    }##expression~END
    , filter = "top"
    , caption = "Table. MergeFile 2 (Sites)."
    , options = list(scrollX = TRUE
                     , pageLength = 5
                     , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                     , autoWidth = TRUE)
    )##df_import_mf1_DT ~ END

    ## Merge, UI----

    output$UI_mergefiles_f1_col_merge <- renderUI({
      str_col <- "Merge Identifier, Primary File, Column Name"
      selectInput("mergefiles_f1_col_merge"
                  , label = str_col
                  # , choices = c("SiteID", "feature", "in progress")
                  , choices = c("", names(df_import_mf1()))
                  , selected = "SiteID"
                  , multiple = FALSE)
    })## UI_colnames

    output$UI_mergefiles_f2_col_merge <- renderUI({
      str_col <- "Merge Identifier, Secondary File, Column Name"
      selectInput("mergefiles_f2_col_merge"
                  , label = str_col
                  #, choices = c("SiteID", "feature", "in progress")
                  , choices = c("", names(df_import_mf2()))
                  , selected = "SiteID"
                  , multiple = FALSE)
    })## UI_colnames


    ## b_Calc_MergeFiles ----
    observeEvent(input$b_calc_mergefiles, {
      shiny::withProgress({

        #### Calc, 00, Set Up Shiny Code ----

        prog_detail <- "Calculation, Merge Files..."
        message(paste0("\n", prog_detail))

        # Number of increments
        prog_n <- 6
        prog_sleep <- 0.25

        ### Calc, 01, Initialize ----
        prog_detail <- "Initialize Data"
        message(paste0("\n", prog_detail))
        # Increment the progress bar, and update the detail text.
        incProgress(1/prog_n, detail = prog_detail)
        Sys.sleep(prog_sleep)

        # button, disable, download
        shinyjs::disable("b_download_mergefiles")

        ### Calc, 02, Gather and Test Inputs  ----
        prog_detail <- "QC Inputs"
        message(paste0("\n", prog_detail))
        # Increment the progress bar, and update the detail text.
        incProgress(1/prog_n, detail = prog_detail)
        Sys.sleep(prog_sleep)

        # inputs
        ## file names
        fn_mf1 <- input$fn_input_mf1$name
        fn_mf2 <- input$fn_input_mf2$name
        ## column names
        col_siteid_mf1 <- input$mergefiles_f1_col_merge
        col_siteid_mf2 <- input$mergefiles_f2_col_merge
        ## file name base (file 1)
        fn_input_base <- tools::file_path_sans_ext(fn_mf1)

        # Stop if don't have both MF1 and MF2
        if (is.null(fn_mf1)) {
          msg <- "Merge File 1 filename is missing!"
          shinyalert::shinyalert(title = "Merge File Calculation Error"
                                 , text = msg
                                 , type = "error"
                                 , closeOnEsc = TRUE
                                 , closeOnClickOutside = TRUE)
          validate(msg)
        }## IF ~ is.null (mf1)

        if (is.null(fn_mf2)) {
          msg <- "Merge File 2 filename is missing!"
          shinyalert::shinyalert(title = "Merge File Calculation Error"
                                 , text = msg
                                 , type = "error"
                                 , closeOnEsc = TRUE
                                 , closeOnClickOutside = TRUE)
          validate(msg)
        }## IF ~ is.null (mf1)

        # Stop if colname for merge is NA
        if (col_siteid_mf1 == "") {
          msg <- "Merge File 1 merge column is missing!"
          shinyalert::shinyalert(title = "Merge File Calculation Error"
                                 , text = msg
                                 , type = "error"
                                 , closeOnEsc = TRUE
                                 , closeOnClickOutside = TRUE)
          validate(msg)
        }## IF ~ is.null (mf1)

        if (col_siteid_mf2 == "") {
          msg <- "Merge File 2 merge column is missing!"
          shinyalert::shinyalert(title = "Merge File Calculation Error"
                                 , text = msg
                                 , type = "error"
                                 , closeOnEsc = TRUE
                                 , closeOnClickOutside = TRUE)
          validate(msg)
        }## IF ~ is.null (mf1)



        # Remove non-MergeFiles files
        # Remove all files in "Results" folder
        # 2 file imports so moved Results folder clean up here from import section
        fn_results <- list.files(path_results
                                 , full.names = TRUE
                                 , include.dirs = FALSE
                                 , recursive = TRUE)
        message(paste0("Files in 'results' folder (before removal) = "
                       , length(fn_results)))

        # Exclude MF1 and MF2
        fn_mf_keep <- file.path(path_results
                                , c(fn_mf1, fn_mf2))
        fn_results <- fn_results[!fn_results %in% fn_mf_keep]
        # Remove non MF files
        file.remove(fn_results) # ok if no files
        # QC, repeat
        fn_results2 <- list.files(path_results
                                  , full.names = TRUE
                                  , include.dirs = FALSE
                                  , recursive = TRUE)
        message(paste0("Files in 'results' folder (after removal [should be 2]) = "
                       , length(fn_results2)))


        ### Calc, 03, Run Function----
        suff_1x <- ".x"
        suff_2y <- ".y"
        df_merge <- merge(df_import_mf1()
                          , df_import_mf2()
                          , by.x = col_siteid_mf1
                          , by.y = col_siteid_mf2
                          , suffixes = c(suff_1x, suff_2y)
                          , all.x = TRUE
                          , sort = FALSE
        )
        # ***REPEAT*** same merge statement in DT statement for display on tab

        # move MF2 columns to the start (at end after merge)
        ## use index numbers
        ncol_1x <- ncol(df_import_mf1())
        ncol_merge <- ncol(df_merge)
        df_merge <- df_merge[, c(1, seq(ncol_1x + 1, ncol_merge), 2:ncol_1x)]

        ### Calc, 04, Save Results ----
        fn_merge <- paste0(fn_input_base, "_MergeFiles_RESULTS.csv")
        pn_merge <- file.path(path_results, fn_merge)
        write.csv(df_merge, pn_merge, row.names = FALSE)


        ### Calc, 05, Clean Up----
        prog_detail <- "Calculate, Clean Up"
        message(paste0("\n", prog_detail))
        # Increment the progress bar, and update the detail text.
        incProgress(1/prog_n, detail = prog_detail)
        Sys.sleep(2 * prog_sleep)

        # # activate tab Panel with table of imported data
        # updateTabsetPanel(session = getDefaultReactiveDomain()
        #                   , "MF_mp_tsp"
        #                   , selected = "tab_MF_merge")


        ### Calc, 06, Zip Results ----
        fn_4zip <- list.files(path = path_results
                              , full.names = TRUE)
        zip::zip(file.path(path_results, "results.zip"), fn_4zip)

        # button, enable, download
        shinyjs::enable("b_download_mergefiles")

      }## expr ~ withProgress ~ END
      , message = "Merging Files"
      )## withProgress ~ END
    }##expr ~ ObserveEvent ~ END
    )##observeEvent ~ b_calc_met_therm ~ END


    ## b_download_mergefiles ----
    output$b_download_mergefiles <- downloadHandler(

      filename = function() {
        inFile <- input$fn_input_mf2
        fn_input_base <- tools::file_path_sans_ext(inFile$name)
        paste0(fn_input_base
               , "_MergeFiles_"
               , format(Sys.time(), "%Y%m%d_%H%M%S")
               , ".zip")
      } ,
      content = function(fname) {

        file.copy(file.path(path_results, "results.zip"), fname)

      }##content~END
      #, contentType = "application/zip"
    )##download ~ MergeFiles



})##shinyServer~END
