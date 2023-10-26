function(){
  tabPanel("Calculate MMI",
           # SideBar
           sidebarLayout(
             sidebarPanel(
               # 0. Progress
               h3("App Steps")
               , h5(paste("If you create the input file using the"
                          ,"‘Prepare Data – Within App’ functions, you will need"
                          ,"to refresh the website before running the ‘Calculate MMI’"
                          ,"function, otherwise the app will crash. To refresh,"
                          ,"press the F5 button on your keyboard."))
               , h4("1. Load File")
               , fileInput("fn_input"
                           , label = "Choose file to upload"
                           , multiple = FALSE
                           , accept = c("text/csv"
                                        , "text/comma-separated-values"
                                        #, "text/tab-separated-values"
                                        , "text/plain"
                                        , ".csv")
               )##fileInput~END

               , h4("2. Mark redundant (non-distinct) taxa")
               , checkboxInput("ExclTaxa", "Generate Exclude Taxa Column", TRUE)
               # need to create code to use in b_Calc
               , h5("Click "
                    ,a("here"
                       , href = "https://raw.githubusercontent.com/Blocktt/ShinyAppDocuments/main/KDHEtools/Reports/RedundantTaxa_20231025.pdf"
                       , target="_blank")
                    , " for information on how the ‘redundant’ designations were made, and how to use your own ‘non-distinct’ taxa designations if desired.")
               , h4("3. Calculate IBI")
               , actionButton("b_Calc", "Calculate Metric Values and Scores")
               , tags$hr()

               , h4("4. Download Results")
               # Button
               , h5("Select button to download zip file with input and results.")
               , h5("Check 'results_Log.txt' for any warnings or messages.")
               , h5("Click "
                    ,a("here"
                       , href = "https://raw.githubusercontent.com/Blocktt/ShinyAppDocuments/main/KDHEtools/Example_Files/Example_Metric_PredAdjScored.xlsx"
                       , target="_blank")
                    , " for information on how the random forest metric adjustments were made.")
               , useShinyjs()
               , shinyjs::disabled(downloadButton("b_downloadData", "Download"))

             )##sidebarPanel~END
             , mainPanel(
               tabsetPanel(type="tabs"
                           , id = "tabs_MMIcalc_main"
                           , tabPanel("Data Import Viewer"
                                      , DT::dataTableOutput('df_import_DT')
                                      , value = "tab_MMIcalc_viewer")
                           , tabPanel("Outputs"
                                      , includeHTML("www/App_CalculateMMI_2Output.html"))
             )##tabsetPanel~END
             )##mainPanel~END

           )##sidebarLayout~END

  )## tabPanel~END
}##FUNCTION~END
