function(){
  tabPanel("Prepare Data",
           tabsetPanel(
             # tabPanel, 1, Prepare Data ----
             tabPanel("Prepare Data",
                      sidebarLayout(
                        sidebarPanel(
                          img(src = "KDHE_logo.png", height = 200)
                          , width = 3
                        )# sidebarPanel~END
                        , mainPanel(
                          includeHTML("www/App_PrepData_1PrepData.html")
                          , width = 7
                        )# mainPanel~END
                      )# sidebarLayout~END

             )## tabPanel ~ END
             # tabPanel, 2, Outside App Preparation ----
             , tabPanel("Outside App Preparation",
                        sidebarLayout(
                          sidebarPanel(
                            img(src = "KDHE_logo.png", height = 200)
                            , width = 3
                          )# sidebarPanel~END
                          , mainPanel(
                            includeHTML("www/App_PrepData_2OutsideApp.html")
                            , width = 7
                          )# mainPanel~END
                        )# sidebarLayout~END

             )## tabPanel ~ END
             # tabPanel, 3, Merge Files ----
             , tabPanel("Within App Preparation"
                        , tabsetPanel(
                          # tabPanel, 1, Taxa Trans ----
                          tabPanel("1. Taxa Translate and Attribute Assignment",
                                   # SideBar
                                   sidebarLayout(
                                     sidebarPanel(h2("Taxa Translate and Attribute Assignment")
                                                  , useShinyjs()

                                                  , p("The process below will combine user data with an official taxa list.")
                                                  , h4("A. Upload a File")
                                                  , fileInput("fn_input_taxatrans"
                                                              , label = "Import Data File for Adding Taxa Translation and Attributes"
                                                              , multiple = FALSE
                                                              , accept = c("text/csv"
                                                                           , "text/comma-separated-values"
                                                                           , "text/tab-separated-values"
                                                                           , "text/plain"
                                                                           , ".csv")
                                                  )

                                                  , h4("B. Operational Taxonomic Unit")
                                                  , uiOutput("UI_taxatrans_otu_pick")

                                                  , h4("C. User File Column Names")

                                                  , h6("Required Fields")
                                                  , p("If the default values are present they will be auto-populated.")
                                                  , uiOutput("UI_taxatrans_user_col_sampid")
                                                  , uiOutput("UI_taxatrans_user_col_taxaid")
                                                  , uiOutput("UI_taxatrans_user_col_n_taxa")

                                                  , h6("Optional Fields")
                                                  , p("All columns other than those specified above (required) or below (optional) will be dropped.
                IMPORTANT! Do not repeat the required columns, and do not include Life Stage or other fields that might cause a taxon to occur in more than one row for a given sample (which could lead to double-counting of that taxon in the richness metrics) .")
                , uiOutput("UI_taxatrans_user_col_groupby")

                , h4("D. Run Operation")
                , p("This button will merge the user file with the official taxa file")
                , shinyjs::disabled(shinyBS::bsButton("b_calc_taxatrans"
                                                      , label = "Run Operation"))

                , h4("E. Download Output")
                , p("All input and output files will be available in a single zip file.")
                , shinyjs::disabled(downloadButton("b_download_taxatrans"
                                                   , "Download Results"))

                                     )##sidebarPanel~END
                , mainPanel(
                  tabsetPanel(type = "tabs"
                              , id = "FB_TaxaTrans"
                              , tabPanel(title = "About (Taxa Translator)"
                                         , includeHTML("www/App_FB_TaxaTrans_1About.html")
                              )## tabPanel ~ END
                              , tabPanel(title = "Output Explanation (Taxa Translator)"
                                         , includeHTML("www/App_FB_TaxaTrans_2Output.html")
                              )## tabPanel ~ END
                              , tabPanel(title = "Data Import (Taxa Translator-Attributes)"
                                         , p("A table is shown below after data is loaded.")
                                         , DT::dataTableOutput("df_import_taxatrans_DT")
                                         , value = "tab_FB_TaxaTrans_DT"
                              )## tabPanel ~ END
                  )## tabsetPanel ~ END
                )##mainPanel~END

                                   )##sidebarLayout~END
                          )## tabPanel ~ END
                # tabPanel, 2, Predictors ----
                , tabPanel("2. Generate Predictor Parameters",
                           # SideBar
                           sidebarLayout(
                             sidebarPanel(h2("Assign Predictor Variables")
                                          #, useShinyjs()
                                          , h4("A. Upload a File")
                                          , fileInput("fn_input_predictors"
                                                      , label = "Import Data File for Adding Predictors"
                                                      , multiple = FALSE
                                                      , accept = c("text/csv"
                                                                   , "text/comma-separated-values"
                                                                   , "text/tab-separated-values"
                                                                   , "text/plain"
                                                                   , ".csv")
                                          )

                                          , h4("C. Define COMID Fields")
                                          , h6("Required Fields")

                                          , p("The input file needs to include latitude, longitude, and project (EPSG) to derive the COMID.")
                                          , p("The COMID will be used to match to the 'predictors' file.")
                                          , p("Any existing field named 'COMID' will be renamed to 'COMID_user'.")
                                          , uiOutput("UI_predictors_user_col_lat")
                                          , uiOutput("UI_predictors_user_col_long")
                                          , uiOutput("UI_predictors_user_col_epsg")

                                          , h4("D. Run Operation")
                                          , p("This button will assign predictors based on COMID")
                                          , shinyjs::disabled(shinyBS::bsButton("b_calc_predictors"
                                                                                , label = "Run Operation"))

                                          , h4("E. Download Output")
                                          , p("All input and output files will be available in a single zip file.")
                                          , shinyjs::disabled(downloadButton("b_download_predictors"
                                                                             , "Download Results"))

                             )## sidebarPanel ~ END
                             , mainPanel(
                               tabsetPanel(type = "tabs"
                                           , id = "FB_Predictors"
                                           , tabPanel(title = "About (Predictors)"
                                                      , includeHTML("www/App_FB_Predictors_1About.html")
                                           )## tabPanel ~ END
                                           , tabPanel(title = "Output Explanation (Predictors)"
                                                      , includeHTML("www/App_FB_Predictors_2Output.html")
                                           )## tabPanel ~ END
                                           , tabPanel(title = "Data Import (Predictors)"
                                                      , p("A table is shown below after data is loaded.")
                                                      , DT::dataTableOutput("df_import_predictors_DT")
                                                      , value = "tab_FB_Predictors_DT"
                                           )## tabPanel ~ Import ~ END
                               )## tabsetPanel ~ END
                             )## mainPanel ~ END
                           )##sidebarLayout~END
                )## tabPanel ~ END
                # tabPanel, 3, Merge Files ----
                , tabPanel("3. Merge Files"
                           , sidebarLayout(
                             sidebarPanel(
                               h2("Merge Sample and Site Files")
                               , p("This process will merge two CSV files.")
                               , p("File 1 is the primary file. All rows in the primary file will be carried through into the ouput file.")
                               , p("File 2 is the secondary file. Only rows that match with the common identifier in the primary file will be carried through into the output file.")
                               , br()

                               , h4("A. Upload files.")
                               # file input
                               , fileInput("fn_input_mf1"
                                           , label = "Import Primary File"
                                           , multiple = FALSE
                                           , accept = c("text/csv"
                                                        , "text/comma-separated-values"
                                                        , "text/tab-separated-values"
                                                        , "text/plain"
                                                        , ".csv")
                               )
                               , fileInput("fn_input_mf2"
                                           , label = "Import Secondary File"
                                           , multiple = FALSE
                                           , accept = c("text/csv"
                                                        , "text/comma-separated-values"
                                                        , "text/tab-separated-values"
                                                        , "text/plain"
                                                        , ".csv")
                               )

                               , h4("B. Select common identifier column for the merge.")
                               , uiOutput("UI_mergefiles_f1_col_merge")
                               , uiOutput("UI_mergefiles_f2_col_merge")

                               , h4("C. Run Operation")
                               , p("This button will merge the two files based on inputs")
                               , shinyjs::disabled(shinyBS::bsButton("b_calc_mergefiles"
                                                                     , label = "Run Operation"))

                               , h4("D. Download Output")
                               , p("All input and output files will be available in a single zip file.")
                               , shinyjs::disabled(downloadButton("b_download_mergefiles"
                                                                  , "Download Results"))

                             )## sidebarPanel ~ END
                             , mainPanel(
                               tabsetPanel(type = "tabs"
                                           , id = "FB_MergeFiles"
                                           , tabPanel(title = "About (Merge Files)"
                                                      , includeHTML("www/App_FB_MergeFiles_1About.html")
                                           )
                                           , tabPanel(title = "Output (Merge Files)"
                                                      , includeHTML("www/App_FB_MergeFiles_2Output.html"))
                                           , tabPanel(title = "Data Import (Merge Files, File 1)"
                                                      , p("A table is shown below after data is loaded.")
                                                      , DT::dataTableOutput("df_import_mf1_DT")
                                                      , value = "tab_FB_MF1_DT"
                                           )## tabPanel ~ Import ~ END
                                           , tabPanel(title = "Data Import (Merge Files, File 2)"
                                                      , p("A table is shown below after data is loaded.")
                                                      , DT::dataTableOutput("df_import_mf2_DT")
                                                      , value = "tab_FB_MF2_DT"
                                           )## tabPanel ~ Import ~ END
                               )## tabsetPanel ~ END
                             )## mainPanel ~ END
                           )##sidebarLayout~END
                )## tabPanel ~ END
                        )## tabsetPanel~END
             )## tabPanel ~ END
           ) ## tabsetPanel~END
  )## tabPanel~END
}##FUNCTION~END
