function(){
  tabPanel("File Builder",
           tabsetPanel(
             tabPanel("1. Taxa Translate and Attribute Assignment",
                      # SideBar
                      sidebarLayout(
                        sidebarPanel(h2("Taxa Translate and Attribute Assignment")
                          , useShinyjs()

                          , p("The process below will combine user data with an official taxa list.")
                          , h4("A. Upload a File")
                          , p("If no file name showing below repeat 'Import File' in the left sidebar.")
                          # , p(textOutput("fn_input_display_taxatrans"))

                          , h4("B. Select Calculation.")
                          # , uiOutput("UI_taxatrans_pick_official")

                          , h4("C. User File Column Names")

                          , h6("Required Fields")
                          , p("If the default values are present they will be auto-populated.")
                          # SampleID (really for group_by)
                          # , uiOutput("UI_taxatrans_user_col_sampid")
                          # , uiOutput("UI_taxatrans_user_col_taxaid")
                          # N_Taxa (really for group_by)

                          # , uiOutput("UI_taxatrans_user_col_n_taxa")

                          , h6("Optional Fields")
                          , p("All columns other than those specified above (required) or below (optional) will be dropped.
                IMPORTANT! Do not repeat the required columns, and do not include Life Stage or other fields that might cause a taxon to occur in more than one row for a given sample (which could lead to double-counting of that taxon in the richness metrics) .")

                # , uiOutput("UI_taxatrans_user_col_groupby")

                , h4("D. Run Operation")
                , p("This button will merge the user file with the official taxa file")
                # , shinyjs::disabled(shinyBS::bsButton("b_calc_taxatrans"
                #                                       , label = "Run Operation"))

                , h4("E. Download Output")
                , p("All input and output files will be available in a single zip file.")
                # , shinyjs::disabled(downloadButton("b_download_taxatrans"
                #                                    , "Download Results"))

                        )##sidebarPanel~END
                        , mainPanel(
                          tabsetPanel(type = "tabs"
                                      , tabPanel(title = "About (Taxa Translator)"
                                                 ,includeHTML("www/ShinyHTML_FB_TaxaTrans_1About.html")
                                      )## tabPanel ~ END
                                      , tabPanel(title = "Output Explanation (Taxa Translator)"
                                                 ,includeHTML("www/ShinyHTML_FB_TaxaTrans_2Output.html")
                                      )## tabPanel ~ END
                          )## tabsetPanel ~ END
                        )##mainPanel~END

                      )##sidebarLayout~END
                      )## tabPanel ~ END
             , tabPanel("2. Generate Predictor Parameters",
                        # SideBar
                        sidebarLayout(
                          sidebarPanel(h2("Assign Index Class")
                                       #, useShinyjs()
                                       , h4("A. Upload a File")
                                       , p("If no file name showing below repeat 'Import File' in the left sidebar.")
                                       # , p(textOutput("fn_input_display_indexclass"))

                                       , h4("B. Define Index Name")
                                       # , uiOutput("UI_indexclass_indexname")

                                       , h4("C. Define Index Class Fields")
                                       , h6("Required Fields")
                                       , p("For the Maritime NW BCG model, the input file needs to include elevation and % flowline slope. During calibration of the BCG model, we derived slope from the NHDPlusV2 and elevation from the EPA StreamCat dataset (local catchment scale).")
                                       # for expediency hard code the classification fields
                                       # , uiOutput("UI_indexclass_user_col_elev")
                                       # , uiOutput("UI_indexclass_user_col_slope")
                                       # SampleID (only for group_by)
                                       # , uiOutput("UI_indexclass_user_col_sampid")

                                       , h4("D. Run Operation")
                                       , p("This button will assign Index_Class based on inputs")
                                       # , shinyjs::disabled(shinyBS::bsButton("b_calc_indexclass"
                                       #                                       , label = "Run Operation"))

                                       , h4("E. Download Output")
                                       , p("All input and output files will be available in a single zip file.")
                                       # , shinyjs::disabled(downloadButton("b_download_indexclass"
                                       #                                    , "Download Results"))

                          )## sidebarPanel ~ END
                        , mainPanel(
                          tabsetPanel(type = "tabs"
                                      , tabPanel(title = "About (Predictors)"
                                                 ,includeHTML("www/ShinyHTML_FB_IndexClass_1About.html")
                                      )## tabPanel ~ END
                                      , tabPanel(title = "Output Explanation (Predictors)"
                                                 ,includeHTML("www/ShinyHTML_FB_IndexClass_2Output.html")
                                      )## tabPanel ~ END
                          )## tabsetPanel ~ END
                        )## mainPanel ~ END
                        )##sidebarLayout~END
             )## tabPanel ~ END
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
                            # , fileInput("fn_input_mf1"
                            #             , label = "Import Primary File"
                            #             , multiple = FALSE
                            #             , accept = c("text/csv"
                            #                          , "text/comma-separated-values"
                            #                          , "text/tab-separated-values"
                            #                          , "text/plain"
                            #                          , ".csv")
                            # )
                            # , fileInput("fn_input_mf2"
                            #             , label = "Import Secondary File"
                            #             , multiple = FALSE
                            #             , accept = c("text/csv"
                            #                          , "text/comma-separated-values"
                            #                          , "text/tab-separated-values"
                            #                          , "text/plain"
                            #                          , ".csv")
                            # )

                            , h4("B. Select common identifier column for the merge.")
                            # , uiOutput("UI_mergefiles_f1_col_merge")
                            # , uiOutput("UI_mergefiles_f2_col_merge")

                            , h4("C. Run Operation")
                            , p("This button will merge the two files based on inputs")
                            # , shinyjs::disabled(shinyBS::bsButton("b_calc_mergefiles"
                            #                                       , label = "Run Operation"))

                            , h4("D. Download Output")
                            , p("All input and output files will be available in a single zip file.")
                            # , shinyjs::disabled(downloadButton("b_download_mergefiles"
                            #                                    , "Download Results"))

                          )## sidebarPanel ~ END
                          , mainPanel(
                            tabsetPanel(type = "tabs"
                                        , id = "MF_mp_tsp"
                                        , tabPanel(title = "About (Merge Files)"
                                                   ,includeHTML("www/ShinyHTML_FB_MergeFiles_1About.html")
                                                   )
                                        , tabPanel(title = "Output (Merge Files)"
                                                   ,includeHTML("www/ShinyHTML_FB_MergeFiles_2Output.html"))
                            )## tabsetPanel ~ END
                          )## mainPanel ~ END
                        )##sidebarLayout~END
                        )## tabPanel ~ END
           ) ## tabsetPanel~END
  )## tabPanel~END
}##FUNCTION~END



# function(){
#   tabPanel("File Builder",
#            # SideBar
#            sidebarLayout(
#              sidebarPanel(
#
#              )##sidebarPanel~END
#              , mainPanel(
#
#              )##mainPanel~END
#
#            )##sidebarLayout~END
#
#   )## tabPanel~END
# }##FUNCTION~END
