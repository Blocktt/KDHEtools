function(){
  tabPanel("Metric Adjustment Download",
           # SideBar
           sidebarLayout(
             sidebarPanel(
               h3("Metric Adjustment Factors")
               , h4("1. Load File")
               , h5("Select file parameters")
               , radioButtons('sep_sci', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ',')
               , radioButtons('quote_sci', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
               , fileInput('fn_input_MetAdjFact', 'Choose file to upload',
                           accept = c(
                             'text/csv'
                             ,'text/comma-separated-values'
                             ,'text/tab-separated-values'
                             ,'text/plain'
                             ,'.csv'
                             ,'.tsv'
                             ,'.tab')
               )##fileInput~END

               , h4("2. Obtain Metric Adjustment Factors")
               , h5("Metrics adjusted using 7 StreamCat variables")
               , actionButton("c_Calc", "Obtain factors")
               , tags$hr()
               , h4("3. Download Results")

               # Button
               , p("Select button to download zip file with input and results.")
               , useShinyjs()
               , shinyjs::disabled(downloadButton("c_downloadData", "Download"))

             )##sidebarPanel~END
             , mainPanel(
               includeHTML("www/App_MetricAdjustmentFactors.html")
               , DT::dataTableOutput('df_site_import_DT')
             )##mainPanel~END

           )##sidebarLayout~END

  )## tabPanel~END
}##FUNCTION~END
