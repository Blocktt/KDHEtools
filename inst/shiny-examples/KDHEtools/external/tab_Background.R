function(){
  tabPanel("Background",
           tabsetPanel(

             # tabPanel, 1
             tabPanel("Background",
                      # SideBar
                      sidebarLayout(
                        sidebarPanel(
                          img(src = "KDHE_logo.png", width = "100%")
                          , width = 3
                        )# sidebarPanel~END
                        , mainPanel(
                          includeHTML("www/App_Background_1Background.html")
                          , width = 7
                        )# mainPanel~END
                      )# sidebarLayout~END
             )## tabPanel ~ END

             # tabPanel, 2
             , tabPanel("Input Metrics",
                        # SideBar
                        sidebarLayout(
                          sidebarPanel(
                            img(src = "KDHE_logo.png", width = "100%")
                            , width = 3
                          )# sidebarPanel~END
                          , mainPanel(
                            includeHTML("www/App_Background_2InputMetrics.html")
                            , width = 7
                          )# mainPanel~END
                        )# sidebarLayout~END
             )## tabPanel ~ END

             # tabPanel, 3
             , tabPanel("Predictor variables"
                        , sidebarLayout(
                          sidebarPanel(
                            img(src = "KDHE_logo.png", width = "100%")
                            , width = 3
                          )# sidebarPanel~END
                          , mainPanel(
                            includeHTML("www/App_Background_3PredictorVariables.html")
                            , width = 7
                          )# mainPanel~END
                        )# sidebarLayout~END
             )## tabPanel ~ END
           ) ## tabsetPanel~END
  )## tabPanel~END
}##FUNCTION~END
