function(){
  tabPanel("Troubleshooting",
           sidebarLayout(
             sidebarPanel(
               img(src = "KDHE_logo.png", width = "100%")
               , width = 3
             )# sidebarPanel~END
             , mainPanel(
               includeHTML("www/App_Troubleshoot.html")
               , width = 7
             )# mainPanel~END
           )# sidebarLayout~END
  ) # tabPanel~END
}# FUNCTION~END
