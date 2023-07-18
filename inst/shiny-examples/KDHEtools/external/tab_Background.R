function(){
  tabPanel("Background",
           sidebarLayout(
             sidebarPanel(
               img(src = "WDEQ_logo.png", height = 200)
               , width = 3
             )# sidebarPanel~END
             , mainPanel(
               includeHTML("www/App_Background.html")
               , width = 7
             )# mainPanel~END
           )# sidebarLayout~END
  ) # tabPanel~END
}# FUNCTION~END
