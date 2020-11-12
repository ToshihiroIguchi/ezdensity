
library(shiny)

shinyUI(fluidPage(

  titlePanel(title = "", windowTitle = "ezdensity"),

  sidebarLayout(
    sidebarPanel(
      
      #ファイル選択
      fileInput("file", "Data file(.csv, .xls, .xlsx, .xlsm)",
                accept = c("csv", "xls", "xlsx", "xlsm")
      ),
      
      #変数の選択
      htmlOutput("colname"),
      
      #スケール変換
      htmlOutput("scale")
      
      
      
    ),

    mainPanel(
      
      tabsetPanel(type = "tabs", 
                  tabPanel("Histgram", plotOutput("hist")),
                  tabPanel("Density", plotOutput("dens")),
                  tabPanel("CDF", plotOutput("cdf"))
                  
                  )
      
      
      
      
      
      
    )
  )
))
