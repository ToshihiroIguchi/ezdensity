
library(shiny)

probit.logit <- c("probit", "logit")
names(probit.logit) <- c("Probit", "Logit")


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
                  tabPanel("pdf", plotOutput("pdf")),
                  tabPanel("cdf", plotOutput("cdf")),
                  tabPanel("Paper", 
                           selectInput("probit.logit", 
                                       label = "Scale transformation", 
                                       choices = probit.logit),
                           
                           plotOutput("paper"))
                  
                  )
      
      
      
      
      
      
    )
  )
))
