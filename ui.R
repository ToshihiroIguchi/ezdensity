#ライブラリ読み込み
library(shiny)

library(plotly)


#確率紙の縦軸の変換
probit.logit <- c("probit", "logit", "loglogit")
names(probit.logit) <- c("Probit", "Logit", "Log-logit")

#プロッティングポジション
plottingposition <- c(0, 0.3, 0.31, 0.38, 0.4, 0.44, 0.5, 1)
names(plottingposition) <- c("Mean rank", "Median rank", "Beard formula",
                             "Blom formula", "Cunnane formula", "Gringorten formula",
                             "Hazen formula", "Weibull formula")


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
                  tabPanel("Histgram", 
                           plotly::plotlyOutput("hist")),
                  tabPanel("pdf", plotlyOutput("pdf")),
                  tabPanel("cdf", plotlyOutput("cdf")),
                  tabPanel("Paper", 
                           
                           fluidRow(
                             column(6,
                                    selectInput("probit.logit", 
                                                label = "Scale transformation", 
                                                choices = probit.logit)
                                    ),
                             
                             column(6,
                                    selectInput("plottingposition",
                                                label = "Plotting position",
                                                choices = plottingposition,
                                                selected = "0.3")
                                    )
                           ),

                           plotOutput("paper"),
                           
                           
                           fluidRow(
                             column(6,
                                    numericInput("p.input",
                                                 label = "Probability",
                                                 value = NA,
                                                 min = 1e-30,
                                                 max = 1 - 1e-30),
                                    textOutput("q.out")
                                    
                                    ),
                             column(6,
                                    numericInput("q.input",
                                                 label = "Quantile",
                                                 value = NA),
                                    textOutput("p.out")
                                    
                                    )
                           )
                           
                           )
      )
                  
                  
      
      
      
      
      
      
    )
  )
))
