
library(shiny)

probit.logit <- c("probit", "logit", "loglogit")
names(probit.logit) <- c("Probit", "Logit", "Log-logit")

plottingposition <- c(0, 0.3, 0.31, 0.38, 0.4, 0.44, 0.5, 1)
names(plottingposition) <- c("Mean rank", "Median rank", "Beard formula",
                             "Blom formula", "Cunnane formula", "Gringorten formula",
                             "Hazen formula", "Weibull formula")


#Weibull formula a = 1, Hazen formula a = 1/2, 
#Gringorten formula a = 0.44, Blom formula  a = 3.8, Cunnane formula a = 2/5
#Beard formula a = 0.31
#Mean rank a = 0, Median rank a = 0.3, 


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
                           
                           selectInput("plottingposition",
                                       label = "Plotting position",
                                       choices = plottingposition,
                                       selected = "0.3"),
                           
                           plotOutput("paper"))
                  
                  )
      
      
      
      
      
      
    )
  )
))
