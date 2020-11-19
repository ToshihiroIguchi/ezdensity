library(shiny)
library(dplyr)

#スケールの選択
scale <- c("linear", "log")
names(scale) <- c("Linear", "Log")


shinyServer(function(input, output) {
  
  observeEvent(input$file, {
    
    
    #データ読み込み
    raw.data <- reactive({
      try(
        read.data(input$file$datapath) %>%
          select_if(is.numeric) #数値のみ選択
        , silent = FALSE)
    })
    
    #データ選択
    output$colname <- renderUI({
      if(class(raw.data())[1] != "try-error" && is.data.frame(raw.data())){
        
        selectInput("colname", label = "Use data", choices = c(NA, colnames(raw.data())))
      }else{
        NULL
      }
    })
    
    #ファイルの内容がおかしかったらエラーメッセージ
    if(class(raw.data())[1] == "try-error"){
      showModal(modalDialog(
        title = "Error",
        "Failed to read the numerical data from the file. Check the contents of the file.",
        easyClose = TRUE
      ))
    }
    
    #項目名が選択された場合
    observeEvent(is.null.na.null(input$colname), {
      
      #ベクトルデータ
      vec.data <- reactive({
        raw.data()[, input$colname] %>% na.omit() %>% as.vec()
      })
      
      #リニアスケールかログスケールか選択
      output$scale <- renderUI({
        selectInput("scale", label = "Scale", 
                    choices = if(min(vec.data())<= 0){scale[1]}else{scale})
      })
      
      #ヒストグラムを表示
      output$hist <- renderPlot({
        gg.hist(vec.data(), scale = input$scale) 
      })
      
      #カーネル密度推定（メイン）
      result <- reactive({
        kde2(vec.data(), log = if(input$scale == "log"){TRUE}else{FALSE})
      })
      
      #密度推定結果表示
      output$pdf <- renderPlot({plot(result(), method = "pdf")})
      
      #CDF表示
      output$cdf <- renderPlot({plot(result(), method = "cdf")})
      
      #確率紙表示
      output$paper <- renderPlot({
        try.null(plot(result(), method = input$probit.logit, 
             alpha = as.numeric(input$plottingposition),
             log = if(input$scale == "log"){TRUE}else{FALSE}))
        })
      

    })
    
    
  })


})
