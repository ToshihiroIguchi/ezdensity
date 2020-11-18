library(ks)
library(dplyr)
library(readr)
library(readxl)

library(ggplot2)
#library(plotly)

#ベクトルに強制変換
as.vec <- function(x){
  as.matrix(x) %>% as.vector()
}

#文字列末尾確認
chk.end <- function(chk, chr){
  
  chr.pos <- grep(paste0("\\", chk, "$"), chr)
  if(length(chr.pos) == 0){return(FALSE)}else{return(TRUE)}
  
}

#NULLとNAであればNULLを返す
is.null.na.null <- function(x){
  
  if(is.null(x)){return(NULL)}
  if(is.na(x)){return(NULL)}
  if(x == "NA"){return(NULL)}
  return(TRUE)
  
}

#データ読み込み
read.data <- function(file){
  
  #csvの場合
  if(chk.end(".csv", file)){
    
    #csvが数値だけのデータの場合はシート全体をベクトル化
    #1行目からデータを読み込む
    ret.chk.raw <- suppressMessages(read_csv(file, col_names = FALSE))  
    
    #数値のみ選択
    ret.chk <- ret.chk.raw %>% select_if(is.numeric) 
    
    #文字の列が全くない場合
    if(ncol(ret.chk.raw) == ncol(ret.chk)){
      
      #ベクトル化
      ret <- tibble(Data = ret.chk %>% as.vec())
      
      #戻り値
      return(ret)
      
    }
    
    #文字の列が存在する阿合は、項目名から再度読み込んで各列を数値化
    ret <- suppressMessages(read_csv(file))   %>%
      select_if(is.numeric) #数値のみ選択
    
    #戻り値
    return(ret)
  }
  
  #エクセル形式の場合
  if(chk.end(".xlsx", file) || chk.end(".xls", file) || chk.end(".xlsm", file)){
    
    #空のデータフレームを準備
    ret <- tibble()
    
    #シート名を抜き出す
    excel.sheets <- excel_sheets(file)
    
    #各シートから抜き出す
    for(i in 1:length(excel.sheets)){
      
      #シートがすべて数値になっているかチェック
      df.i.chk.raw <- try(read_excel(file, sheet = i, col_names = FALSE), 
                          silent = FALSE) #項目名はなし
      df.i.chk <- try(df.i.chk.raw  %>% select_if(is.numeric), silent = FALSE)
      df.i.chk.error <- try(ncol(df.i.chk.raw) == ncol(df.i.chk), silent =FALSE)
      
      #数値のみ抜き出したデータが、元のデータ数と一致するかチェック
      if(df.i.chk.error == TRUE){
        
        #一致する場合
        df.i <- data.frame(Data = df.i.chk %>% as.vec())
        colnames(df.i) <- excel.sheets[i]
        
      }else{
        
        #シートから読み込みし数値列のみ抜き出す
        df.i <- try(read_excel(file, sheet = i) %>% select_if(is.numeric), silent = FALSE)
        
        
      }
      
      
      
      #エラーが起きない場合の処理
      if(class(df.i)[1] != "try-error"){
        
        #データが存在する場合
        if(nrow(df.i) > 0 && ncol(df.i) > 0){
          
          #戻り値に何かデータが入っているかチェック
          if(nrow(ret) > 0){
            
            #入っている場合
            
            #長い方の行数
            max.nrow <- max(nrow(df.i), nrow(ret))
            
            #行数を拡張したうえで結合
            ret <- tibble(df.long(ret, max.nrow), df.long(df.i, max.nrow))
            
          }else{
            
            #入ってない場合
            ret <- df.i
          }
          
        }
      }
    }
    
    #結合したデータを返す
    return(ret)
    
  }
  
  #該当がなかった場合、NAを返す
  return(NULL)
}

#ヒストグラムを描く
gg.hist <- function(vec, bw = NULL, scale = "linear"){
  
  #https://qiita.com/hoxo_b/items/13d034ab0ed60b4dca88
  
  #エラーチェック
  if(is.null(vec) || is.null(scale)){return(NULL)}
  
  #エラーチェック2
  if(!is.vector(vec)){return(NULL)}
  
  #欠損値を除く
  vec <- na.omit(vec)
  
  #バンド幅が指定されていないければ設定
  if(is.null(bw)){
    bw <- diff(range(vec))/20
  }
  
  #ggplot本体
  ret <- ggplot(data.frame(x = vec), aes(x = x)) +
    geom_histogram(binwidth = bw, fill = "white", color = "black") +
    xlab(NULL) + ylab("Count")
    
  
  if(scale == "log"){
    ret <- ret + scale_x_log10()
  }
  
  
  #戻り値
  return(ret)
}

#カーネル密度推定
kde2 <- function(x, log = FALSE){
  
  #エラーチェック
  if(is.null(x) || is.null(scale)){return(NULL)}
  if(!is.vector(x) || !is.numeric(x)){return(NULL)}
  
  #kde関数に入れる値を作成
  x.vec <- if(log){log(x)}else{x}
  
  #メインの部分
  ret <- kde(x = x.vec)
  
  #対数変換するかしないか代入
  ret$log <- log
  
  #クラスを変更
  class(ret) <- "kde2"
  
  #戻り値
  return(ret)
  
}

#プロッティングポジション公式
pp.formula <- function(x, alpha){
  
  #Weibull formula a = 1, Hazen formula a = 1/2, 
  #Gringorten formula a = 0.44, Blom formula  a = 3.8, Cunnane formula a = 2/5
  #Beard formula a = 0.31
  #Mean rank a = 0, Median rank a = 0.3, 
  
  
  #エラーチェック
  if(alpha < 0 || alpha > 1){return(NULL)}
  
  #xの値が小さい順から数えて何番目か調査
  i <- rank(x)
  
  #データ数
  N <- length(x)
  
  #プロッティングポジション
  ret <- (i - alpha)/(N + 1 - 2*alpha)
  
  #戻り値
  return(ret)
  
}

#yが0,1の場合は除去
y01.del <- function(df){
  #エラーチェック
  if(is.null(df)){return(NULL)}
  if(!is.data.frame(df)){return(NULL)}
  
  #yが0,1の位置
  y.vec <- df$y %>% as.vec()
  y01.pos <- which(y.vec*(1 - y.vec) > 0)
  
  #0,1を削除
  ret <- df[y01.pos, ]
  
  #戻り値
  return(ret)
}

#確率密度関数をプロット
plot.kde2 <- function(obj, log = FALSE, method = "pdf", alpha = 0.3){
  
  #methodはpdf, cdf, probit, logit, loglogitを選択可能
  
  #pdfでもcdfでもprobitでもlogitでもない場合はNULLを返す
  if(method != "pdf" && method != "cdf" && 
     method != "probit" && method != "logit" && method != "loglogit"){return(NULL)}
  
  #オブジェクトがNULLの場合はNULLを返す
  if(is.null(obj)){return(NULL)}
  
  #オブジェクトのクラスがkde2か確認
  if(class(obj) != "kde2"){return(NULL)}
  
  #クラスをkdeに変換
  class(obj) <- "kde"
  
  #オブジェクトに格納されているx軸の値（元のデータか、対数に変換されているか）
  x.raw <- obj$eval.points
  
  #範囲はNULLにしておく
  lim.add <- NULL
  
  
  #x軸のデータ作成(対数変換されている場合は元に戻す)
  if(obj$log){
    x.vec <- exp(x.raw)
    x.obj <- exp(obj$x)
  }else{
    x.vec <- x.raw
    x.obj <- obj$x
  }
  
  #pdfの場合のy軸のデータとラベル名作成
  if(method == "pdf"){
    y <- obj$estimate
    y.name <- "Density"
    point.df <- data.frame(x = x.obj, y = 0)
    gg.add <- geom_point(data = point.df, aes(x = x, y = y))
  }
  
  #cdfの場合のy軸のデータとラベル名作成
  if(method == "cdf"){
    y <- pkde(x.raw, fhat = obj)
    y.name <- "Cumulative Propotion"
    
    #ecdfを計算する関数を作成
    ecdffn <- ecdf(x.obj)
    
    #データフレームを作成
    df.ecdf <- data.frame(x = x.vec, y = ecdffn(x.vec))
    
    #ggplotに付け足すプロット
    gg.add <- geom_line(data = df.ecdf, aes(x = x, y = y)) 
    
  }
  
  
  #ワイブル、ガンベル、正規確率、対数正規確率プロットの場合のy軸のデータとラベル名作成
  if(method == "probit" || method == "logit" || method == "loglogit"){
    y <- pkde(x.raw, fhat = obj)
    y.name <- "Cumulative Propotion"
    
    #データフレームを作成
    df.pp <- data.frame(x = x.obj, y = pp.formula(x.obj, alpha = alpha))
    
    #ggplotに付け足すプロット
    gg.add <- geom_point(data = df.pp, aes(x = x, y = y)) 
    
    #最小値と最大値を規定
    #https://www.r-bloggers.com/2015/09/creating-a-scale-transformation/
    #ゼロと1を含むとエラーになるので、0より大きく1より小さい数にする
    y.min <- (1/length(x.raw))*0.5 #係数は調整する必要があるか？
    lim.add <- ylim(y.min, 1 - y.min)
    
    
    #y軸のスケール設定
    #http://mukkujohn.hatenablog.com/entry/2016/10/08/155023
    #http://goldenstate.cocolog-nifty.com/blog/2014/08/r-27c2.html
    break.scale.half <- 10^c(floor(log10(y.min)) : -1)
    break.scale <- c(break.scale.half, 0.5, 1 - sort(break.scale.half, decreasing = TRUE))
    
    #lim.add + scale_y_continuous(limits = c(y.min, 1- y.min))
    
    #yの値が0,1は除去
    data <- data %>% y01.del()
    
  }
  
  #グラフ用データ作成
  data <- data.frame(x.vec = x.vec, y = y)

  #プロビット変換やロジット変換で0,1を含むとグラフにできないので除去
  if(method == "probit" || method == "logit" || method == "loglogit"){
    data <- data %>% y01.del()
  }
  

  
  #ggplotのオブジェクトを作成
  ret <- ggplot(data = data, mapping = aes(x = x.vec, y = y)) + 
    geom_line(color = "red") +
    gg.add + 
    xlab(NULL) + ylab(y.name) +
    lim.add
  
  
  #対数設定の場合
  if(log){
    ret <- ret  + scale_x_log10()
  }
  
  
  
  #probitかlogitの場合は縦軸を変えて直線に近づける
  if(method == "probit" || method == "logit" || method == "loglogit"){
    
    #y軸をプロビット変換。正規分布なら直線になる。
    if(method == "probit"){
      ret <- ret + coord_trans(y = scales::probit_trans())
    }
    
    #y軸をロジット変換
    if(method == "logit"){
      ret <- ret + coord_trans(y = scales::logit_trans())
    }
    
    #y軸を対数のロジットで変換。ワイブルやガンベルが直線に
    if(method == "loglogit"){
      #https://stackoverflow.com/questions/49248937/in-rs-scales-package-why-does-trans-new-use-the-inverse-argument
      lnln <- trans_new("lnln", 
                        transform = function(x){log(log(1/(1 - x)))},
                        inverse = function(x){x})
      
      ret <- ret + coord_trans(y = lnln)
      
    }
    
    
  }
  
  
  
  
  #戻り値
  return(ret)
}

#エラーをNULLにする
try.null <- function(obj){
  
  ret <- try(obj, silent = FALSE)
  
  if(class(ret)[1] == "try-error"){return(NULL)}
  
  return(ret)
  
  
}

