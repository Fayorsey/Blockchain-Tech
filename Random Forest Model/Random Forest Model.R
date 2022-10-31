#Packages
packages <- c('dplyr' ,'magrittr' ,'tidyr' ,'tibble','readr','foreach',
              'doParallel','purrr','tidyverse','stringr','gtable','plyr',
              'httr','binancer','randomForest','keras','TTR','pROC',
              'GA','ggplot2','grid','gridExtra','gtable','corrplot','xtable',
              'dygraphs','caret','lubridate','zoo','timeSeries','timetk','TTR',
              'xts','tidyquant','TSstudio','QuantTools','spatialEco','knitr',
              'Hmisc','htmltools','blastula')

suppressMessages(invisible(lapply(packages, library, character.only = TRUE)))

options(scipen = 999)


limit <- .03
stop_limit <- .03
lag <- 48

#Predicting buys & stays
modeling <- function(candles, lag) {
  
  pairs <- names(candles)
  
  data <-  foreach(i = pairs, .packages = c("foreach", 'randomForest', 'dplyr', 'lubridate', 'xts'), .combine = rbind, .verbose = F, .export = c('limit', 'stop_limit'), .errorhandling = 'remove') %dopar% {
    temp_df <-  candles[[i]]  
    
    #Testing model
    # temp_df <- splitData$BTCUSDT
    
    
    mtry <- as.data.frame(tuneRF(temp_df$x_train, as.factor(temp_df$y_train), ntreeTry = 500))
    
    
    best_mtry <- last(mtry[, 'mtry']);best_mtry
    
    cat(paste0(best_mtry, " variables will be used to evaluate ", i))
    
    
    model <- randomForest(x = temp_df$x_train,
                          y = as.factor(temp_df$y_train),
                          ytest = as.factor(temp_df$y_test),
                          xtest = temp_df$x_test,
                          ntree = 500,
                          mtry = best_mtry,
                          cutoff = c(0.2, 0.8))
    # varImpPlot(model)
    
    # cat(paste0(i, ' 24h preidictions are : ', print(model$test$predicted), '\n'))
    
    prediction <- length(which(tail(model$test$predicted, 1) == 1));prediction
    signal <- ifelse(prediction > 0, 'Buy', 'Stay')
    last_price <- last(temp_df$original[, close])
    
    # upLimit <- abs(max(temp_df$original$percentChange));
    # downLimit <- abs(min(temp_df$original$percentChange));
    
    upLimit <- limit;
    downLimit <- stop_limit;
    
    profitLimit <- (1+upLimit)*last_price;
    stopTarget <- (1-downLimit)*last_price;
    
    #Index of buys and stays
    true_buys <- intersect(which(model$predicted == 1) , which(temp_df$y_train == 1))
    false_buys <- intersect(which(model$predicted == 1) , which(temp_df$y_train == 0))
    
    true_stays <- intersect(which(model$predicted == 0) , which(temp_df$y_train == 0))
    false_stays <- intersect(which(model$predicted == 0) , which(temp_df$y_train == 1))
    
    
    #Number of buys and stays
    n_buys <- length(which(model$predicted == 1))
    n_stays <- length(which(model$predicted == 0))
    
    n_true_buys <- length(true_buys)
    n_false_buys <- length(false_buys)
    n_true_stays <- length(true_stays)
    n_false_stays <- length(false_stays)
    
    
    
    #Summary Table
    percent_buys <- round(n_buys/ (n_buys + n_stays), 2)
    percent_stays <- round(n_stays / (n_buys + n_stays), 2)
    
    percent_true_buys <- round(n_true_buys / n_buys, 2)
    percent_false_buys <- round(n_false_buys / n_buys, 2)
    
    percent_true_stays <- round(n_true_stays / n_stays, 2)
    percent_false_stays <- round(n_false_stays / n_stays, 2)
    
    accuracy <- round((n_true_buys + n_true_stays) /  (n_buys + n_stays), 2)
    
    
    
    
    
    summary <- cbind.data.frame(i, percent_buys, percent_true_buys, percent_false_buys, percent_stays, percent_true_stays, 
                                percent_false_stays, accuracy, signal, last_price, profitLimit, stopTarget)
    
    colnames(summary) <- c('symbol', 'buys', 'trueBuys', 'falseBuys', 'stays',
                           'trueStays','falseStays', 'accuracy', 'signal', 'lastPrice', 'profitLimit', 'stopTarget')
    
    cat(paste0("Trading pair ", i, " evaluated."), '\n \n')
    return(summary)
  }
  # print(data)
  return(data)
  
}

summary <- modeling(splitData, lag)