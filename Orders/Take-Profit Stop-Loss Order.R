#Loading Data
packages <- c('dplyr', 'magrittr',  'tidyr', 'readr', 'foreach', 
              'httr', 'binancer', 'lubridate', 'Hmisc', 'xts', 'utils')

# Packages loading
suppressMessages(invisible(lapply(packages, library, character.only = TRUE)))

options(scipen = 999)

key <- ''
secret <- ''
binance_credentials(key, secret)


allCoins <- binance_symbols()
coinsUSDT <- allCoins[grepl('BUSD', allCoins)]
coinsETH <- allCoins[grepl('ETH', allCoins)]
coinsBNB <- allCoins[grepl('BNB', allCoins)]

portfolio <- binance_balances() %>% 
  dplyr::filter(free > 0 | locked > 0);portfolio

holdings <- portfolio
holdings <- arrange(holdings, asset)
currentPrice <- binance_coins_prices()
currentPrice <- arrange(currentPrice, symbol)

holdings$usd <- currentPrice[currentPrice$symbol %in% holdings$asset, 2];
holdings$value <- holdings$total * holdings$usd


holdings
sum(holdings$value)
freeAssets <- holdings %>% 
  dplyr::filter(free > 0 & value > 10 & asset != 'BUSD');

freeAssets
sum(freeAssets$value)


openOrders <- binance_open_orders()

if(nrow(openOrders) > 0) {
  
  openOrders <- openOrders %>% 
    dplyr::filter(side == "SELL")
  
  symbols <- openOrders$symbol;symbols
  
  #Exit open positions
  foreach(i = 1:length(symbols), .errorhandling = 'pass', .combine = rbind, .verbose = F) %do% {
    
    symbol <- symbols[i]
    
    
    #Aggregate all known orders for locked coins
    
    allOrders <- binance_all_orders(symbol);allOrders
    
    buyOrders <- allOrders %>% 
      dplyr::filter(side == "BUY") %>% 
      dplyr::arrange(time); buyOrders
    
    sellOrders <- allOrders %>%
      dplyr::filter(status == 'NEW' & side == "SELL"); sellOrders
    
    orderID <- sellOrders[1, order_id]; orderID
    clientID <- sellOrders[1, client_order_id];clientID
    
    
    #Filter all open orders by last filled buy
    
    lastBuy <- last(buyOrders);lastBuy
    lastSale <- last(sellOrders);lastSale
    
    executionPrice <- lastBuy[1, price]; executionPrice
    quantity <- lastBuy[1, executed_qty]; quantity
    
    tickerInfo <- binance_ticker_price(symbol);
    currentPrice <- tickerInfo$price; currentPrice
    
    
    currentYield <- (currentPrice/executionPrice - 1) * 100; currentYield
    
    # Cancel sell order and exit position
    if(currentYield < - 6 | currentYield > 10) {
      
      filters <- binance_filters(symbol);filters
      
      #Sell price digit constraints
      s <- filters[filterType == 'PRICE_FILTER', minPrice];s
      roundPrice <- nchar(strsplit(as.character(s), ".", fixed = T)[[1]][[2]]);roundPrice
      currentPrice <- round(currentPrice, roundPrice);currentPrice
      
      #Sell quantity digit constraints
      t <- filters[filterType == 'LOT_SIZE', minQty];t
      if(t < 1) {
        roundQuantity <- nchar(strsplit(as.character(t), ".", fixed = T)[[1]][[2]]);roundQuantity
        quantity <- round(quantity, roundQuantity);quantity
      } else if (t == 1) {
        quantity <- round(quantity)
      };quantity
      
      
      if(nrow(sellOrders) > 0){
        binance_cancel_order(symbol, order_id = orderID, client_order_id = clientID)
        
        cat(paste0(symbol, " buy order canceled \n \n"))
      }
      
      
      binance_new_order(symbol, side = 'SELL', type = 'LIMIT', price = currentPrice, quantity = quantity, time_in_force = 'GTC', test = F)
      
      cat(paste0(symbol, " position exited for ", round(currentYield, 1), "% time is: ", Sys.time(),"\n \n"))
    }  else if (currentYield >= - 5){
      cat(paste0(symbol, " stop loss not reached. \n \n"))
    }
  }
}



if(nrow(freeAssets) > 0) {
  
  #Exiting free holdings
  foreach(i = 1:nrow(freeAssets), .errorhandling = 'pass', .combine = rbind, .verbose = F) %do% {
    
    coin <- freeAssets[i, asset];coin
    quantity <- freeAssets[i, free];quantity
    
    symbolUSDT <- paste0(coin, 'BUSD')
    symbolETH <- paste0(coin, 'ETH')
    symbolBNB <- paste0(coin, 'BNB')
    
    
    #Aggregate all known orders for locked coins
    if(symbolUSDT %in% coinsUSDT){
      USDTorders <- binance_mytrades(symbolUSDT)
    } else (USDTorders <- data.frame())
    if(symbolETH %in% coinsETH){
      ETHorders <- binance_mytrades(symbolETH)
    } else(ETHorders <- data.frame())
    if(symbolBNB %in% coinsBNB){
      BNBorders <- binance_mytrades(symbolBNB)
    } else(BNBorders <- data.frame())
    
    allOrders <- rbind(USDTorders, ETHorders, BNBorders)
    
    allOrders <- allOrders %>% 
      arrange(time)
    
    buyOrders <- allOrders %>% 
      dplyr::filter(is_buyer == TRUE) %>% 
      dplyr::arrange(time); buyOrders
    
    
    #Filter all open orders by last filled buy
    
    lastBuy <- last(buyOrders);lastBuy
    
    executionPrice <- as.numeric(lastBuy[1, price]); executionPrice
    # quantity <- as.numeric(lastBuy[1, qty]); quantity
    
    symbol <- lastBuy$symbol;symbol
    tickerInfo <- binance_ticker_price(symbol); 
    currentPrice <- tickerInfo$price; currentPrice
    
    
    currentYield <- (currentPrice/executionPrice - 1) * 100; currentYield
    
    # Cancel sell order and exit position
    if(currentYield > -.06 | currentYield > .10) {
      
      filters <- binance_filters(symbol);filters
      
      #Sell price digit constraints
      s <- filters[filterType == 'PRICE_FILTER', minPrice];s
      roundPrice <- nchar(strsplit(as.character(s), ".", fixed = T)[[1]][[2]]);roundPrice
      currentPrice <- round(currentPrice, roundPrice);currentPrice
      
      #Sell quantity digit constraints
      t <- filters[filterType == 'LOT_SIZE', minQty];t
      if(t < 1) {
        roundQuantity <- nchar(strsplit(as.character(t), ".", fixed = T)[[1]][[2]]);roundQuantity
        quantity <- round(quantity, roundQuantity);quantity
      } else if (t == 1) {
        quantity <- round(quantity)
      };quantity 
      
      
      binance_new_order(symbol, side = 'SELL', type = 'LIMIT', price = currentPrice, quantity = quantity, time_in_force = 'GTC', test = F)
      
      return(cat(paste0(symbol, " position exited for ", round(currentYield, 1), "% time is: ", Sys.time(),"\n \n")))
    }  else if (currentYield >= - 5){
      return(cat(paste0(symbol, " stop loss not reached. \n \n")))
    }
  }
}




Sys.time()
