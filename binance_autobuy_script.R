library(httr,quietly=TRUE)
library(cowplot,quietly=TRUE)
library(dplyr,quietly=TRUE)
library(data.table,quietly=TRUE)
library(ggplot2,quietly=TRUE)
library(ranger,quietly=TRUE)
library(quantmod,quietly=TRUE)
library(binancer,quietly=TRUE)
library(stringr,quietly=TRUE)

setwd("C:/Users/Visitor/Desktop/bin_logs/")



PurchPower <- 30

evaluator <- function(coin,x)
{
  
  #coin <- binance_klines("PERPUSDT",interval="15m")
  
  #coin[,rsi:=RSI(coin[,c("close")],n=12)]
  coin[,sma100:=SMA(close,n=100)]
  coin[,sma50:=SMA(close,n=50)]
  macd <- MACD(coin[,close])
  coin <- cbind(coin,macd)
  coin <- tail(coin,x)
  
  state <- 1
  #state <- as.numeric(coin[1,sma]<0.95*coin[x,sma])*state
  ## 2 buy conditions, either the prica breaks above the sma line or macd cross when price above the sma lines, so first condition price
  #breaks through the sma lines, recently in terms of macd
  if (coin[x-1,sma100] >coin[x-1,open] || coin[x-1,sma50]>coin[x-1,open])
  {
    
    coin <- tail(coin,10)
    x <- 10
    state <- as.numeric(coin[x,sma100] <coin[x,close] & coin[x,sma50] <coin[x,close])*state 
    state <- as.numeric(nrow(coin[sma100 >open])>x-4 || nrow(coin[sma50 > open])>x-4)*state
    state <- as.numeric(nrow(coin[macd>signal])<x-6 || nrow(coin[macd>signal])>x-2 )*state 
    
  }else {
    
    state <- as.numeric(nrow(coin[sma100>close])>5 || nrow(coin[sma50>close])>5)*state
    
    
    
    y = x-10
    
    
    coin <- tail(coin,y)
    state <- as.numeric(coin[y,sma100] <coin[y,close] & coin[y,sma50] <coin[y,close])*state
    
    state <- as.numeric(nrow(coin[macd<signal])>y-2)*state
    state <- as.numeric(nrow(coin[y][macd>signal])>=1)*state
  }


  
return(state)
}





wallet_check <- function()
{
  a <- binance_balances() %>% .[asset=="USDT",free]
  if (a>30)
  {
    
    return(a)
    
  }
  else {
    return(0)
  }
  
  
}


s_round <- function(a,b)
{
  c = a
  while(round(c,b)>a)
  {
    c=c*0.99
  }
  return(round(c,b))
}

binance_buy <- function(i)
{
  a <- binancer::binance_filters(i) %>% .[filterType=="LOT_SIZE",minQty]
  #tot am
 
  
  if (a<1){a <- abs(log(a,base=10))}
  
  if (a%%1!=0) {stop()}
  
  price <- binance_depth(i) %>% .$asks %>% .[1,price] 
  
  binance_new_order(symbol=i,side="BUY",type="LIMIT",quantity = round(PurchPower/price,a-1),price=price,test=FALSE,time_in_force = "GTC")
}


binance_new_oco <- function(symbol,side,price,stopprice,quantity) {
  
  
  params <- list(symbol   = symbol,
                 side     = side,
                 price    = price,
                 stopLimitPrice = stopprice,
                 stopPrice = stopprice, 
                 quantity = quantity,
                 stopLimitTimeInForce = "GTC")    
  
  
  
  
  ord <- binancer:::binance_query(endpoint = 'api/v3/order/oco', method = 'POST', params = params, sign = TRUE)
  
  
}    



try(source("./binance_old_sell.R"))

try(source("./binance_old_sell.R"))



li <- fread("./bin_cred.txt",header=FALSE)
key <- paste(do.call(c,li[1]))
secret <- paste(do.call(c,li[2]))

binance_credentials(key=key,secret=secret)

# find the coins

coinlist <- fread("./coinlist_vol_sorted.csv",header=FALSE) %>% unlist(.) %>% as.array(.) %>% as.character() %>% str_remove_all(.,"/") 



#{ lots <- fread("coin_lots.csv")

## lot sizes
# coin_lots <- data.table()
# for (i in coinlist)
# {
#   a <- binancer::binance_filters(i) 
#   a1 <-  a %>% .[filterType=="LOT_SIZE",minQty]
#   a2 <- a %>% .[filterType=="PRICE_FILTER",minPrice]
#   cl_tmp <- cbind(name=i,lot=a1,price=a2)
#   coin_lots <- rbind(coin_lots,cl_tmp)
# }
# 
# write.csv(coin_lots,"coin_lots.csv")

# coins <- binance_coins()
#  sink("test.txt")
#  
# for (i in coins)
#  {  
#   try({
#     btc <- binance_klines(symbol = paste0(i,"USDT"),interval = "1d",limit=1)
#     cat(unlist(tail(btc,1)),"\n")
#  })
# }
#  
#  sink()


# fread("test.txt") %>% .[,mass:=V8*V1] %>% .[order(-mass)] %>% .[,V12] %>% write.csv(.,"coinlist_vol_sorted.csv",row.names = FALSE,quote=FALSE)
# btc


# check wallet state


# check all coins for ema 100 break, tkae last sort by volumne, take only the heighest volume

options(scipen=100000)

money <- wallet_check()

if (money<PurchPower){
  print("wallet empty, pls refill")
  source("./binance_autosell_script.R")
  quit()
}



#the buy loop


print(paste("starting the buy loop\n",Sys.time()))
for (i in coinlist)
{
  if (money<PurchPower) break()
  
  
  
  
  try({
    
    #if (grep("DOWNUSDT",i)==TRUE) next()
    
    #if (grep("UPUSDT",i)==TRUE) next()
    
    btc <- binance_klines(i,interval="15m")
    print(i)
    print(evaluator(btc,20))
    if(evaluator(btc,20)==1)
    {
      
      
      name <- str_remove(i,"USDT")
      
      qt <- binance_balances(usdt = TRUE)  %>% .[asset==name,usd]
      
      
      if(qt>10) {
        next
      }
      money=money-PurchPower
      print("we have a buy signal")
      binance_buy(i)
      print(paste("bought:",i))
    }
    
    
    })
}


source("./binance_autosell_script.R")



#attempting sell for all which are between 20 and 40 and are unlocked
#cycle_buy <- c("PORTOUSDT")

#sink()
