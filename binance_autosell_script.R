library(httr,quietly=TRUE)
library(cowplot,quietly=TRUE)
library(dplyr,quietly=TRUE)
library(data.table,quietly=TRUE)
library(ggplot2,quietly=TRUE)
library(ranger,quietly=TRUE)
library(quantmod,quietly=TRUE)
library(binancer,quietly=TRUE)
library(stringr,quietly=TRUE)


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
  price <- binance_depth(i) %>% .$asks %>% .[1,price] 
  
  binance_new_order(symbol=i,side="BUY",type="LIMIT",quantity = round(30/price,a-1),price=price,test=FALSE,time_in_force = "GTC")
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




li <- fread("./bin_cred.txt",header=FALSE)
key <- paste(do.call(c,li[1]))
secret <- paste(do.call(c,li[2]))

binance_credentials(key=key,secret=secret)


print("Starting the oco sell loop")

balances <- binance_balances(threshold=0,usdt = TRUE)  

#unsold
balances <- balances[free>0.95*total]



for(i in balances[,asset])
{
  
  try({
    name <- i
    i=paste0(i, "USDT")
    
    z <- binancer::binance_filters(i)
    
    
    btc <- tail(binance_klines(i,interval = "15m",limit = 300),5)
    
    s_price <- btc[,min(low)]
    
    a <- log(z[filterType=="PRICE_FILTER",minPrice],base=10)
    
    b <- abs(log(z[filterType=="LOT_SIZE",minQty],base=10))
    
    qt <- binance_balances(usdt = TRUE)  %>% .[asset==name,free]
    
    
    
    
    if (qt!=0)
    {
      
      price <- binance_depth(i) %>% .$asks %>% .[1,price] 
      if (price*qt<35 & price*qt>20)
      {
        #self half for 16 dollars or 14
        binance_new_oco(symbol=i,side = "SELL",quantity = s_round(qt/2,b), price = round(price*1.05,abs(a)), stopprice=round(s_price,abs(a)))
      
        
        qt <- binance_balances(usdt = TRUE)  %>% .[asset==name,free]
        
        binance_new_oco(symbol=i,side = "SELL",quantity = s_round(qt,b), price = round(price*1.075,abs(a)), stopprice=round(s_price,abs(a)))
        
        
      }
      
    }
    
  })
}      

print("finished")