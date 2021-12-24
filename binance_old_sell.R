library(httr,quietly=TRUE)
library(cowplot,quietly=TRUE)
library(dplyr,quietly=TRUE)
library(data.table,quietly=TRUE)
library(ggplot2,quietly=TRUE)
library(ranger,quietly=TRUE)
library(quantmod,quietly=TRUE)
library(binancer,quietly=TRUE)
library(stringr,quietly=TRUE)




s_round <- function(a,b)
{
  c = a
  while(round(c,b)>a)
  {
    c=c*0.99
  }
  return(round(c,b))
}


binance_sell <- function(i)
{
  a <- binancer::binance_filters(i) %>% .[filterType=="LOT_SIZE",minQty]
  #tot am
  name=str_remove(i,"USDT")
  
  
  
  if (a<1){a <- abs(log(a,base=10))}
  
  qt <- binance_balances(usdt = TRUE)  %>% .[asset==name,free]
  
  price <- binance_depth(i) %>% .$bids %>% .[1,price] 
  
  binance_new_order(symbol=i,side="SELL",type="LIMIT",quantity = s_round(qt,a),price=price,test=FALSE,time_in_force = "GTC")
  
}


finding_old <- function()
  
{
  
  al <- binance_open_orders() %>% .[price*orig_qty>10 & price*orig_qty<33]

  al[,time_diff:=as.numeric( difftime(Sys.time(),time,units="min") ) ]
  
  #if they upen under both line we should sell them

  
  under <- c()
  for(i in al[,symbol])
  {
    stats <- binance_klines(i,interval="15m")
    stats[,sma100:=SMA(close,n=100)]
    stats[,sma50:=SMA(close,n=50)]
    stats <- tail(stats,5)
    if ((nrow(stats[sma50 > close ])>2 & nrow(stats[sma50 > close ])>2))
    {
      under <- c(under,i)
      print(under)
    }
    
  }
  
  return(al[symbol %in% under & time_diff > 45 ])
}


li <- fread("./bin_cred.txt",header=FALSE)
key <- paste(do.call(c,li[1]))
secret <- paste(do.call(c,li[2]))

binance_credentials(key=key,secret=secret)




print("Finding old coins:")



old <- finding_old()

old<- old[type=="STOP_LOSS_LIMIT"]

for (i in old[,symbol])
{
  
  
    
    
      print("we have an old order")
    print(i)
    print("trying to delete the order")  
    try({
        binance_cancel_order(symbol=i,order_id=old[symbol==i,order_id][1],client_order_id = old[symbol==i,client_order_id][1])
       })
    print("deleting order,executing sell")
    try(binance_sell(i))
   print("executed sell")
  
}
  
  




























