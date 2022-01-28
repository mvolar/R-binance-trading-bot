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
  
  al <- binance_open_orders() %>% .[price*orig_qty>9 & price*orig_qty<33]

  al[,time_diff:=as.numeric( difftime(Sys.time(),time,units="min") ) ]
  
  #if they upen under both line we should sell them

  
  under <- c()
  for(i in al[,symbol])
  {
    stats <- binance_klines(i,interval="15m")
    stats[,sma100:=SMA(close,n=100)]
    stats[,sma50:=SMA(close,n=50)]
    stats <- tail(stats,5)
    if (((nrow(stats[sma50 > open ])>1 || nrow(stats[sma100 > open ])>1)))
    {
      under <- c(under,i)
      print(under)
    }
    
  }
  
  return(al[symbol %in% under & time_diff > 14 ])
}


replacing_stoploss <- function()
  
{
  
  qt <- binance_balances(usdt = TRUE,threshold = 0)
  
  
  #check those which were halved 
  qt
  name_pair <- paste0(qt[usd>15 & usd < 20,asset],"USDT")
  
  if(name_pair[1]=="USDT") break;
  
  al <- binance_open_orders() 
  
  tmp <-  al[symbol%in%name_pair]#,.N,by=symbol] %>% .[N<=2,symbol]
  
  for ( i in name_pair)
  {
    
    binance_new_oco(symbol=i,side="SELL",price=round(15*1.15/qty,abs(a)),quantity = qt,stopprice =round(15/qty,abs(a)))
    
  }
  
  
  
  #for all in halved delete the existing order move the stoploss at 15 usdt and limitprice at 17 dollars
  for (i in al[symbol%in%tmp,symbol])
  {
    
    try({
    print ("deleting old order")
    
    z <- binancer::binance_filters(i)
    
    a <- log(z[filterType=="PRICE_FILTER",minPrice],base=10)
    
    qty <- al[symbol==i,orig_qty][1]
    
    binance_cancel_order(symbol=i,order_id=al[symbol==i,order_id][1],client_order_id = al[symbol==i,client_order_id][1])
    
    print("creating new increased stoporder at breakeven")
    
    binance_new_oco(symbol=i,side="SELL",price=round(15*1.15/qty,abs(a)),quantity = qt,stopprice =round(15/qty,abs(a)))
    
    })
  }
  
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
   
   print("finding halved")
   
 #  try(replacing_stoploss())
  
}
  
  




























