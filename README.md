# R-binance-trading-bot
A basic 100/50 SMA MACD bincance trading bot

------------

The R script searches for SMA100/50 crossevers by all the rules as described in:

https://www.investopedia.com/articles/forex/08/macd-combo.asp

in a 15 minute timeframe


----------------

The repository contains 3 scripts:

> binance_autobuy_script.R  <br/>
> binance_autosell_script.R <br/>
> binance_old_sell.R <br/>

The main wrapper is the autobuy script which then runs the other 2 scripts.

---------------------

For it to run correctly you need to place you binance credentials in a "bin_cred.txt" file, first the public then the secret key, seperated by a new line. The repository also contains all binance coins with their USDT pair, volume sorted, locasted in the "coinlist_vol_sorted.csv" file. The scripts reads all the coins in the file searches for any crossovers, once it finds a crossovers it executes the buy for a value of 30 dollars (changed by the "PurchPower" variable). Next the autosell script places Binance OCO orders. Next the autosell scripts looks in you wallet for all coins which were bought and exectues a sell if they broke the SMA50 line which is the long exit strategy.

The positive long exists are breakeven for first half or 20% increase for the second half (but more likely then not the second exit will happen at the SMA50 fall, as exectued by the old_sell scripts, and described in the link)



----------

For now the script has following functionalities:

 - it performs a check of wallet state in a recursive manner to check if the wallet has enough money to execute a trade
 - checks for coins which already exists in order to prevent double buying
 - if the price falls below SMA50 fro 2 consecutive cnandles the coin is sold

------

TODO 
 - add logging of buys and sells
 - add profit tracker 
 - ad additional signals evaluator functions
 
 
 ```
 
btc <- binance_klines(i,interval="15m",limit=1000)

# 
 #btc[,rsi:=RSI(close,n=8)]
 btc[,sma100:=SMA(close,n=100)]
 btc <- cbind(btc,TTR::BBands(btc[,c("high","low","close")]))
 btc <- cbind(btc,MACD(btc[,close]))
 btc[,vwap:=VWAP(close,volume)]

btc[,nclose:=lead(close,1)]

btc[,growth:=(nclose-close)*100/close]

btc[,nclose:=NULL]

btc[,profitable:="no"]

btc[growth>0.15,profitable:="yes"]



btc[,growth:=NULL]

btc[,profitable:=factor(profitable)]


train <- btc[1:800,-c(7,12)]
test <- btc[800:1000,-c(7,12)]


train <- na.omit(train)

model <- ranger(profitable~.,data=train[,-c(1)],importance = "impurity",mtry = 16,num.trees = 1000)

preds <- predict(model,test)


example <- confusionMatrix(data=preds$predictions, reference = test$profitable,positive="yes")

example

test[,preds:=preds$predictions]

test[,nclose:=lead(close,1)]

test[,growth:=(nclose-close)*100/close]

ggplot(data.frame(importance=model$variable.importance,variable=rownames(cbind(model$variable.importance))), aes(x=reorder(variable,importance), y=importance,fill=importance))+
     geom_bar(stat="identity", position="dodge")+ coord_flip()+
     ylab("Variable Importance")+
     xlab("")+
     scale_fill_gradient(low="red", high="blue")


test <- na.omit(test)

test
test[preds=="yes",sum(growth)]
