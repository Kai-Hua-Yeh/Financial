#install.packages("quantmod") 
library("quantmod")

tw2357 <- getSymbols("2357.TW",from = "2017-01-01",to = Sys.Date(),
                     auto.assign=FALSE) 
rm1_tw2357 <- tw2357[complete.cases(tw2357), ]
head(rm1_tw2357)

close<-rm1_tw2357[,4]              
names(close) <- "Close"       

sma05 <- SMA(close,5)
sma20 <- SMA(close,20)
sma60 <- SMA(close,60)
sma120 <- SMA(close,120)

plot(sma05)                 
lines(sma20, col = "red")     
lines(sma60, col = "blue")  
lines(sma120, col = "green")  
legend("bottomright",
       legend=c("SMA05","SMA20","SMA60","SMA120"),
       col=c("black","red","blue","green"),
       lty=c(1,1,1,1))


chartSeries(close, theme= "white", col = 'black' , name = "TW2357, 2019 ~ now")
addSMA(n=5, col = 'red')
addSMA(n=20, col = 'blue')

chartSeries(rm1_tw2357, theme= "white", name = "TW2357, 2019 ~ now", 
            up.col='red',dn.col='green')

addSMA(n=5, col = 'black')
addSMA(n=20, col = 'red')
addSMA(n=60, col = 'blue')
addSMA(n=120, col = 'green')
legend("bottomright",
       legend=c("SMA05","SMA20","SMA60","SMA120"),
       col=c("black","red","blue","green"),
       lty=c(1,1,1,1))

addMACD(12,26,9)   


#strategy

rm1_tw2357sma<-na.omit(merge(sma05,lag(sma05,1)))
head(rm1_tw2357sma)

rm1_tw2357close<-na.omit(merge(close,lag(close,1)))
rm1_tw2357close<-rm1_tw2357close[-(1:4),]
head(rm1_tw2357close)

sigdata<-rm1_tw2357close-rm1_tw2357sma
View(sigdata)

colnames(sigdata)<-c("close_sma05","lag_close_sma05") 
head(sigdata,3)

cross<-function(x){
  ifelse(x[1]<0&x[2]>0,1,ifelse(x[1]>0&x[2]<0,-1,0))
}

SmaSignal<-apply(sigdata,1,cross)
SmaSignal<-xts(SmaSignal,order.by=index(sigdata))
head(SmaSignal)


SmaTrade<-lag(SmaSignal,2)   
SmaTrade<-na.omit(SmaTrade)
head(SmaTrade)

SmaBuy<-SmaTrade[SmaTrade==1]
length(SmaBuy)
head(SmaBuy)

SmaSell<-SmaTrade[SmaTrade==(-1)]
length(SmaSell)
head(SmaSell)

plot(close)
lines(sma05,col="blue")
# add marker

x <- which(SmaSignal == 1)
View(x)
points(rm1_tw2357close[x, "Close"], pch=17, col="red", cex=1)
y <- which(SmaSignal == -1)
View(y)
points(rm1_tw2357close[y, "Close"], pch=25, col="green", cex=1)


rm1_tw2357ret<-ROC(close,type="discrete") 
names(rm1_tw2357ret)<-"TW50ret"
smaRet<-rm1_tw2357ret*SmaTrade
names(smaRet)<-"smaRet"
head(smaRet)

#install(PerformanceAnalytics)
library(PerformanceAnalytics)
charts.PerformanceSummary(merge(rm1_tw2357ret["2014-01-20/"],smaRet),lty=c(1,6),
                          main="Simple Moving Average Strategy Trading Performance")

win<-smaRet[smaRet>0]
smawin<-length(win)/length(smaRet[smaRet!=0])
smawin

