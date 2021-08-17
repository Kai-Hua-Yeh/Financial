library("quantmod")

twii_na <- getSymbols("^TWII",from = "2018-03-01",to = "2018-10-01",src = "yahoo",auto.assign=FALSE) 
twii<- na.omit(twii_na)
head(twii)
close<-twii[,4]               #取出收盤價存入close
names(close) <- "Close"         #變更欄位名稱為Close

sma5 <- SMA(close,5)        #20日均線存入存入sma20
sma05<- na.omit(sma5)
#sma20 <- SMA(close,20)        #20日均線存入存入sma20
#sma60 <- SMA(close,60)        #60日均線存入存入sma60
#sma120 <- SMA(close,120)      #120日均線存入存入sma120
#sma240 <- SMA(close,240)      #240日均線存入存入sma240
#找出紅三兵
red=function(x){
  ifelse(x[,1]<x[,2]&x[,2]<x[,3]&x[,3]<x[,4],1,0)
}
black=function(x){
  ifelse(x[,1]>x[,2]&x[,2]>x[,3]&x[,3]>x[,4],-1,0)
}
### Close #####################################################################
Close1=close;head(Close1);colnames(Close1)="第1天收盤價"
Close2=lag(close,-1);head(Close2);colnames(Close2)="第2天收盤價"
Close3=lag(close,-2);head(Close3);colnames(Close3)="第3天收盤價"
Close4=lag(close,-3);head(Close4);colnames(Close4)="第4天收盤價"
C=na.omit(merge(Close1,Close2,Close3,Close4)) #合併後去除na
View(C)
head(red(C),30)
head(black(C),30)
View(red(C))
View(black(C))

plot(close)
x <- which(red(C) == 1)
points(C[x, "第1天收盤價"],pch=20,col="red",cex=1)
y <- which(black(C) == -1)
points(C[y, "第1天收盤價"],pch=20,col="green",cex=1)
length(x)
length(y)
###############################################################################

### Adjusted ##################################################################
adjusted<-twii[,6]               #取出收盤價存入adjusted
names(adjusted) <- "Adjusted"         #變更欄位名稱為Adjusted
Adj1=adjusted;head(Adj1);colnames(Adj1)="第1天成交價"
Adj2=lag(adjusted,-1);head(Adj2);colnames(Adj2)="第2天成交價"
Adj3=lag(adjusted,-2);head(Adj3);colnames(Adj3)="第3天成交價"
Adj4=lag(adjusted,-3);head(Adj4);colnames(Adj4)="第4天成交價"
Ad=na.omit(merge(Adj1,Adj2,Adj3,Adj4)) #合併後去除na
View(Ad)
head(red(Ad))

head(black(Ad))
Black=matrix(black(Ad))
View(red(Ad))
View(black(Ad))

plot(close)
x <- which(red(Ad) == 1)
points(Ad[x, "第1天成交價"],pch=20,col="red",cex=1)
y <- which(black(Ad) == -1)
points(Ad[y, "第1天成交價"],pch=20,col="green",cex=1)
###############################################################################

### Volume ####################################################################
volume<-twii[,5]               #取出收盤價存入volume
names(volume) <- "Volume"         #變更欄位名稱為Volume
Vol1=volume;head(Vol1);colnames(Vol1)="第1天成交量"
Vol2=lag(volume,-1);head(Vol2);colnames(Vol2)="第2天成交量"
Vol3=lag(volume,-2);head(Vol3);colnames(Vol3)="第3天成交量"
Vol4=lag(volume,-3);head(Vol4);colnames(Vol4)="第4天成交量"
Vol=na.omit(merge(Vol1,Vol2,Vol3,Vol4)) #合併後去除na
View(Vol)
head(red(Vol))
head(black(Vol))

#成交量在close圖上無法繪製
#plot(close)
#m <- which(red(Vol) == 1)
#points(Vol[m, "第1天成交量"],pch=20,col="red",cex=1)
#n <- which(black(Vol) == -1)
#points(Vol[n, "第1天成交量"],pch=50,col="green",cex=1)

both <- cbind(close,sma05)
View(both)
signal=matrix()
for(i in 5:nrow(both)){
  signal[i]=0
  if(both$Close[i] > both$SMA[i]){
    signal[i]=1
  }else if(both$Close[i] < both$SMA[i]){
    signal[i]=-1
  }else{signal[i]=0}
}
View(signal)

G <- cbind(red(Vol),black(Vol))
View(G)

AllSignal<-xts(signal,order.by=index(close))
Alldata<-cbind(AllSignal,G)
View(Alldata)


buy<-which(Alldata$AllSignal == 1 & Alldata$第1天成交量==1)
View(buy)
head(Alldata[buy,"第1天成交量"],30)
sell <- which(Alldata$AllSignal == -1 & Alldata$第1天成交量== 1)
head(Alldata[sell,"第1天成交量"],30)

###############################################################################



##畫k線圖
chartSeries(twii["2018-03-01::2018-10-01"], theme= "white", name = "^TWII", 
            up.col='red',dn.col='green')
addSMA(n=5, col = 'black')
#addSMA(n=20, col = 'black')
#addSMA(n=60, col = 'orange')
#addSMA(n=120, col = 'blue')

