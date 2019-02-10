#Mindset:
#比特幣的價格波動來自：
# 1.前一期的價格（時間序列資料）
# 2.保值貨幣價格、浮動性 （黃金、主要貨幣）
# 3.市場對加密貨幣的需求
# 4.市場上其他投機性高的標的狀況：美股、以太幣
# 5.消息面的影響：討論熱度

#Data Import (For real time data)
#install.packages('Quandl')
library(quantmod)
library(Quandl)
getFX('BTC/USD',from='2017-07-15')
getFX('EUR/USD',from='2017-07-15')
getFX('CNY/USD',from='2017-07-15')
getFX('JPY/USD',from='2017-07-15')
getMetals('gold',from='2017-07-15')
getFX('ETH/USD',from='2017-07-15')
getSymbols("^DJI", from='2017-07-15')
getSymbols("^GSPC",from='2017-07-15')
getSymbols("^NDX",from='2017-07-15')
ETH <- Quandl("BITFINEX/ETHUSD", api_key="-GNJxjPntak8s-AxpM5o",type = "xts",start_date="2017-07-15")
ETH=ETH[,4]
colnames(ETH)='ETH'
DJI=DJI[,4]
GSPC=GSPC[,4]
NDX=NDX[,4]
library(xts)
library('gtrendsR')
bit_trend_web <- gtrends('bitcoin', time="2017-07-15 2018-01-13")$interest_over_time
bit_trend = xts(bit_trend_web, order.by = as.Date(bit_trend_web$date, "%Y-%m-%d"))[,2]
bit_trend$hit=as.numeric(bit_trend$hits)
bit_trend=bit_trend[,1]
all = cbind(BTCUSD,EURUSD,CNYUSD,JPYUSD,XAUUSD,DJI,GSPC,NDX,ETH,bit_trend)
d=all
write.csv(all,file='newalldata')
data2=data[complete.cases(data),]

#EDA
require(TTR)
#Price of bitcoin
chartSeries(BTCUSD,theme="white") 
#addMACD()
#addATR()
addBBands()
#addBBands(draw="p")
#addEMA(n=8, col="lightblue")
#addEMA(n=21, col="blue")
par(mfrow=c(2,3))
chart_Series(BTCUSD)
chart_Series(ETH)
chart_Series(CNYUSD)
chart_Series(bit_trend)
chart_Series(XAUUSD)
chart_Series(DJI)


add_TA(data2$hits, on = 5)
add_TA(RSI(data2$BTC.USD), on = NA, col = "red", lty = 3)


####Google Trends
#install.packages('gtrendsR')


par(mfrow=c(1,3))
plot(bit_trend_web)
plot(bit_trend_new)
bit_trend_new$interest_over_time$hits

####Start code
library(rethinking)
library(rstan)
d=read.csv('newalldata')
head(d)
d=as.data.frame(d)
d$dBTC = c(diff(d$BTC.USD)/d$BTC.USD[-length(d$BTC.USD)],NA)
d$dEUR = c(diff(d$EUR.USD)/d$EUR.USD[-length(d$EUR.USD)],NA)
d$dCNY = c(diff(d$CNY.USD)/d$CNY.USD[-length(d$CNY.USD)],NA)
d$dJPY = c(diff(d$JPY.USD)/d$JPY.USD[-length(d$JPY.USD)],NA)
d$dGOLD = c(diff(d$XAU.USD)/d$XAU.USD[-length(d$XAU.USD)],NA)
d$dDJI = c(diff(d$DJI.Close)/d$BTC.USD[-length(d$DJI.Close)],NA)
d$dGSPC = c(diff(d$GSPC.Close)/d$GSPC.Close[-length(d$GSPC.Close)],NA)
d$dNDX = c(diff(d$NDX.Close)/d$NDX.Close[-length(d$NDX.Close)],NA)
d$dETH = c(diff(d$ETH)/d$ETH[-length(d$ETH)],NA)
d$dhits = c(diff(d$hits)/d$hits[-length(d$hits)],NA)
data=d[complete.cases(d),]
pairs(~dBTC+dEUR+dCNY+dJPY+dGOLD+dDJI+dGSPC+dNDX+dETH+hits+dhits,data=data, col=rangi2 )

f1 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bG*dGOLD,
  a ~ dnorm( 0 , 5 ) ,
  bG ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

f2 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bC*dCNY,
  a ~ dnorm( 0 , 5 ) ,
  bC ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

f3 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bC*dCNY+bG*dGOLD,
  a ~ dnorm( 0 , 5 ) ,
  bC ~ dnorm( 0 , 1 ) ,
  bG ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)
m1 <- map(
  f1 ,
  data=data)
m2 <- map(
  f2 ,
  data=data)
m3 <- map(
  f3 ,
  data=data)

f4 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bE*dETH,
  a ~ dnorm( 0 , 5 ) ,
  bE ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

f5 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bJ*dJPY,
  a ~ dnorm( 0 , 5 ) ,
  bJ ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

f6 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bEU*dEUR,
  a ~ dnorm( 0 , 5 ) ,
  bEU ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

f7 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bEU*dEUR+bE*dETH+bJ*dJPY,
  a ~ dnorm( 0 , 5 ) ,
  bEU ~ dnorm( 0 , 1 ) ,
  bE ~ dnorm( 0 , 1 ) ,
  bJ ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)
m4 <- map(
  f4 ,
  data=data)
m5 <- map(
  f5 ,
  data=data)
m6 <- map(
  f6 ,
  data=data)
m7 <- map(
  f7 ,
  data=data)

f8 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a +bC*dCNY+bG*dGOLD+bEU*dEUR+bE*dETH+bJ*dJPY,
  a ~ dnorm( 0 , 2 ) ,
  bC ~ dnorm( 0 , 1 ) ,
  bG ~ dnorm( 0 , 1 ) ,
  bEU ~ dnorm( 0 , 1 ) ,
  bE ~ dnorm( 0 , 1 ) ,
  bJ ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)
m8 <- map(
  f8 ,
  data=data)

###

f9 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bD*dDJI,
  a ~ dnorm( 0 , 5 ) ,
  bD ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

f10 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bGS*dGSPC,
  a ~ dnorm( 0 , 5 ) ,
  bGS ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

f11 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bN*dNDX,
  a ~ dnorm( 0 , 5 ) ,
  bN ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

f12 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bN*dNDX+bGS*dGSPC+bD*dDJI,
  a ~ dnorm( 0 , 5 ) ,
  bN ~ dnorm( 0 , 1 ) ,
  bGS ~ dnorm( 0 , 1 ) ,
  bD ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)

m9 <- map(
  f9 ,
  data=data)
m10 <- map(
  f10 ,
  data=data)
m11 <- map(
  f11 ,
  data=data)
m12 <- map(
  f12 ,
  data=data)


f13 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a +bC*dCNY+bG*dGOLD+bEU*dEUR+bE*dETH+bJ*dJPY+bD*dDJI+bh*hits,
  a ~ dnorm( 0 , 5 ) ,
  bC ~ dnorm( 0 , 1 ) ,
  bG ~ dnorm( 0 , 1 ) ,
  bEU ~ dnorm( 0 , 1 ) ,
  bE ~ dnorm( 0 , 1 ) ,
  bJ ~ dnorm( 0 , 1 ) ,
  bh ~ dnorm( 0 , 1 ) ,
  bD ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)
m13 <- map(
  f13 ,
  data=data)

#####
m4.1=map2stan(
  f4 ,
  data=data,iter=2000 , warmup=1000 , chains=2 )

m7.1=map2stan(
  f7 ,
  data=data,iter=2000 , warmup=1000 , chains=2 )

m9.1=map2stan(
  f9 ,
  data=data,iter=2000 , warmup=1000 , chains=2 )

m8.1=map2stan(
  f8 ,
  data=data,iter=2000 , warmup=1000 , chains=2 )


m12.1=map2stan(
  f12 ,
  data=data,iter=2000 , warmup=1000 , chains=2)
m13.1=map2stan(
  m13 ,
  data=data,iter=2000 , warmup=1000 , chains=2 )

f14 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bD*dDJI+bE*dETH,
  a ~ dnorm( 0 , 5 ) ,
  bE ~ dnorm( 0 , 1 ) ,
  bD ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)
m14=map(f14,data=data)


m14.1=map2stan(
  f14 ,
  data=data,iter=2000 , warmup=1000 , chains=2 )

f15 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bh*hits,
  a ~ dnorm( 0 , 5 ) ,
  bh ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)
m15=map(f15,data=data)

m15.1=map2stan(f15,data=data,iter=2000 , warmup=1000 , chains=2 )
f16 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bH*dhits,
  a ~ dnorm( 0 , 5 ) ,
  bH ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)
m16=map(f16,data=data)
m16.1=map2stan(f16,data=data,iter=2000 , warmup=1000 , chains=2)
f17 =  alist(
  dBTC ~ dnorm( mu , sigma ) ,
  mu <- a + bD*dDJI+bE*dETH+bH*dhits,
  a ~ dnorm( 0 , 5 ) ,
  bE ~ dnorm( 0 , 1 ) ,
  bD ~ dnorm( 0 , 1 ) ,
  bH ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
)
m17=map(f17,data=data)
m17.1=map2stan(f17,data=data,iter=2000 , warmup=1000 , chains=2)
cp1 = compare(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17)
com1 = compare(m4.1,m7.1,m9.1,m8.1,m13.1,m14.1,m15.1,m16.1,m17.1)
coe1 = coeftab(m4.1,m7.1,m9.1,m8.1,m13.1,m14.1,m15.1,m16.1,m17.1)

####
par(mfrow=c(1,3))
DJI.seq <- seq(from=-0.07,to=0.05,length.out=94)

##Prediction of DJI
d.pred <- data.frame(
  dDJI = DJI.seq,
  dCNY = rep(data$dCNY[50],94),
  dGOLD= rep(data$dGOLD[50],94),
  dEUR= rep(data$dEUR[50],94),
  dETH= rep(data$dETH[50],94),
  dJPY= rep(data$dJPY[50],94),
  dNDX= rep(data$dNDX[50],94),
  dGSPC= rep(data$dGSPC[50],94),
  hits= rep(data$hits[50],94),
  dhits= rep(data$dhits[50],94)
)
plot( data$dDJI , data$dBTC , col=rangi2 ,
      xlab="DJI" , ylab="BTC" )
bit.ensemble <- ensemble( m14.1,m4.1,m9.1,m17.1,m7.1,m8.1 , data=d.pred)
bit.mean <- apply( bit.ensemble$link , 2 , mean )
bit.PI <- apply( bit.ensemble$sim , 2 , PI )
bit.PIl<- apply( bit.ensemble$link , 2 , PI )
lines( DJI.seq, bit.mean , col=rangi2 )
shade( bit.PIl , DJI.seq , col=col.alpha(rangi2,0.4) )
shade( bit.PI , DJI.seq , col=col.alpha(rangi2,0.2) )


##ETH
##Prediction of DJI
ETH.seq <- seq(-0.2,0.21,length.out = 94)
d.pred <- data.frame(
  dETH = ETH.seq,
  dCNY = rep(data$dCNY[50],94),
  dGOLD= rep(data$dGOLD[50],94),
  dEUR= rep(data$dEUR[50],94),
  dDJI= rep(data$dDJI[50],94),
  dJPY= rep(data$dJPY[50],94),
  dNDX= rep(data$dNDX[50],94),
  dGSPC= rep(data$dGSPC[50],94),
  hits= rep(data$hits[50],94),
  dhits= rep(data$dhits[50],94)
)
plot( data$dETH , data$dBTC , col=rangi2 ,
      xlab="ETH" , ylab="BTC" )
bit.ensemble <- ensemble(m14.1,m4.1,m9.1,m17.1,m7.1,m8.1 , data=d.pred)
bit.mean <- apply( bit.ensemble$link , 2 , mean )
bit.PI <- apply( bit.ensemble$sim , 2 , PI )
bit.PIl<- apply( bit.ensemble$link , 2 , PI )
lines( ETH.seq, bit.mean , col=rangi2 )
shade( bit.PIl , ETH.seq , col=col.alpha(rangi2,0.4) )
shade( bit.PI , ETH.seq , col=col.alpha(rangi2,0.2) )
##Google Trends
GT.seq <- seq(-0.4,0.98,length.out = 94)
d.pred.GT <- data.frame(
  dhits = GT.seq,
  dCNY = rep(data$dCNY[50],94),
  dGOLD= rep(data$dGOLD[50],94),
  dEUR= rep(data$dEUR[50],94),
  dDJI= rep(data$dDJI[50],94),
  dETH= rep(data$dETH[50],94),
  dJPY= rep(data$dJPY[50],94),
  dNDX= rep(data$dNDX[50],94),
  dGSPC= rep(data$dGSPC[50],94),
  hits= rep(data$hits[50],94)
)
bit.ensemble.GT <- ensemble( m14.1,m4.1,m9.1,m17.1,m7.1,m8.1 , data=d.pred.GT)
bit.mean.GT <- apply( bit.ensemble.GT$link , 2 , mean )
bit.PIl.GT <- apply( bit.ensemble.GT$link , 2 , PI )
bit.PI.GT <- apply( bit.ensemble.GT$sim , 2 , PI )
#Plot
plot( data$dhits , data$dBTC , col=rangi2 ,
      xlab="Google Trend Hits" , ylab="BTC" )
lines( GT.seq, bit.mean.GT , col=rangi2 )
shade( bit.PIl.GT , GT.seq , col=col.alpha(rangi2,0.4) )
shade( bit.PI.GT , GT.seq , col=col.alpha(rangi2,0.2) )

#####
data1 = data[1:60,]

m1 <- map(
  f1 ,
  data=data1)
m2 <- map(
  f2 ,
  data=data1)
m3 <- map(
  f3 ,
  data=data1)

m4 <- map(
  f4 ,
  data=data1)
m5 <- map(
  f5 ,
  data=data1)
m6 <- map(
  f6 ,
  data=data1)
m7 <- map(
  f7 ,
  data=data1)


m8 <- map(
  f8 ,
  data=data1)


m9 <- map(
  f9 ,
  data=data1)
m10 <- map(
  f10 ,
  data=data1)
m11 <- map(
  f11 ,
  data=data1)
m12 <- map(
  f12 ,
  data=data1)


m13 <- map(
  f13 ,
  data=data1)

#####
m4.1.1=map2stan(
  f4 ,
  data=data1,iter=2000 , warmup=1000 , chains=2 )

m7.1.1=map2stan(
  f7 ,
  data=data1,iter=2000 , warmup=1000 , chains=2 )

m9.1.1=map2stan(
  f9 ,
  data=data1,iter=2000 , warmup=1000 , chains=2)

m8.1.1=map2stan(
  f8 ,
  data=data1,iter=2000 , warmup=1000 , chains=2 )


m12.1.1=map2stan(
  f12 ,
  data=data1,iter=2000 , warmup=1000 , chains=2 )
m13.1.1=map2stan(
  f13 ,
  data=data1,iter=2000 , warmup=1000 , chains=2 )

m14=map(f14,data=data1)

m14.1.1=map2stan(
  f14 ,data=data1,iter=2000 , warmup=1000 , chains=2)

m15=map(f15,data=data1)

m15.1.1=map2stan(f15,data=data1,iter=2000 , warmup=1000 , chains=2)
m16=map(f16,data=data1)

m16.1.1=map2stan(f16,data=data1,iter=2000 , warmup=1000 , chains=2)
m17=map(f17,data=data1)
m17.1.1=map2stan(f17,data=data1,iter=2000 , warmup=1000 , chains=2)
cp2 = compare(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17)
com2 = compare(m4.1.1,m7.1.1,m9.1.1,m8.1.1,m13.1.1,m14.1.1,m15.1.1,m16.1.1,m17.1.1)
coe2 = coeftab(m4.1.1,m7.1.1,m9.1.1,m8.1.1,m13.1.1,m14.1.1,m15.1.1,m16.1.1,m17.1.1)



###Counterfactual analysis
par(mfrow=c(1,3))
DJI1.seq <- seq(from=-0.07,to=0.05,length.out=60)

##Prediction of DJI
d.pred.DJI1 <- data.frame(
  dDJI = DJI1.seq,
  dCNY = rep(mean(data1$dCNY),60),
  dGOLD= rep(mean(data1$dGOLD),60),
  dEUR= rep(mean(data1$dEUR),60),
  dETH= rep(mean(data1$dETH),60),
  dJPY= rep(mean(data1$dJPY),60),
  dNDX= rep(mean(data1$dNDX),60),
  dGSPC= rep(mean(data1$dGSPC),60),
  dhits= rep(mean(data1$dhits),60),
  hits= rep(mean(data1$hits),60)
)

bit.ensemble.DJI1 <- ensemble( m13.1.1,m14.1.1,m8.1.1,m7.1.1,m17.1.1, data=d.pred.DJI1)
bit.mean.DJI1 <- apply( bit.ensemble.DJI1$link , 2 , mean )
bit.PIl.DJI1 <- apply( bit.ensemble.DJI1$link , 2 , PI )
bit.PI.DJI1 <- apply( bit.ensemble.DJI1$sim , 2 , PI )

plot( data1$dDJI , data1$dBTC , col=rangi2 ,
      xlab="DJI" , ylab="BTC" )+
  lines( DJI1.seq, bit.mean.DJI1 , col=rangi2 )+
  shade( bit.PIl.DJI1 , DJI1.seq , col=col.alpha(rangi2,0.4) )+
  shade( bit.PI.DJI1 , DJI1.seq , col=col.alpha(rangi2,0.2) )

##ETH
##Prediction of ETH
ETH1.seq <- seq(-0.19,0.21,length.out = 60)
d.pred.ETH1 <- data.frame(
  dETH = ETH1.seq,
  dCNY = rep(mean(data1$dCNY),60),
  dGOLD= rep(mean(data1$dGOLD),60),
  dEUR= rep(mean(data1$dEUR),60),
  dDJI= rep(mean(data1$dDJI),60),
  dJPY= rep(mean(data1$dJPY),60),
  dNDX= rep(mean(data1$dNDX),60),
  dGSPC= rep(mean(data1$dGSPC),60),
  dhits= rep(mean(data1$dhits),60),
  hits= rep(mean(data1$hits),60)
)

bit.ensemble.ETH1 <- ensemble( m13.1.1,m14.1.1,m8.1.1,m7.1.1,m17.1.1, data=d.pred.ETH1)
bit.mean.ETH1 <- apply( bit.ensemble.ETH1$link , 2 , mean )
bit.PIl.ETH1 <- apply( bit.ensemble.ETH1$link , 2 , PI )
bit.PI.ETH1 <- apply( bit.ensemble.ETH1$sim , 2 , PI )

plot( data1$dETH , data1$dBTC , col=rangi2 ,
      xlab="ETH" , ylab="BTC" )+
  lines( ETH1.seq, bit.mean.ETH1 , col=rangi2 )+
  shade( bit.PIl.ETH1 , ETH1.seq , col=col.alpha(rangi2,0.4) )+
  shade( bit.PI.ETH1 , ETH1.seq , col=col.alpha(rangi2,0.2) )

##Google Trend

GT1.seq <- seq(-0.37,0.67,length.out = 60)
d.pred.GT1 <- data.frame(
  dhits = GT1.seq,
  dCNY = rep(mean(data1$dCNY),60),
  dGOLD= rep(mean(data1$dGOLD),60),
  dEUR= rep(mean(data1$dEUR),60),
  dDJI= rep(mean(data1$dDJI),60),
  dETH= rep(mean(data1$dETH),60),
  dJPY= rep(mean(data1$dJPY),60),
  dNDX= rep(mean(data1$dNDX),60),
  dGSPC= rep(mean(data1$dGSPC),60),
  hits= rep(mean(data1$hits),60)
)
bit.ensemble.GT1 <- ensemble( m13.1.1,m14.1.1,m8.1.1,m7.1.1,m17.1.1, data=d.pred.GT1)
bit.mean.GT1 <- apply( bit.ensemble.GT1$link , 2 , mean )
bit.PIl.GT1 <- apply( bit.ensemble.GT1$link , 2 , PI )
bit.PI.GT1 <- apply( bit.ensemble.GT1$sim , 2 , PI )
plot( data1$dhits , data1$dBTC , col=rangi2 ,
      xlab="Google Trend Hits" , ylab="BTC" )+
  lines( GT1.seq, bit.mean.GT1 , col=rangi2 )+
  shade( bit.PIl.GT1 , GT1.seq , col=col.alpha(rangi2,0.4) )+
  shade( bit.PI.GT1 , GT1.seq , col=col.alpha(rangi2,0.2) )


####

data2 = data[61:94,]

m1 <- map(
  f1 ,
  data=data2)
m2 <- map(
  f2 ,
  data=data2)
m3 <- map(
  f3 ,
  data=data2)
m4 <- map(
  f4 ,
  data=data2)
m5 <- map(
  f5 ,
  data=data2)
m6 <- map(
  f6 ,
  data=data2)
m7 <- map(
  f7 ,
  data=data2)
m8 <- map(
  f8 ,
  data=data2)
m9 <- map(
  f9 ,
  data=data2)
m10 <- map(
  f10 ,
  data=data2)
m11 <- map(
  f11 ,
  data=data2)
m12 <- map(
  f12 ,
  data=data2)
m13 <- map(
  f13 ,
  data=data2)

#####
m4.1.2=map2stan(
  f4 ,
  data=data2,iter=2000 , warmup=1000 , chains=2 )

m7.1.2=map2stan(
  f7 ,
  data=data2,iter=2000 , warmup=1000 , chains=2 )

m9.1.2=map2stan(
  f9 ,
  data=data2,iter=2000 , warmup=1000 , chains=2 )

m8.1.2=map2stan(
  f8 ,
  data=data2,iter=2000 , warmup=1000 , chains=2 )


m12.1.2=map2stan(
  f12 ,
  data=data2,iter=2000 , warmup=1000 , chains=2 )
m13.1.2=map2stan(
  f13 ,
  data=data2,iter=2000 , warmup=1000 , chains=2 )

m14=map(f14,data=data2)
m14.1.2=map2stan(
  f14 ,
  data=data2,iter=2000 , warmup=1000 , chains=2 )

m15=map(f15,data=data2)


m15.1.2=map2stan(f15,data=data2,iter=2000 , warmup=1000 , chains=2 )
m16=map(f16,data=data2)

m16.1.2=map2stan(f16,data=data2,iter=2000 , warmup=1000 , chains=2 )
m17=map(f17,data=data2)
m17.1.2=map2stan(f17,data=data2,iter=2000 , warmup=1000 , chains=2)
cf3 = compare(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17)
com3 =compare(m4.1.2,m7.1.2,m9.1.2,m8.1.2,m13.1.2,m14.1.2,m15.1.2,m16.1.2,m17.1.2)
coe3 = coeftab(m4.1.2,m7.1.2,m9.1.2,m8.1.2,m13.1.2,m14.1.2,m15.1.2,m16.1.2,m17.1.2)



###Counterfactual analysis

DJI2.seq <- seq(from=-0.021,to=0.032,length.out=34)

##Prediction of DJI
d.pred.DJI2 <- data.frame(
  dDJI = DJI2.seq,
  dCNY = rep(mean(data2$dCNY),34),
  dGOLD= rep(mean(data2$dGOLD),34),
  dEUR= rep(mean(data2$dEUR),34),
  dETH= rep(mean(data2$dETH),34),
  dJPY= rep(mean(data2$dJPY),34),
  dNDX= rep(mean(data2$dNDX),34),
  dGSPC= rep(mean(data2$dGSPC),34),
  dhits= rep(mean(data2$dhits),34),
  hits= rep(mean(data2$hits),34)
)
bit.ensemble.DJI2 <- ensemble( m9.1.2,m8.1.2,m7.1.2,m16.1.2,m4.1.2,m14.1.2, data=d.pred.DJI2)
bit.mean.DJI2 <- apply( bit.ensemble.DJI2$link , 2 , mean )
bit.PIl.DJI2 <- apply( bit.ensemble.DJI2$link , 2 , PI )
bit.PI.DJI2 <- apply( bit.ensemble.DJI2$sim , 2 , PI )

plot( data2$dDJI , data2$dBTC , col=rangi2 ,
      xlab="DJI" , ylab="BTC" )+
  lines( DJI2.seq, bit.mean.DJI2 , col=rangi2 )+
  shade( bit.PIl.DJI2 , DJI2.seq , col=col.alpha(rangi2,0.4) )+
  shade( bit.PI.DJI2 , DJI2.seq , col=col.alpha(rangi2,0.2) )

##ETH
##Prediction of DJI
ETH2.seq <- seq(-0.1,0.2,length.out = 34)
d.pred.ETH2 <- data.frame(
  dETH = ETH2.seq,
  dCNY = rep(mean(data2$dCNY),34),
  dGOLD= rep(mean(data2$dGOLD),34),
  dEUR= rep(mean(data2$dEUR),34),
  dDJI= rep(mean(data2$dDJI),34),
  dJPY= rep(mean(data2$dJPY),34),
  dNDX= rep(mean(data2$dNDX),34),
  dGSPC= rep(mean(data2$dGSPC),34),
  dhits= rep(mean(data2$dhits),34),
  hits= rep(mean(data2$hits),34)
)

bit.ensemble.ETH2 <- ensemble( m9.1.2,m8.1.2,m7.1.2,m16.1.2,m4.1.2,m14.1.2, data=d.pred.ETH2)
bit.mean.ETH2 <- apply( bit.ensemble.ETH2$link , 2 , mean )
bit.PIl.ETH2 <- apply( bit.ensemble.ETH2$link , 2 , PI )
bit.PI.ETH2 <- apply( bit.ensemble.ETH2$sim , 2 , PI )

plot( data2$dETH , data2$dBTC , col=rangi2 ,
      xlab="ETH" , ylab="BTC" )+
  lines( ETH2.seq, bit.mean.ETH2 , col=rangi2 )+
  shade( bit.PIl.ETH2 , ETH2.seq , col=col.alpha(rangi2,0.4) )+
  shade( bit.PI.ETH2 , ETH2.seq , col=col.alpha(rangi2,0.2) )

##Google Trend

GT2.seq <- seq(-0.37,0.98,length.out = 34)
d.pred.GT2 <- data.frame(
  dhits = GT2.seq,
  dCNY = rep(mean(data2$dCNY),34),
  dGOLD= rep(mean(data2$dGOLD),34),
  dEUR= rep(mean(data2$dEUR),34),
  dDJI= rep(mean(data2$dDJI),34),
  dETH= rep(mean(data2$dETH),34),
  dJPY= rep(mean(data2$dJPY),34),
  dNDX= rep(mean(data2$dNDX),34),
  dGSPC= rep(mean(data2$dGSPC),34),
  hits= rep(mean(data2$hits),34)
)

bit.ensemble.GT2 <- ensemble(m9.1.2,m8.1.2,m7.1.2,m16.1.2,m4.1.2,m14.1.2, data=d.pred.GT2)
bit.mean.GT2 <- apply( bit.ensemble.GT2$link , 2 , mean )
bit.PIl.GT2 <- apply( bit.ensemble.GT2$link , 2 , PI )
bit.PI.GT2 <- apply( bit.ensemble.GT2$sim , 2 , PI )

plot( data2$dhits , data2$dBTC , col=rangi2 ,
      xlab="Google Trend Hits" , ylab="BTC" )+
  lines( GT2.seq, bit.mean.GT2 , col=rangi2 )+
  shade( bit.PIl.GT2 , GT2.seq , col=col.alpha(rangi2,0.4) )+
  shade( bit.PI.GT2 , GT2.seq , col=col.alpha(rangi2,0.2) )
