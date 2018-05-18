
trafficdata=read.csv(file.choose())
trafficdata1=trafficdata$Vehicles

firstHour<- 24*(as.Date("2015-11-01 00:00:00")-as.Date("2015-1-1 00:00:00"))
library(fpp)
library(forecast)
library(dplyr)
tt<-ts(trafficdata$Vehicles,start=c(2015,firstHour),end=c(2017,12),frequency=24*365)
ttarima=auto.arima(tt)
summary(ttarima)
#ARIMA(3,1,2) with drift
plot(ttarima$fitted,col="blue")
lines(tt,col="red")
plot(forecast(ttarima,h=1500))
ttfore=forecast(ttarima,h=1500)
plot(ttfore)


ttneural=nnetar(tt,size = 10)
nnetfor=forecast(ttneural)
plot(nnetfor)