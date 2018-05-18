# load library
library(tseries, quietly = T)
library(forecast, quietly = T)

start <- as.Date("2012-01-01")
end <- Sys.Date()

getSymbols("AAPL", src = "yahoo", from = start, to = end)

# What is AAPL?
class(AAPL)

# Let's see the first few rows
head(AAPL)

chartSeries(AAPL, subset='last 6 months')
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)

stock <- AAPL

stock.ts = ts(rev(stock$AAPL.Close),start=c(2010, 1),frequency=12)
train <- window(stock.ts, end=c(2015,12))
test <- window(stock.ts, start=c(2016,1), end=c(2018,2))

# Plot prediction
tl = seq(2000,2013,length=length(train))
tl2 = tl^5
polystock = lm(train ~ tl + tl2)
stlstock = stl(train,s.window = "per", t.window = 100)
NN.fit <- nnetar(train)
arima.fit <- arima(train, order=c(15,3,3))
arima2.fit <- arima(train, order=c(1,0,0), list(order=c(2,1,0), period=12))
tslm.fit <- tslm(train ~ trend + season, lambda=0)
HWstock_ng = HoltWinters(train,gamma=FALSE)
HWstock = HoltWinters(train)



# Plot all in a graph
#plot(as.Date(rownames(AAPL), "%Y-%m-%d"), AAPL$Adj.Close, xlab= "Dates", ylab= "Adjusted closing price", type='l', col='red', main="Adjusted closing price of Apple")
plot(train)
plot(forecast(HWstock), main="Forecast of Holt-Winter Filter Model")
plot(forecast(HWstock_ng), main="Forecast of Exponential Smoothing Model")
plot(forecast(tslm.fit), main="Advanced Linear Model")
plot(forecast(NN.fit), main="Forecast of Neural Networks Model")
plot(forecast(arima2.fit), main="Forecast of ARIMA Sesonal Model")
plot(forecast(arima.fit), main="Forecast of ARIMA Model")
plot(forecast(stlstock), main="Sesonal Fit Model")
plot(forecast(polystock$fit), main="Polynominal Fit Model")