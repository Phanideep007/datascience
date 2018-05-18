install.packages("readxl")
library(readxl)
install.packages("zoo") ##convert data into time series
library(zoo)
business_data <- read_excel("D:/analytics/a.xlsx")
business_data
head(business_data, 30)
business_data_ts <- zoo(business_data$`Count of TASK_ID`,
                        seq(from = as.POSIXct("2017-02-05 00:00"),
                           to = as.POSIXct("2017-02-28 23:00"), by = "hour"))
business_data_ts
plot(business_data_ts)
library(fpp)


##Forecasting using Auto Arima

fit <- auto.arima(business_data_ts, stepwise = FALSE,
                  approximation = FALSE, max.order = 10)
forecast(fit)

#Forecasting manually - Box jenkins method
#finding diff(d) values
#business_data_stationary <- diff(business_data_ts,1)   #based on Dickey_fuller
#business_data_stationary2 <- diff(business_data_ts,2)  #based on Dickey_fuller
business_data_stationary3 <- diff(business_data_ts,3)
#adf.test(business_data_stationary)                     #based on Dickey_fuller
#adf.test(business_data_stationary2)                    #based on Dickey_fuller
adf.test(business_data_stationary3)
adf.test(business_data_ts)

plot(business_data_stationary3)

par(mfrow = c(1,2))   #to divide the screen in graph
Acf(business_data_stationary3)  #Autocorellation func
Pacf(business_data_stationary3) #PartialAutoCorellatio Func

#(1,3,x)

model1 <- Arima(business_data_ts, order = c(1,1,2))
model2 <- Arima(business_data_ts, order = c(1,3,0))
model3 <- Arima(business_data_ts, order = c(1,3,1))
model4 <- Arima(business_data_ts, order = c(1,2,1))
model5 <- Arima(business_data_ts, order = c(4,0,0))

AIC_COMPARE <- data.frame("model1" = model1$aic,"model2" = model2$aic,"model3" = model3$aic,"model4" = model4$aic,"model5" = model5$aic)
AIC_COMPARE
#since less AIC value hence model4

forecast(model4)
