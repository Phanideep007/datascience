library(TTR)
library(devtools)
library(quantmod)
library(magrittr)

start <- as.Date("2012-01-01")
end <- Sys.Date()

getSymbols("AAPL", src = "yahoo", from = start, to = end)

# What is AAPL?
class(AAPL)

# Let's see the first few rows
head(AAPL)

chartSeries(AAPL, subset='last 6 months')
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)


plot(AAPL[, "AAPL.Close"], main = "AAPL")
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")

getSymbols(c("MSFT", "GOOG", "AMZN", "CA"), src = "yahoo", from = start, to = end)

# Create an xts object (xts is loaded with quantmod) that contains closing
# prices for AAPL, MSFT, and GOOG
stocks <- as.xts(data.frame(AAPL = AAPL[, "AAPL.Close"], 
                            MSFT = MSFT[, "MSFT.Close"], 
                            GOOG = GOOG[, "GOOG.Close"], 
                            AMZN = AMZN[, "AMZN.Close"],
                            CA = CA[, "CA.Close"]))
head(stocks)


# Create a plot showing all series as lines; must use as.zoo to use the zoo
# method for plot, which allows for multiple series to be plotted on same
# plot
plot(as.zoo(stocks), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price", col=rainbow(5))
legend("right", c("AAPL", "MSFT", "GOOG", "AMZN", "CA"), lty = 1:3, cex = 0.5)


plot(as.zoo(stocks[, c("AAPL.Close", "MSFT.Close", "GOOG.Close", "AMZN.Close", "CA.Close")]), screens = 1, lty = 1:2, 
     xlab = "Date", ylab = "Price", col=rainbow(5))
par(new = TRUE)
plot(as.zoo(stocks[, "GOOG.Close"]), screens = 1, lty = 3, xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "")
axis(4)
mtext("Price", side = 4, line = 3)
legend("topleft", c("AAPL (left)", "MSFT (left)", "GOOG"), lty = 1:3, cex = 0.5)


#### Calculate the Stock Returns
stock_return = apply(stocks, 1, function(x) {x / stocks[1,]}) %>% 
  t %>% as.xts

head(stock_return)

### Plot

plot(as.zoo(stock_return), screens = 1, lty = 1:3, xlab = "Date", ylab = "Return", col = rainbow(10))
legend("topleft", c("AAPL", "MSFT", "GOOG", "AMZN","CA"), lty = 1:3, cex = 0.5)


stock_change = stocks %>% log %>% diff
head(stock_change)

plot(as.zoo(stock_change), screens = 1, lty = 1:3, xlab = "Date", ylab = "Log Difference", col = rainbow(10))
legend("topleft", c("AAPL", "MSFT", "GOOG", "AMZN","CA"), lty = 1:3, cex = 0.5)


candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2017-01-04/")
addSMA(n = 20)
zoomChart("2018")

AAPL_sma_20 <- SMA(
  Cl(AAPL),  # The closing price of AAPL, obtained by quantmod's Cl() function
  n = 20     # The number of days in the moving average window
)

AAPL_sma_50 <- SMA(
  Cl(AAPL),
  n = 50
)

AAPL_sma_200 <- SMA(
  Cl(AAPL),
  n = 200
)

zoomChart("2017")  # Zoom into the year 2016 in the chart
addTA(AAPL_sma_20, on = 1, col = "red")  # on = 1 plots the SMA with price
addTA(AAPL_sma_50, on = 1, col = "blue")
addTA(AAPL_sma_200, on = 1, col = "green")
