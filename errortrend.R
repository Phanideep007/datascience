hsales ##install fpp package and run hsales
plot(hsales)
fit <- ets(hsales,"ZZZ")
forecast(fit)
fit
