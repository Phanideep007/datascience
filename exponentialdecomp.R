#### Holts Winter #####
elec
plot(elec)
fit <- hw(elec, seasonal = "additive")
plot(fit)
fit
##########Holts as there is no symmetric seasonality###########
hsales
plot(hsales)
stl2 <- stl(hsales,t.window=4,s.window=2)
plot(stl2)
fit2 <- holt(hsales, exponential = TRUE, damped = TRUE)
plot(fit2)
fit2
