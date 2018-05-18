elec
plot(elec)
ma1 <- ma(elec,order=60,centre = TRUE)
plot(ma1)
forecast(ma1)

