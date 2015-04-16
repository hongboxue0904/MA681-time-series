souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirts <- ts(souvenir, frequency=12, start=c(1987,1))
plot.ts(souvenirts)
# log transform the series
logsouvenirts <- log(souvenirts)
plot.ts(logsouvenirts)
