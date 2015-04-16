births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthsts <- ts(births, frequency=12, start=c(1946,1))
plot.ts(birthsts)