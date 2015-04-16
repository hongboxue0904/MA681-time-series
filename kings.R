kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingsts <- ts(kings)
plot.ts(kingsts)