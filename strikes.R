library(itsmr)
strikes <- scan("strikes.dat")
strikes <- as.ts(strikes)
# use class and structure to tell them apart
yr <- seq(as.Date("1951/1/1"),by = "years",length=length(strikes))
plot(yr,strikes, type="l",pch=20,main = "Strikes in the U.S.A., 1951 -1980" )
acf=acf(strikes)

rr <- smooth.ma(strikes,2)

plot(yr,strikes, type="l",pch=20,main = "Strikes in the U.S.A., 1951 -1980" )
plot(yr,rr, type="l",pch=20,main = "Strikes in the U.S.A., 1951 -1980" )
rr1 <- strikes - rr
plot(yr,rr1, type="l",pch=20,main = "Strikes in the U.S.A., 1951 -1980" )
acf(rr1)
