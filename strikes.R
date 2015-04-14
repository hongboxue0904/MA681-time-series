strikes <- scan("strikes.dat")
strikes <- as.ts(strikes)
# use class and structure to tell them apart
yr <- seq(as.Date("1951/1/1"),by = "years",length=length(strikes))
plot(yr,strikes, type="l",pch=20,main = "Strikes in the U.S.A., 1951 -1980" , ylab="feet" )
acf=acf(strikes)

d <- decompose(strikes)
s <- sfilter(strikes)
