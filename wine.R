wine <- scan("wine.dat")
wine <- as.ts(wine)

yr <- seq(as.Date("1980/1/1"),by = "months",length=length(wine))
plot(yr,wine, type="l",pch=20,main = "Australian Red Wine" , ylab="feet" )
acf=acf(wine)

# transformation
wine.t <- log(wine)
plot(yr,wine.t, type="l",pch=20,main = "Strikes in the U.S.A., 1951 -1980" , ylab="feet" )
acf=acf(wine)