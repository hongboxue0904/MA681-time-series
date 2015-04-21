library(TTR)
library(forecast)
library(xts)
library(zoo)

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingsts <- ts(kings)
plot(kingsts, main="Age of Death of Successive Kings of England", 
        sub= "William the Conqueror to George VI", xlab="Monarchs",
     ylab="Age at Death")
######################################

kingstsSMA2 <- SMA(kingsts, n=2)
plot(kingstsSMA2, main="Monarchs:Age of Death - SMA2", 
     xlab="Monarchs",
     ylab="Age at Death")

kingstsSMA3 <- SMA(kingsts, n=3)
plot(kingstsSMA2, main="Monarchs:Age of Death - SMA3", 
     xlab="Monarchs",
     ylab="Age at Death")

kingstsSMA8 <- SMA(kingsts, n=8)
plot(kingstsSMA8, main="Monarchs:Age of Death - SMA8", 
     xlab="Monarchs",
     ylab="Age at Death")


# kingscomp <- decompose(kingsts)


###################################################
# ARIMA
###################################################

plot(kingsts, main="Age of Death of Successive Kings of England", 
     sub= "William the Conqueror to George VI", xlab="Monarchs",
     ylab="Age at Death")


kingtsd1<- diff(kingsts, differences=1)
plot.ts(kingtsd1)

kingstsArima<-arima(kingsts, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
kingstsArima

kingstsforc<-forecast.Arima(kingstsArima, h=5)
kingstsforc

plot.forecast(kingstsforc)

acf(kingstsforc$residuals, lag.max=20)
Box.test(kingstsforc$residuals, lag=20, type="Ljung-Box")


plotForecastErrors<- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize<-IQR(forecasterrors)/4
  mysd<-sd(forecasterrors)
  mymin<- min(forecasterrors)-mysd*5
  mymax<- max(forecasterrors)+mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm<-rnorm(10000, mean=0, sd=mysd)
  mymin2<- min(mynorm)
  mymax2<- max(mynorm)
  if (mymin2<mymin) { mymin<-mymin2 }
  if (mymax2>mymax) { mymax<-mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins<- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist<-hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}







plot.ts(kingstsforc$residuals) # make time plot of forecast errors
plotForecastErrors(kingstsforc$residuals) # make a histogram

