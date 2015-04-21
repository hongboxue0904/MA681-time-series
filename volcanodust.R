volcanodust<- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)

volcanodustTS<-ts(volcanodust,start=c(1500))
plot.ts(volcanodustTS, main="volcanic dust")

# plot a correlogram
acf(volcanodustTS, lag.max=20) 
# get the values of the autocorrelations
acf(volcanodustTS, lag.max=20, plot=FALSE) 

pacf(volcanodustTS, lag.max=20)
pacf(volcanodustTS, lag.max=20, plot=FALSE)

library(forecast)
auto.arima(volcanodustTS)

volcanodustTSarima<-arima(volcanodustTS, order=c(2,0,0))
volcanodustTSarima

volcanodustTSforc<-forecast.Arima(volcanodustTSarima, h=31)
volcanodustTSforc

plot.forecast(volcanodustTSforc)

acf(volcanodustTSforc$residuals, lag.max=20)
Box.test(volcanodustTSforc$residuals, lag=20, type="Ljung-Box")

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




plot.ts(volcanodustTSforc$residuals) # make time plot of forecast errors
plotForecastErrors(volcanodustTSforc$residuals) # make a histogram

mean(volcanodustTSforc$residuals)



