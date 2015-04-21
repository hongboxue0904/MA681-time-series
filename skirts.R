skirts<- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)

skirtsTS<-ts(skirts,start=c(1866))

plot.ts(skirtsTS, main="Skirts")

skirtsTSforc <- HoltWinters(skirtsTS, gamma=FALSE)
skirtsTSforc

skirtsTSforc$SSE

plot(skirtsTSforc)

skirtsTSforc2<-forecast.HoltWinters(skirtsTSforc, h=19)
plot.forecast(skirtsTSforc2)


acf(skirtsTSforc2$residuals, lag.max=20)
Box.test(skirtsTSforc2$residuals, lag=20, type="Ljung-Box")

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


plot.ts(skirtsTSforc2$residuals) # make a time plot
plotForecastErrors(skirtsTSforc2$residuals) # make a histogram

##################################
# ARIMA
##################################


plot.ts(skirtsTS)

skirtsTSd1<- diff(skirtsTS, differences=1)
plot.ts(skirtsTSd1)

skirtsTSd2<- diff(skirtsTS, differences=2)
plot.ts(skirtsTSd2)


##### test for stationarity?

