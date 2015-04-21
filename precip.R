rain<- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)

raints <-ts(rain,start=c(1813))
plot.ts(raints, main="Rain in London")

# ﬁt a simple exponential smoothing predictive
# model with the “HoltWinters()” function

raintsForc <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
raintsForc
raintsForc$fitted
plot(raintsForc)
raintsForc$SSE

library(forecast)

raintsForc2 <- forecast.HoltWinters(raintsForc, h=8)

raintsForc2

plot.forecast(raintsForc2)

acf(raintsForc2$residuals, lag.max=20)

Box.test(raintsForc2$residuals, lag=20, type="Ljung-Box")

plot.ts(raintsForc2$residuals)
qqnorm(raintsForc2$residuals)
hist(raintsForc2$residuals)


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

plotForecastErrors(raintsForc2$residuals)





