souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirts <- ts(souvenir, frequency=12, start=c(1987,1))

plot.ts(souvenirts, main="Bruce and Fiona Smyth Surf and Souvenirs",
        sub ="Monthly Sales January 1987-December 1993", ylab = "$ AUS",
         xlab = " ")

###################################################

# log transform the series
logsouvenirts <- log(souvenirts)

plot.ts(logsouvenirts,
        main="Bruce and Fiona Smyth Surf and Souvenirs",
        sub ="Monthly Sales January 1987-December 1993",
        ylab = "log($ AUS)",
        xlab = " " )

#################################################

souvenirtsforc<-HoltWinters(logsouvenirts)
souvenirtsforc

souvenirtsforc$SSE

plot(souvenirtsforc)

souvenirtsforc2<-forecast.HoltWinters(souvenirtsforc, h=48)
plot.forecast(souvenirtsforc2)


acf(souvenirtsforc2$residuals, lag.max=20)
Box.test(souvenirtsforc2$residuals, lag=20, type="Ljung-Box")

plot.ts(souvenirtsforc2$residuals) # make a time plot
plotForecastErrors(souvenirtsforc2$residuals) # make a histogram


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





