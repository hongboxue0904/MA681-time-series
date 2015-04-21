rain<- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries<-ts(rain,start=c(1813))
