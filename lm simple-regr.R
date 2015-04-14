x<- 1:20
a <- 4
y <- seq(4,by=2,length=20) + a*rnorm(20)
out <- lm(y~x-1)
plot(x,y)
summary(out)
plot(resid(out))
c <- coefficients(out)
acf(resid(out))
resid <- resid(out)
simple <- as.numeric(resid)

