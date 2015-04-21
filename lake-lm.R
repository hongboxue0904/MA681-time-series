# library(zoo)
lake <- scan("lake.dat")
lake <- as.ts(lake)
yr <- seq(as.Date("1875/1/1"),by = "months",length=length(lake))
plot(yr,lake, type="l",pch=20,main = "Lake Huron Water Level" , ylab="feet" )
acf=acf(lake)


lake1 <- lake[-1]

m <- lake[1:(length(lake1))]

out <- lm(lake1 ~ m)
print(out)
resid <- resid(out)
plot(resid)
acf(resid)
# analysis of second order AR parameter starts here
resid1 <- resid[-1]
m1 <- resid[1:(length(resid1))]
out1 <- lm(resid1 ~ m1)

print(out1)
resid2 <-resid(out1)
plot(resid2)
hist(resid1)
hist(resid2)
acf(resid2)
qqnorm(resid,main = "resid")
qqnorm(resid2,main = "resid2")

lake1<-lake1[1:length(resid1)]


level <- (.8364*lake1)+ (.1857*resid1)+(-.0174)
yr1 <- yr[1:length(resid1)]
yr2 <- yr[-1]
yr2 <- yr2[1:length(resid1)]
plot(yr,lake, type="l",pch=20,main = "Lake Huron Water Level" , ylab="feet" )
lines(yr2,level, col="red")
modelresid = resid1 - level
plot(yr2,modelresid,type="l",col="blue")
plot(resid2,type="l", col="green")
lines(1:96,modelresid,type="l",col="red")

arout <- ar(lake)
print(arout)

level2 <- (1.0534*lake1)+ (-.2668*resid1) 

plot(yr,lake, type="l",pch=20,main = "Lake Huron Water Level" , ylab="feet" )
lines(yr2,level2, col="red")

meanlake <-mean(lake)
meanlevel2 <- mean(level2)

print("meanlake", meanlake,"meanlevel2)",mean

