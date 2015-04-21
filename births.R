births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthsts <- ts(births, frequency=12, start=c(1946,1))
plot.ts(birthsts, main ="Number of births per month in New York City",
        ylab="Births")
###########################3

birthscomp <- decompose(birthsts)

plot(birthscomp)

# seasonal adjustment

birthsSeasonallyadj<-birthsts-birthscomp$seasonal

plot(birthsSeasonallyadj)


x <- 1:200
y <- 3*x +5
acf(y)

