install.packages("TSA")
install.packages("tseries")

library("TSA")
library("tseries")

beer<-read.csv("monthly_beer_production_australia.csv", header =T)


beerts <- ts(beer$beer_prod, start=c(1956,1), frequency=12)
plot(beerts, ylab="Megalitres Produced", type="l")
points(y=beerts, x=time(beerts), pch=as.vector(season(beerts)), col=4, cex=0.8)



beerts2 <- ts(beerts[-c(429:476) ], start=c(1956,1), frequency=12) #removing 10% of observations
plot(beerts2, ylab="Megalitres Produced", type="l")
points(y=beerts2, x=time(beerts2), pch=as.vector(season(beerts2)), col=4, cex=0.5)

qqnorm(beerts2); qqline(beerts2); #testing normality before transformation

BC <-BoxCox.ar(beerts2, lambda=seq(-2,2,0.1) ) 
BC$mle
BC$ci


BCbeer <- ((beerts2^-0.3)-1)/-0.3 #Power-transformation using lambda = -0.3
plot(BCbeer)

qqnorm(BCbeer); qqline(BCbeer);  #testing normality after transformation
shapiro.test(BCbeer)

beerdecom2 <- decompose(BCbeer) #decomposing data to check for overall/seasonal trend
plot(beerdecom2)
diff(range(BCbeer))
diff(range(beerdecom2$trend,na.rm=T))
diff(range(beerdecom2$seasonal,na.rm=T))
diff(range(beerdecom2$random,na.rm=T))


acf(BCbeer)
adf.test(BCbeer, k=12)



beerdiff <- diff(BCbeer) #differecing the data
plot(beerdiff)

ACF1<-acf(beerdiff)
ACF1$lag=ACF1$lag*12
plot(ACF1)

beerdecom3 <- decompose(beerdiff)
plot(beerdecom3)
diff(range(beerdiff))
diff(range(beerdecom3$trend,na.rm=T))
diff(range(beerdecom3$seasonal,na.rm=T))
diff(range(beerdecom3$random,na.rm=T))


beerdiffs <- diff(beerdiff, lag= 12) #seasonal differencing the data


plot(beerdiffs)

ACF2<-acf(beerdiffs)
ACF2$lag=ACF2$lag*12
plot(ACF2)

PACF2<-pacf(beerdiffs)
PACF2$lag=PACF2$lag*12
plot(PACF2)



beerdecom4 <- decompose(beerdiffs)
plot(beerdecom4)
diff(range(beerdiffs))
diff(range(beerdecom4$trend,na.rm=T))
diff(range(beerdecom4$seasonal,na.rm=T))
diff(range(beerdecom4$random,na.rm=T))

adf.test(beerdiffs, k=12)

acf(beerdiffs)
pacf(beerdiffs)
eacf(beerdiffs)

beersubs <- armasubsets(beerdiffs, nar=12, nma=12)
plot(beersubs)


beermod <- arima(beerts2, order=c(2,1,3),
                 seasonal=list(order=c(0,1,2), period=12) ) #Model 1
beermod


beermod <- arima(beerts2, order=c(2,1,3),
                 seasonal=list(order=c(1,1,2), period=12) ) #Model 2
beermod

tsdiag(beermod)



resids <- rstandard(beermod); fit <- fitted(beermod)
runs(resids)
plot(resids)
acf(resids)
plot(fit, resids)

qqnorm(resids); qqline(resids);
shapiro.test(resids)
hist(resids)

predic<-predict(beermod, n.ahead=48)
predic$pred #predicted values from model 

plot(beermod, n.ahead=48, col='blue') #forecast 48 months in the future
lines(beerts, ylab="Megalitres Produced", type="l") #adding back in the original 10% of data removed at the start


  
