#3a. Get Exxon Mobil stock data from Yahoo! Finance.
URL <- "http://ichart.finance.yahoo.com/table.csv?s=XOM"
exxon <- read.csv(URL)
exxon$Date <- as.Date(exxon$Date, "%Y-%m-%d") #Transform Date into date datatype.
head(exxon)
exxon.date.range <- seq.Date(from=min(exxon$Date), #Check number of years of data.
                             to=max(exxon$Date),
                             by="years")
View(exxon.date.range) #43 years.
exxon <- exxon[order(exxon$Date),] #Sort by date, ascending.
xom.date <- exxon$Date #Separate Date to prep TS file.
xom.close <- exxon$Close #Separate Close to prep TS file.
xom.vol <- exxon$Volume #Separate Volume to prep TS file.

#3b. Create times series of daily returns of stock price.
require(zoo)
xom.ts.daily <- zoo(xom.close, xom.date) #Create TS object with Close.
head(xom.ts.daily) #Check data.
head(cbind(xom.ts.daily, lag(xom.ts.daily, k=-1))) #Sanity check lag.
xom.ts.return <- log(xom.ts.daily/lag(xom.ts.daily, k=-1)) #Calculate daily returns.
xom.train <- window(xom.ts.return, start='1970-01-02', end='2000-01-02') #Create in-sample.
xom.test <- window(xom.ts.return, start='2000-01-03', end='2012-10-12') #Create out-of-sample.

#3c. Create times series of daily volume of stock price.
xom.ts.vol <- zoo(xom.vol, xom.date) #Create TS object with Volume.
head(xom.ts.vol) #Check data.
head(cbind(xom.ts.vol, lag(xom.ts.vol, k=-1))) #Sanity check lag.
xom.ts.volume <- log(xom.ts.vol/lag(xom.ts.vol, k=-1)) #Calculate daily returns.
xom.train.vol <- window(xom.ts.volume, start='1970-01-02', end='2000-01-02') #Create in-sample.
xom.test.vol <- window(xom.ts.volume, start='2000-01-03', end='2012-10-12') #Create out-of-sample.

#3d. Linear regression using past 2 returns to predict next return.
acf(coredata(xom.ts.return))
pacf(coredata(xom.ts.return))
ccf(coredata(xom.ts.return), lag(coredata(xom.ts.return), k=-1))

#Build linear regression to predict price.
require(dyn)
xom.prior2 <- dyn$lm(xom.train ~ lag(xom.train, -1) + lag(xom.train, -2))
summary(xom.prior2)
plot(xom.prior2)
coeff.xom <- coef(xom.prior2)
coeff.xom.prior2 <- data.frame(Coefficient=names(coeff.xom), Value=coeff.xom)

#Draw scatterplot of model's coefficients.
require(ggplot2)
postscript(file="XOM_Coeffs_Scttr.eps", #Save graph to EPS file.
           onefile=FALSE, 
           width=6,
           height=6,
           horizontal=FALSE)
ggplot(coeff.xom.prior2, aes(x=Value, y=Coefficient)) +
    geom_vline(xintercept=0, color="blue", linetype=2) +
    geom_point(shape=1)
dev.off()

#Build causal model for returns.
require(forecast)
auto.arima(xom.train)
xom.train.arima <- arima(coredata(xom.train), order=c(2,0,1))
summary(xom.train.arima)
confint(xom.train.arima)
tsdiag(xom.train.arima)
forecast(xom.train.arima)
plot(fcast)

pred10 <- predict(xom.train.arima, newdata=xom.test[1:10], n.ahead=10)
test10 <- subset(xom.test[1:10])
ts.plot(pred10$pred)
ts.plot(xom.test[1:10])

require(quantmod)
require(PerformanceAnalytics)
charts.PerformanceSummary(xom.train)
chart.CumReturns(xom.arima)

#Build causal model for volumes.
zoo()

