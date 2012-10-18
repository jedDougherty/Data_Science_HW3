#==========3a. Get Exxon Mobil stock data from Yahoo! Finance.==========
URL <- "http://ichart.finance.yahoo.com/table.csv?s=XOM"
exxon <- read.csv(URL)
exxon$Date <- as.Date(exxon$Date, "%Y-%m-%d") #Transform Date into date datatype.
head(exxon)
exxon <- exxon[order(exxon$Date),] #Sort by date, ascending.

#==========3b. Create times series of daily returns of stock price.==========
require(zoo)
xom.ts.daily <- zoo(exxon$Close, exxon$Date) #Create TS object with Close.
head(xom.ts.daily) #Check data.
head(cbind(xom.ts.daily, lag(xom.ts.daily, k=-1))) #Sanity check lag.
xom.ts.return <- log(xom.ts.daily/lag(xom.ts.daily, k=-1)) #Calculate daily returns.
ts.plot(xom.ts.return)

#==========3c. Create times series of daily volume of stock price.==========
xom.ts.vol <- zoo(exxon$Volume, exxon$Date) #Create TS object with Volume.
head(xom.ts.vol) #Check data.
head(cbind(xom.ts.vol, lag(xom.ts.vol, k=-1))) #Sanity check lag.
xom.ts.volume <- log(xom.ts.vol/lag(xom.ts.vol, k=-1)) #Calculate daily returns.
plot(xom.ts.vol)

#==========3d. Linear regression using past 2 returns to predict next return.==========
#Build exponential decay.
decay <- 0.97

#Create a function to calculate the new volatility.
volatility.new <- function(r, v, s) {    
    v <- s * v + (1 - s) * (r ^ 2)
    return(v)
}

#Calculate old volatility.
xom.old <- as.matrix(xom.ts.return[1:365]) #Using 1-year lookback window
vol.old <- vector(mode = "numeric", length(xom.old))
numerator <- xom.old[1]^2
denominator <- 1
vol.old[1] <- numerator/denominator

for(i in 2:length(xom.old)) {
    numerator <- numerator * decay + xom.old[i] ^ 2
    denominator <- denominator + (decay ^ (i - 1))
    vol.old[i] <- numerator / denominator
}

#Calculate the new volatility.
for (i in 366:length(xom.ts.return)) {
    vol.old[i] <- volatility.new(xom.ts.return[i], vol.old[i - 1], decay)
}

#plot the volatility
plot(vol.old)
#[?]log.pred <- data.frame(vol.old, log.time = my_data$time[-1])
#[?]p3 <- ggplot(log.pred, aes(y = log.pred$vol.old, x = log.pred$log.time))
#[?]p3 + geom_line() + geom_abline(intercept = 0, slope = 0,size=1,colour='gray')

#Log returns normalized for volatility.
xom.ts.return_new <- xom.ts.return / sqrt(vol.old)
xom.ts.return_new <- zoo(xom.ts.return_new, exxon$Date)
plot(xom.ts.return_new)

acf(xom.ts.return_new, na.action=na.pass) #Lags 1&2 are significant!
pacf(xom.ts.return_new, na.action=na.pass) #Lags 1&2 are significant!

##Build linear regression on non-normalized log returns to predict price.
# require(dyn)
# xom.prior2 <- dyn$lm(xom.train ~ lag(xom.train, -1) + lag(xom.train, -2))
# summary(xom.prior2)
# plot(xom.prior2)
# coeff.xom <- coef(xom.prior2)
# coeff.xom.prior2 <- data.frame(Coefficient=names(coeff.xom), Value=coeff.xom)
# 
# #Draw scatterplot of model's coefficients.
# require(ggplot2)
# postscript(file="XOM_Coeffs_Scttr.eps", #Save graph to EPS file.
#            onefile=FALSE, 
#            width=6,
#            height=6,
#            horizontal=FALSE)
# ggplot(coeff.xom.prior2, aes(x=Value, y=Coefficient)) +
#     geom_vline(xintercept=0, color="blue", linetype=2) +
#     geom_point(shape=1)
# dev.off()

#Build ARIMA model for returns.
require(forecast)
xom.arima.fit <- arima(as.ts(xom.ts.return_new), order=c(2,0,1)) #Add 2 lags
summary(xom.arima.fit) #Coefficients for ARIMA model.
confint(xom.arima.fit) #Confidence intervals for ARIMA model.
tsdiag(xom.arima.fit) #Diagnostic tests for ARIMA model.
xom.forecast <- forecast(xom.arima.fit) #Predict returns using ARIMA model.
plot(xom.forecast)

#Extract data into DF to prep GGPLOT 
#Adapted from R-Bloggers: 
#http://www.r-bloggers.com/plotting-forecast-objects-in-ggplot-part-1-extracting-the-data-2/

##Extract Source and Training Data.
ds <- as.data.frame(xom.ts.return)
names(ds)<-'observed'
ds$date <- as.Date(time(window(xom.ts.return)))
    
#Extract the Fitted Values.
dfit <- as.data.frame(xom.forecast$fitted)
names(dfit)[1] <- 'predicted' #[?]Why all the NAs? What date do these forecast refer to?
# dfit <- dfit[1:10801,]
# dfit$date <- as.Date(time(window(xom.ts.return)))

ds <- merge(ds,dfit,all.x=T) #Merge fitted values with source and training data

#Graph extracted data into GGPLOT.
library(ggplot2)
library(scales)

ggplot(data=ds,aes(x=date,y=observed)) +
    geom_line(col='red') +
    geom_line(aes(y=predicted),col='blue') +
    scale_y_continuous(name='Units of Y') +
    opts(axis.text.x=theme_text(size=10),
         title='Arima Fit to Exxon\n (Blue=Predicted, Red=Observed)')

##Convert return back to price.
ds$obs.price <- exp(ds$observed)
ds$p0 <- exxon$Close[1:10801]
ds$p1 <- ds$obs.price * ds$p0