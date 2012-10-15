#==========3a. Get Exxon Mobil stock data from Yahoo! Finance.==========
URL <- "http://ichart.finance.yahoo.com/table.csv?s=XOM"
exxon <- read.csv(URL)
exxon$Date <- as.Date(exxon$Date, "%Y-%m-%d") #Transform Date into date datatype.
head(exxon)
exxon.date.range <- seq.Date(from=min(exxon$Date), #Check number of years of data.
                             to=max(exxon$Date),
                             by="years")
View(exxon.date.range) #43 years.
exxon <- exxon[order(exxon$Date),] #Sort by date, ascending.

#==========3b. Create times series of daily returns of stock price.==========
require(zoo)
xom.ts.daily <- zoo(exxon$Close, exxon$Date) #Create TS object with Close.
head(xom.ts.daily) #Check data.
head(cbind(xom.ts.daily, lag(xom.ts.daily, k=-1))) #Sanity check lag.
xom.ts.return <- log(xom.ts.daily/lag(xom.ts.daily, k=-1)) #Calculate daily returns.
xom.train <- window(xom.ts.return, start='1970-01-02', end='2002-01-02') #Create in-sample.
xom.test <- window(xom.ts.return, start='2002-01-03', end='2012-10-12') #Create out-of-sample.

#==========3c. Create times series of daily volume of stock price.==========
xom.ts.vol <- zoo(exxon$Volume, exxon$Date) #Create TS object with Volume.
head(xom.ts.vol) #Check data.
head(cbind(xom.ts.vol, lag(xom.ts.vol, k=-1))) #Sanity check lag.
xom.ts.volume <- log(xom.ts.vol/lag(xom.ts.vol, k=-1)) #Calculate daily returns.
xom.train.vol <- window(xom.ts.volume, start='1970-01-02', end='2002-01-02') #Create in-sample.
xom.test.vol <- window(xom.ts.volume, start='2002-01-03', end='2012-10-12') #Create out-of-sample.

#==========3d. Linear regression using past 2 returns to predict next return.==========
#Build exponential decay.
decay <- 0.97

#Create a function to calculate the new volatility.
volatility.new <- function(r, v, s) {    
    v <- s * v + (1 - s) * (r ^ 2)
    return(v)
}

#Calculate old volatility.
xom.ts.return_old <- xom.ts.return[1:365] #[?] Why 365?
vol.old <- vector(mode = "numeric", length(xom.ts.return))
numerator <- xom.ts.return_old[1]^2
denominator <- 1
vol.old[1] <- numerator/denominator
for(i in 2:length(xom.ts.return_old)) {
    numerator <- numerator * decay + xom.ts.return_old[i]^2
    denominator <- denominator + (decay ^ (i-1))
    vol.old[i] <- numerator/denominator
} #[?] Get error of "replacement has length zero"

#Calculate the new volatility.
for (i in 366:length(xom.ts.return)) {
    vol.old[i] <- volatility.new(xom.ts.return[i], vol.old[i - 1], decay)
}

#plot the volatility
plot(vol.old)
#[?]log.pred <- data.frame(vol.old, log.time = my_data$time[-1])
#[?]p3 <- ggplot(log.pred, aes(y = log.pred$vol.old, x = log.pred$log.time))
#[?]p3 + geom_line() + geom_abline(intercept = 0, slope = 0,size=1,colour='gray')

xom.ts.return_new <- xom.ts.return / vol.old
ts.plot(xom.ts.return_new)

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

#Build ARIMA model for returns.
require(forecast)
xom.train.arima <- arima(coredata(xom.train), order=c(2,0,1)) #Add 2 lags
summary(xom.train.arima) #Coefficients for ARIMA model.
confint(xom.train.arima) #Confidence intervals for ARIMA model.
tsdiag(xom.train.arima) #Diagnostic tests for ARIMA model.
fcast <- forecast(xom.train.arima, 2716) #Forecast test set dates using ARIMA model.

#Extract data into DF to prep GGPLOT 
#Adapted from R-Bloggers: 
#http://www.r-bloggers.com/plotting-forecast-objects-in-ggplot-part-1-extracting-the-data-2/

en<-'2012-10-12' #enter the max date used in the forecast    
#Extract Source and Training Data
ds<-as.data.frame(window(xom.ts.return,end=en))
names(ds)<-'observed'
ds$date<-as.Date(time(window(xom.ts.return,end=en)))
    
#Extract the Fitted Values (need to figure out how to grab confidence intervals)
dfit<-as.data.frame(fcast$fitted)
dfit$date<-as.Date(time(fcast$fitted))
names(dfit)[1]<-'fitted'
    
ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
    
#Exract the Forecast values and confidence intervals
dfcastn<-as.data.frame(fcast)
dfcastn$date<-exxon$Date[8083:10798]
names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
    
pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot

#Graph extracted data into GGPLOT.
library(ggplot2)
library(scales)

ggplot(data=pd,aes(x=date,y=observed)) +
    geom_line(col='red') +
    geom_line(aes(y=fitted),col='blue') +
    geom_line(aes(y=forecast)) +
    geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=.25) +
    scale_x_date(name='',breaks='5 year',
                 minor_breaks='1 month',
                 labels=date_format("%y"),
                 expand=c(0,0)) +
    scale_y_continuous(name='Units of Y') +
    opts(axis.text.x=theme_text(size=10),
         title='Arima Fit to Simulated Data\n (black=forecast, blue=fitted, red=data, shadow=95% conf. interval)')
