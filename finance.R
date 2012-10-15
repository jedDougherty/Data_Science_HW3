#Get data from Yahoo!Finance
require(quantmod)
require(ggplot2)
require(lubridate)
#getQuote(Symbols, src = "yahoo", what = standardQuote(),...)
#standardQuote(src="yahoo")
#yahooQF(names)

getSymbols("XOM", src = "yahoo", from = '2002-01-01')

# Add multi-coloring and change background to white 
candleChart(XOM,multi.col=TRUE,theme="white") 
#?chartSeries
barChart(XOM)

time <- ymd(as.character(index(XOM)))
open <- as.numeric(Op(XOM))
high <- as.numeric(Hi(XOM))
low <- as.numeric(Lo(XOM))
close <- as.numeric(Cl(XOM))
volume <- as.numeric(Vo(XOM))   #Extract and Transform OHLC Time-Series Columns

#time <- index(close)  #Generic functions for extracting the index of an object and replacing it.
#value <- as.vector(close)
profit <- (close-open)/open

my_data <- data.frame(time, close, volume)
save(my_data, file = "my_data.rda")

#plot the time series of XOM
PF <- vector("numeric", length(profit))
PF[profit > 0] <- 1
my_data <- cbind(my_data, PF)
p <- ggplot(my_data, alpha = 0.4, aes(my_data$time, my_data$close, colour = factor(my_data$PF)))
p + geom_line()

#plot log return
log.return <- log((my_data[-1, ]$close)/(my_data[-length(my_data$close), ]$close))
class(log.return)

log.data <- data.frame(log.return, log.time = my_data$time[-1])
p <- ggplot(log.data, aes(y = log.data$log.return, x = log.data$log.time))
p + geom_line() + geom_abline(intercept = 0, slope = 0,size=1,colour='gray')  #

#######   Exp decay   #######
decay <- 0.97

volatility.new <- function(r, v, s){    # to calculate the new volatility
    v <- s * v + (1 - s) * (r ^ 2)
    return(v)
}

#class(log.return)
#v <- volatitlity(r, v, s)
 
vol.old <- vector(mode = "numeric", length(log.return))

#calculate the old volatility part
log.return.old <- as.matrix(log.return[1 : 365])

numerator <- log.return.old[1] ^ 2
denominator <- 1
vol.old[1] <- numerator / denominator
for(i in 2 : length(log.return.old)){
    numerator <- numerator * decay + log.return.old[i] ^ 2
    denominator <- denominator + (decay ^ (i - 1))
    #print(denominator)
    vol.old[i] <- numerator / denominator
    #print(vol.old[i])
}

#calculate the new volatility part
for (i in 366 : length(log.return)){
    vol.old[i] <- volatility.new(log.return[i], vol.old[i - 1], decay)
}

#plot the volatility
log.pred <- data.frame(vol.old, log.time = my_data$time[-1])
p3 <- ggplot(log.pred, aes(y = log.pred$vol.old, x = log.pred$log.time))
p3 + geom_line() + geom_abline(intercept = 0, slope = 0,size=1,colour='gray')


##normalized the log.return with volatility
log.return.new <- log.return / vol.old

log.new <- data.frame(log.return.new, log.time = my_data$time[-1])
p4 <- ggplot(log.new, aes(y = log.new$log.return.new, x = log.new$log.time))
p4 + geom_line() + geom_abline(intercept = 0, slope = 0,size=1,colour='gray')

pred <- as.ts(log.return.new, start = my_data$time[2])
acf(pred)
pacf(pred)

#############

# load useful package and use it's plot function for time series data, only works on univariate time series for now
require(useful)
plot(log.return)
# plot the acf and pacf with it
#plot.ts(log.return, acf=TRUE)

#logDiff <- diff(pred)  #difference the logged data
#plot(logDiff)
#acf(logDiff)
#pacf(logDiff)

#Normal distribution test
shapiro.test(pred)

par(mfrow = c(2,1))
hist(pred, prob = TRUE, 12)
lines(density(pred))
qqnorm(pred)
qqline(pred)
par(mfrow = c(1,1))

#lag.plot(logDiff, 9)

#plot(ddd <- stl(log(close),"per"))
fit.ts <- function(ts, ar=2, int=0:1, ma=0:2)
{
    # build every possible combination of ar, int and ma
    theGrid <- expand.grid(ar, int, ma)
    
    # create an empty list to hold the results
    results <- vector(mode="list", length=nrow(theGrid))
    
    ## loop through the possible combinations and fit the model with those orders
    for(i in 1:nrow(theGrid))
    {
        results[[i]] <- arima(x=ts, order=unlist(theGrid[i, ]))
    }
    
    return(results)
}

#This works out to saying that we scale the impact of the past returns depending on how far back in the past they are, and for each day they get multiplied by some number less than 1 (called the decay). 

require(forecast)
#my.model <- auto.arima(pred)
#summary(my.model)


my.pred <- vector(mode = "numeric" , length(pred)) ## 
my.pred <- pred
for(i in (length(my.pred)-50) : (length(my.pred)-1)){
    theModels <- fit.ts(my.pred[1:i])
    browser()
    theAICs <- laply(theModels, AIC)
    my.model <- theModels[[which.min(theAICs)]]
    my.pred[i + 1] <- predict(my.model)$pred[1] #This is how I want to use the arima function to solve the problem, but it doesn't fit the data well 
}