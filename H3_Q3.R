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

acf(xom.ts.return_new, na.action=na.pass) #Lags 2-7 are significant!
pacf(xom.ts.return_new, na.action=na.pass) #Lags 2-7 are significant!



xom.lm.train <- xom.ts.return_new[1 : 5803]  #using the previous half data as the training set
xom.lm.test <- xom.ts.return_new[5804 : 10803]  ##using the other half data as the testing set


##Build linear regression on non-normalized log returns to predict price.
require(dyn)
train.function <- function(train.set){
    train.result <- dyn$lm(train.set ~ lag(train.set, -1) + lag(train.set, -2))
}
xom.prior2 <- train.function(xom.lm.train)  #train the initial linear regression model with xom.lm.train
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

## use the testing data to do the prediction and draw the (cumulative) PnL of the model
test.set <- data.frame(1, lag(xom.lm.test, -1)[-1], lag(xom.lm.test, -2))  #data set for predict
test.set <- as.matrix(test.set)

test.pnl <- vector(mode = "numeric", dim(test.set)[1])
for(i in 1 : dim(test.set)[1]){
    
    if((i + 3) <= dim(test.set)[1]){
        ##do the prediction 
        new.test.data <- test.set[i, ]
        test.predict <- new.test.data %*% coeff.xom  #use the coeff to calculate the predicted data of the third day
        test.pnl[i] <- test.predict * xom.lm.test[i + 2]  #calculate the pnl for the i data of test set
        # when we calculate the test.set, we used 2 lag. So the xom.lm.test should be the date after two days
        
        #browser()
        
        ## update the linear model with the i data of test set
        xom.lm.train <- zoo(c(xom.lm.train, xom.lm.test[i + 2 + 1]))  #put the new data into our training set
        xom.prior.updated <- train.function(xom.lm.train)  #train the initial linear regression model with the new xom.lm.train
        
        coeff.xom <- coef(xom.prior.updated)        
    }
    
}
#predict.test.set <- predict(xom.prior2, newdata = test.set)

save(test.pnl, file = "test_pnl.rda")

test.pnl.modified <- test.pnl[- c(length(test.pnl)-2 ,length(test.pnl)-1, length(test.pnl))]  
#Since the last three data didn't be calculated, they are all zero. So we get rid of them.

c.pnl <- vector(mode = "numeric", length = length(test.pnl.modified))  #This the cumulated PnL
for(i in 1:length(test.pnl.modified)){
    c.pnl[i] <- sum(test.pnl.modified[1 : i])
}

plot(c.pnl)
