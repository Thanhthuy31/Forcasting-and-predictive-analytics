#Read the CSV file 
data <- read.csv("hyvee_sales.csv", header = TRUE)
data = data[,-1]
colnames(data)[2] ="Sales"
data$Date = as.Date(data$Date)


#process data 
mydates = seq.Date(from = as.Date("2019-01-02"), 
                   to = as.Date("2021-11-30"), 
                   by = 1)

# Converting to a df (required for the merge)
mydates = data.frame(Date = mydates)

# Padding with 'mydates'
mydata = merge(mydates, data, by = "Date",all.x =T)

# Removing initial days to start on monday
mydata = mydata[6:nrow(mydata),]

# Removing sundays, watch the from as the first one to remove
mydata = mydata[-(seq(from = 7, to = nrow(mydata), by = 7)),]
# Removing saturdays
mydata = mydata[-(seq(from = 6, to = nrow(mydata), by = 6)),]

# Replace NAs and outliers 
library(forecast)
sale_data = tsclean(mydata$Sales)

#plot time series
ts_data = ts(sale_data)
time = mydata$Date

plot(ts_data,type="l",xlab="Time", ylab="Sale in dollar",main="",xaxp = c(0, 520, 2),axes=FALSE);box()
axis(1, at = 260*(0:2) ,labels = c(2019,2020,2021))
axis(2)


#non-stationary test
library(urca)
library(tseries)
adf.test(ts_data)


#autocorrelation
par(mfrow=c(1,2), mar=c(4,4.5,1,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
acf(ts_data,lag.max = 20,main="Sales ACF") 
pacf(ts_data,lag.max = 20,main="Sales PACF")

#decompose
ts_sales = ts(sale_data,frequency=5)
ts_sales
plot(decompose(ts_sales, type="additive"))



#####ARIMA#######
library(timeSeries)
sales <- timeSeries(sale_data,mydata$Date)
sales

#train split test 
n <- length(sale_data); n
train<- 1:round(n*0.7)
test <- (round(n*0.7)+1):n
t <- train

#fit a trend model
library(quantreg)
ln_model <- lm(sales[train]~t)
qt_model_up <- rq(sales[train]~t, tau=0.025)
qt_model_low <- rq(sales[train]~t, tau=0.975)
pred_ln <- cbind(1,test)%*%coef(ln_model)
pred_qt_up <- cbind(1,test)%*%coef(qt_model_up)
pred_qt_low <- cbind(1,test)%*%coef(qt_model_low)

plot(sales$TS.1,type="l",xlab="Time",ylab="sales", main="",xaxp = c(0, 520, 2),axes=FALSE);box()
axis(1, at = 260*(0:2) ,labels = c(2019,2020,2021))
axis(2)
points(train, sales[train], col="grey")
points(test, sales[test])
lines(fitted(ln_model), col=2, lwd=2)
lines(fitted(qt_model_up), col=2, lwd=2)
lines(fitted(qt_model_low), col=2, lwd=2)
lines(test, pred_ln, col=4, lwd=2)
lines(test, pred_qt_up, col=4, lwd=2)
lines(test, pred_qt_low, col=4, lwd=2)
abline(v=end(train), lty=2)



#fit arima model
arima_500 <- arima(sales[train], c(5,0,0))
arima_503 <- arima(sales[train], c(5,0,3))
arima_501 <- arima(sales[train], c(5,0,1))
arima_507 <- arima(sales[train], c(5,0,7))

#AIC
arima_500$aic
arima_503$aic
arima_501$aic
arima_507$aic

#short term forecast
short_arima_50 <- fitted(Arima(y=sales[test],model=arima_500))
short_arima_51 <- fitted(Arima(y=sales[test],model=arima_501))
short_arima_53 <- fitted(Arima(y=sales[test],model=arima_503))
short_arima_57 <- fitted(Arima(y=sales[test],model=arima_507))

#compare RMSE
mse.arima_50 <- sqrt( mean((short_arima_50-sales[test])^2))
mse.arima_51 <- sqrt( mean((short_arima_51-sales[test])^2))
mse.arima_53 <-sqrt( mean((short_arima_53-sales[test])^2))
mse.arima_57 <-sqrt( mean((short_arima_57-sales[test])^2))

#forecast plot for short-term
#arma(5,3)
par(mfrow=c(1,2), mar=c(4,4.5,1,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(sales$TS.1,type="l",xlab="Time", ylab="sales",main="ARMA(5,3)",xaxp = c(0, 520, 2),axes=FALSE);box()
axis(1, at = 260*(0:2) ,labels = c(2019,2020,2021))
axis(2)
lines(sales$TS.1, col="grey")
lines(test,short_arima_53, col=2)
abline(v=max(train),lty=2)

#arma(5,7)
plot(sales$TS.1,type="l",xlab="Time",ylab="sales", main="ARMA(5,7)",xaxp = c(0, 520, 2),axes=FALSE);box()
axis(1, at = 260*(0:2) ,labels = c(2019,2020,2021))
axis(2)
lines(sales$TS.1, col="grey")
lines(test,short_arima_57, col=2)
abline(v=max(train),lty=2)

#Long-term out-of-sample forecast
library(forecast)
long_arima_503 <- forecast(arima_503, h=(n-max(train)))
long_arima_507 <- forecast(arima_507, h=(n-max(train)))
long_arima_500 <- forecast(arima_500, h=(n-max(train)))
#forecast plot for long-term
#arma(5,3)
par(mfrow=c(1,2), mar=c(4,4.5,1,1), cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(sales$TS.1,type="l",xlab="Time",ylab="sales", main="ARMA(5,3)",xaxp = c(0, 520, 2),axes=FALSE);box()
axis(1, at = 260*(0:2) ,labels = c(2019,2020,2021))
axis(2)
lines(test,long_arima_503$mean, col=2)
polygon(c(test,rev(test)), c(long_arima_503$lower[,2],rev(long_arima_503$upper[,2])), col="pink")
lines(sales$TS.1, col="grey")
lines(test, long_arima_503$mean, col=2, lwd=2)
lines(test, long_arima_503$lower[,2], col=2, lwd=2)
lines(test, long_arima_503$upper[,2], col=2, lwd=2)
abline(v=max(train),lty=2)

#arma(5,7)
plot(sales$TS.1,type="l",xlab="Time",ylab="sales", main="ARMA(5,7)",xaxp = c(0, 520, 2),axes=FALSE);box()
axis(1, at = 260*(0:2) ,labels = c(2019,2020,2021))
axis(2)
lines(test,long_arima_507$mean, col=2)
polygon(c(test,rev(test)), c(long_arima_507$lower[,2],rev(long_arima_507$upper[,2])), col="pink")
lines(sales$TS.1, col="grey")
lines(test, long_arima_507$mean, col=2, lwd=2)
lines(test, long_arima_507$lower[,2], col=2, lwd=2)
lines(test, long_arima_507$upper[,2], col=2, lwd=2)
abline(v=max(train),lty=2)

#check residual
checkresiduals(arima_503)
checkresiduals(arima_507)

