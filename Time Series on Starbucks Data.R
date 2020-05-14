#Read the file
starbucks=read.csv('starbucks.csv')
class(starbucks)
dim(starbucks)
str(starbucks)

starbucks$Date=as.Date(starbucks$Date,format="%Y-%m-%d")
str(starbucks)

min_date=min(starbucks$Date)
max_date=max(starbucks$Date)
View(starbucks)

date_seq=data.frame('date'=seq(min_date,max_date, by='days'))

new_data=merge(starbucks, date_seq, by.x = 'Date', by.y = 'date', all.y = T)

#NA imputation
x = c(4,5,11,6,NA,NA,9,5)
library(zoo)
(na.locf(x) + na.locf(x, fromLast =  T))/2

imputed_close=na.locf(new_data$Close)
imputed_volume=na.locf(new_data$Volume)

new_data$Close=NULL
new_data$Volume=NULL

new_data$imputed_close=imputed_close
new_data$imputed_volume=imputed_volume
View(new_data)

new_data$week=as.numeric(format(new_data$Date,format="%Y.%W"))

library(dplyr)
data_weekly=new_data %>% group_by(week) %>% summarise(AVG_Price=mean(imputed_close), 
                                                      AVG_Volume=mean(imputed_volume))
View(data_weekly)

#Train test split
train=data_weekly[which(data_weekly$week<2018.51),]
test=data_weekly[which(data_weekly$week>2018.50),]
View(train)
class(train)
View(test)
dim(train)
price=ts(train$AVG_Price, frequency = 52)
price
length(price)

plot(price)
acf(diff(price))
acf(price)
pacf(price)

library(forecast)
#Simple Moving average
sm=ma(price, order = 2)
sm_preds=forecast(sm,h=3)
sm_preds
sm_preds$mean
test$AVG_Price

library(DMwR)
regr.eval(test$AVG_Price, sm_preds$mean)
#Exponential moving average
ema_preds=ses(price, h=3)
regr.eval(test$AVG_Price, ema_preds$mean)

#Trend- seasonality
plot(decompose(price))
fit=stl(price, s.window = 'periodic',robust = T)


library(ggplot2)
naive_preds = fit %>% seasadj() %>% naive(h=3)
naive_preds %>% autoplot()
regr.eval(test$AVG_Price, naive_preds$mean)


## With seasonality
seasonal_naive_preds = forecast(fit, method = 'naive', h=3)
seasonal_naive_preds %>% autoplot()
regr.eval(test$AVG_Price, seasonal_naive_preds$mean)

## With trend
drift_preds = forecast(fit, method = 'rwdrift', h=3)
drift_preds %>% autoplot()
regr.eval(test$AVG_Price, drift_preds$mean)

##Holt Winters- used to forecast if the series is seasonal
hw=HoltWinters(price, beta = F, gamma = T, seasonal = 'additive')
hw_preds=forecast(hw, h=3)
hw_preds %>% autoplot()
regr.eval(test$AVG_Price, hw_preds$mean)

hw_preds$mean

#ARIMA
acf(diff(price))
pacf(price)

manual_arima = arima(price, c(1,1,1))
summary(manual_arima)

best_arima = auto.arima(price, ic='aic')
summary(best_arima)

arima_forecast = forecast(best_arima, h=3)
regr.eval(test$AVG_Price, arima_forecast$mean)

plot(price)

## ARIMAX
external_data = cbind(train$AVG_Volume)
colnames(external_data) <- c("Volume")

arimax = auto.arima(price, ic='aic', xreg=external_data)
summary(arimax)

arimax$coef

arimax_forecast = forecast(best_arima, h=3)
regr.eval(test$AVG_Price, arimax_forecast$mean)
