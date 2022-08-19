####SetWorkingDir
setwd("D:/skill_learning/R/workspace")

####PackageRequired
library(fpp2)
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)
library(tidyverse)
library(forecast)

####DataLoading
data <- read.csv("D:/skill_learning/R/workspace/TradingData.csv")
data <- as.data.frame(data)

####DataCleaning
Sys.setlocale("LC_TIME","English")
as.Date("9-Sep-19",format="%d-%b-%y")  
data$Date <- as.Date(data$Date,format="%d-%b-%y")  

library(VIM)
aggr(data,prop=FALSE,numbers=TRUE)

#去除逗号
data$Open <- gsub('[,]', '', data$Open)
data$Open <- as.numeric(data$Open)

data$High <- gsub('[,]', '', data$High)
data$High <- as.numeric(data$High)

data$Low <- gsub('[,]', '', data$Low)
data$Low <- as.numeric(data$Low)

data$Close <- gsub('[,]', '', data$Close)
data$Close <- as.numeric(data$Close)

data$Volume <- gsub('[,]', '', data$Volume)
data$Volume <- as.numeric(data$Volume)

data$Market.Cap <- gsub('[,]', '', data$Market.Cap)
data$Market.Cap <- as.numeric(data$Market.Cap)

#Conclude
data_market.cap <- aggregate(data$Market.Cap, by=list(type=data$Date),sum)

data_open <- aggregate(data$Open, by=list(type=data$Date),sum)

data_high <- aggregate(data$High, by=list(type=data$Date),sum)

data_low <- aggregate(data$Low, by=list(type=data$Date),sum)

data_close <- aggregate(data$Close, by=list(type=data$Date),sum)

data_volume <- aggregate(data$Volume, by=list(type=data$Date),sum)

####Modelling

mydata <- data.frame(data_market.cap, data_open$x,data_high$x,
                     data_low$x,data_close$x, data_volume$x)

mydata01 <- data.frame(data_market.cap)

mydata01 <- mydata01[1330:2412,]

mydata <- rename(mydata, c("market.cap"="x", "open"="data_open.x","high"="data_high.x",
                 "low"="data_low.x","close"="data_close.x","volume"="data_volume.x"))
mydata$type <- as.Date(mydata$type)

mydata <- mydata[1330:2412,]

####Visualization
# package
library(ggplot2)
# patch
library(patchwork)


#LostValueTest
library(VIM)
aggr(mydata,prop=FALSE,numbers=TRUE)

####TimeSeriesAnalysis
ts.plot(mydata)
acf(mydata)

mydata %>% 
  plot_time_series(mydata$type, mydata$market.cap, .interactive = F) + 
  xlab("Date") +
  ylab("market.cap")

splits = mydata %>% 
  time_series_split(assess = "6 months", cumulative = T)

splits %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(mydata$type, mydata$market.cap, .interactive = F)


model_arima = arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(mydata$market.cap ~ mydata$type, training(splits))

####Forecast

capTS <-ts(mydata01$x[1:1083],frequency=365,start=c(2016,12,17))  
plot.ts(capTS,col="red")

capForecasts<- HoltWinters(capTS)
capForecasts

capForecast2<- forecast:::forecast.HoltWinters(capForecasts,h=3650) 
plot(capForecast2$mean,col="red",main="figure",xlab="time",ylab="market cap")
capForecast2$mean

legend("topright",c("MarketCap"),col=c("red"),lty=1)

fullTS <- ts(rbind(data.frame(sales=src_dat$sales),data.frame(sales=salesForecast2$mean)),frequency=12,start=c(2015,1,1))  
plot.ts(fullTS,col="red") 

