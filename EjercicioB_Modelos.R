install.packages("seasonal")
install.packages("x13binary")
install.packages("forecast")
install.packages("fpp2")
install.packages("tseries")
install.packages("lubridate",dependencies = TRUE)
install.packages("ggplot2")
install.packages("stats")

library(lubridate)
library(forecast)
library(fpp2)
library(tseries)
library(x13binary)
library(seasonal)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stats)

feats=read.csv("A:\\Downloads\\Prueba\\Examen\\walmart-recruiting-store-sales-forecasting\\features.csv")
stores=read.csv("A:\\Downloads\\Prueba\\Examen\\walmart-recruiting-store-sales-forecasting\\stores.csv")
train=read.csv("A:\\Downloads\\Prueba\\Examen\\walmart-recruiting-store-sales-forecasting\\train.csv")

colSums(is.na(train))

#Mi primer dato es del viernes 5 de Febrero, hace sentido que sea la 5 toma del año pues el primer viernes del año fue dia primero y tendría la info del año anterior
ventas_semanales <- train %>% group_by(Date) %>% 
  summarize(venta_semanal=sum(Weekly_Sales),
            .groups = 'drop')

as.Date(ventas_semanales$Date,format="%m/%d/%Y")

ventas_mensuales<-ventas_semanales %>% 
  group_by(month = lubridate::floor_date(as.Date(Date,format="%m/%d/%Y"), 'month')) %>%
  summarize(venta_mensual = sum(venta_semanal))

ventas_anuales<-ventas_semanales %>% 
  group_by(year = lubridate::floor_date(as.Date(Date,format="%m/%d/%Y"), 'year')) %>%
  summarize(venta_anual = sum(venta_semanal))

ST_Ventas=ts(ventas_mensuales$venta_mensual, frequency = 12, start = c(2010, 2))
print(ST_Ventas, calendar = T)
sT_Ventas_WM=seas(ST_Ventas)


t=0:32
par(mfcol=c(2,2))
plot(t,ventas_mensuales$venta_mensual,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Stationary signal")
acf(ventas_mensuales$venta_mensual,lag.max = length(ventas_mensuales$venta_mensual),
    xlab = "lag #", ylab = 'ACF',main=' ')

t2=0:142
plot(t2,ventas_semanales$venta_semanal,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Stationary signal")
acf(ventas_semanales$venta_semanal,lag.max = length(ventas_semanales$venta_semanal),
    xlab = "lag #", ylab = 'ACF',main=' ')



par(mfcol=c(1,1))
plot(ventas_mensuales, type="l")
plot(ventas_semanales$venta_semanal, type="l")
plot(ventas_anuales$venta_anual, type="l")


ST_Ventas %>%
  auto.arima() %>%
  forecast(h=20) %>%
  autoplot()

Comps_ST_Ventas<- decompose(ST_Ventas)
plot(Comps_ST_Ventas)

#Quitemos outliers
quartiles <- quantile(train$Weekly_Sales, probs=c(.25, .75), na.rm = FALSE)
IQR=IQR(train$Weekly_Sales)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(train, train$Weekly_Sales > Lower & train$Weekly_Sales< Upper)

ventas_semanales_NO <- data_no_outlier %>% group_by(Date) %>% 
  summarize(venta_semanal=sum(Weekly_Sales),
            .groups = 'drop')

ventas_mensuales_NO<-ventas_semanales_NO %>% 
  group_by(month = lubridate::floor_date(as.Date(Date,format="%m/%d/%Y"), 'month')) %>%
  summarize(venta_mensual = sum(venta_semanal))

ventas_anuales_NO<-ventas_semanales_NO %>% 
  group_by(year = lubridate::floor_date(as.Date(Date,format="%m/%d/%Y"), 'year')) %>%
  summarize(venta_anual = sum(venta_semanal))

par(mfcol=c(3,1))
plot(ventas_mensuales_NO, type="l")
plot(ventas_semanales_NO$venta_semanal, type="l")
plot(ventas_anuales_NO$venta_anual, type="l")

ST_Ventas_NO=ts(ventas_semanales_NO$venta_semanal, frequency = 52, start = c(2010, 5))
ST_Ventas_NO=log(ST_Ventas_NO)
ST_Ventas_Mensual_NO=ts(ventas_mensuales_NO$venta_mensual, frequency = 12, start = c(2010, 2))
ST_Ventas_Mensual_NO=log(ST_Ventas_Mensual_NO)
ST_Ventas_NO %>%
  auto.arima() %>%
  forecast(h=52) %>%
  autoplot()

ggseasonplot(ST_Ventas_NO)
ggseasonplot(ST_Ventas_Mensual_NO)

lambda0<-BoxCox.lambda(ST_Ventas_NO)
lambda0
BoxCoxWM<-BoxCox(ST_Ventas_NO,lambda0)
BoxCoxWM
ST_Ventas_NO
autoplot(BoxCoxWM)
ndiffs(BoxCoxWM)

ggAcf(BoxCoxWM,lag.max = 100)

predict_model<-Arima(ST_Ventas_NO,order=c(1,0,1))
checkresiduals(predict_model)
#Parece que los residuos se comportan como ruido blanco

autoplot(forecast(predict_model))


ndiffs(ST_Ventas_NO)
adf.test(BoxCoxWM) #Dickey Fulier
adf.test(ST_Ventas_NO)
acf(ST_Ventas_NO)


