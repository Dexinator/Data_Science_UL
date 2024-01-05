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


#Lo primero que haremos es importar nuestros datos y revisar los encabezados para ver el tipo de datos
feats=read.csv("A:\\Downloads\\Prueba\\Examen\\walmart-recruiting-store-sales-forecasting\\features.csv")
stores=read.csv("A:\\Downloads\\Prueba\\Examen\\walmart-recruiting-store-sales-forecasting\\stores.csv")
train=read.csv("A:\\Downloads\\Prueba\\Examen\\walmart-recruiting-store-sales-forecasting\\train.csv")
head(feats)
head(train)
head(stores)

#Veo que existen algunos valores NA por lo que procederé a convertirlos en ceros para evitar futuros conflictos
#También convertiré las fechas a formato Date para evitar futuros errores
feats[is.na(feats)] = 0
stores[is.na(stores)] = 0
train[is.na(train)] = 0
train$Date=as.Date(train$Date,format="%m/%d/%Y")
feats$Date=as.Date(feats$Date,format="%m/%d/%Y")


#Primero haremos un análisis desde la perspectiva de series de tiempo.
#Para esto, es necesario agrupar los datos por fecha, esta información se encuentra en los datos train
#Es importante que si agrupo las fechas sume las ventas para cada una.
ventas_semanales <- train %>% group_by(Date) %>% 
  summarize(venta_semanal=sum(Weekly_Sales))
head(ventas_semanales)

#Ya que tenemos nuestro tiempo y datos quiero quitar los outliers existentes
#Lo haré con el método IQR (rango intercuartílico) que me permitirá detectarlos correctamente
quantiles <- quantile(ventas_semanales$venta_semanal, probs=c(.25, .75))
IQR=IQR(ventas_semanales$venta_semanal)
Lower= quantiles[1] - 1.5*IQR
Upper= quantiles[2] + 1.5*IQR 
ventas_semanales[2][ventas_semanales[2]>Upper]=Upper
ventas_semanales[2][ventas_semanales[2]<Lower]=Lower
#Cambié todos mis outliers por el valor del rango superior o inferior según el caso

#Revisaré la gráfica para tener una aproximación visual a mis datos
plot(ventas_semanales, type="l")
#Se ve bien, parece que el acercamiento para outliers fue correcto

#Comenzaré creando mi serie de tiempo con frequencia 52 para que sea semanal
#Mi primer dato es del viernes 5 de Febrero, hace sentido que sea la 5 toma del año pues el primer viernes
#del año fue dia primero y tendría la info del año anterior
ST_Ventas=ts(ventas_semanales$venta_semanal, frequency = 52, start = c(2010, 5))
ggseasonplot(ST_Ventas)
#Graficando la serie de tiempo cada año se puede apreciar cierta estacionalidad
#Para tener más claridad acerca de la estacionalidad haremos también la gráfica por mes
ventas_mensuales<-ventas_semanales %>% 
  group_by(month = lubridate::floor_date(Date, 'month')) %>%
  summarize(venta_mensual = sum(venta_semanal))
ST_Ventas_Mensual=ts(ventas_mensuales$venta_mensual, frequency = 12, start = c(2010, 2))
ggseasonplot(ST_Ventas_Mensual)

#Haremos la prueba de Dickey-Fuller para probar estacionariedad
adf.test(ST_Ventas)
#Nuestro valor p es menor a .01 por lo que puedo concluir que la serie tiene un comportamiento estacionario y
#es adecuada para un modelo ARIMA

#Aplicaré la función decompose tratándola como una serie aditiva ya que estamos trabajando con gasto
#la intuición me dice que obedece a una suma más que a alguna multiplicación
Comps_ST_Ventas<- decompose(ST_Ventas)
plot(Comps_ST_Ventas$trend)
#Aquí vemos la existencia de una tendencia al crecimiento

#Una buena primera aproximación a un modelo es un ARIMA, utilizaremos solo las observaciones que ya tenemos
#de las ventas en el tiempo, ya ordenadas como serie de tiempo con la carga estacional y estacionaria que
#tienen.
ST_Ventas %>%
  auto.arima() %>%
  forecast(h=50) %>%
  autoplot()
modelo_ARIMA=auto.arima(ST_Ventas)

#Por último revisamos los residuos, visualmente se comportan de manera Normal
#Haremos un test Ljung-Box para comprobarlo
checkresiduals(modelo_ARIMA)
#Parece ser una buena propuesta como modelo de predicción