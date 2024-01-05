library(dplyr)
library(ggcorrplot)
library(RcolorBrewer)
library(colorspace)
library(ltm)

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

#Quitamos las columnas MarkDowns que no sabemos qué son o sin son relevantes
feats=subset(feats, select=-c(MarkDown1,MarkDown2,MarkDown3,MarkDown4,MarkDown5))

#Agrupo las ventas por fecha y tienda. Estos son los valores que necesitaré para unir mis datos en una sola tabla
train_deptgpd<-train %>% 
  group_by(Date, Store) %>%
  summarize(venta = sum(Weekly_Sales))

#Ya que tenemos nuestro tiempo y datos quiero quitar los outliers existentes
#Lo haré con el método IQR (rango intercuartílico) que me permitirá detectarlos correctamente
quantiles <- quantile(train_deptgpd$venta, probs=c(.25, .75))
IQR=IQR(train_deptgpd$venta)
Lower= quantiles[1] - 1.5*IQR
Upper= quantiles[2] + 1.5*IQR 
train_deptgpd[3][train_deptgpd[3]>Upper]=Upper
train_deptgpd[3][train_deptgpd[3]<Lower]=Lower
#Cambié todos mis outliers por el valor del rango superior o inferior según el caso



#Uno mis tablas, por fecha y por tienda. Así será más fácil encontrar relaciones entre las variables
full_data_xDate_xStore=merge(merge(feats,  train_deptgpd, by = c( "Date", "Store"), all.x=TRUE), stores, by="Store", all.x=TRUE)
head(full_data_xDate_xStore)

#Obtendré las correlaciones entre las variables no categóricas o continuas 
reduced_data <- subset(full_data_xDate_xStore, select = -c(Store, Date, IsHoliday, Type))
reduced_data[is.na(reduced_data)] = 0
corr_matrix = round(cor(reduced_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)
#Aquí podemos ver que la mayor correlación con la variable ventas la presenta la variable Size

#Pruebas de correlación de variables categóricas
full_data_xDate_xStore$IsHoliday[is.na(full_data_xDate_xStore$IsHoliday)] = 0
full_data_xDate_xStore$Type[is.na(full_data_xDate_xStore$Type)] = 0
full_data_xDate_xStore$venta[is.na(full_data_xDate_xStore$venta)] = 0
chisq.test(full_data_xDate_xStore$IsHoliday, full_data_xDate_xStore$venta)
chisq.test(full_data_xDate_xStore$Type, full_data_xDate_xStore$venta)
#En ninguno de los dos casos tenemos un p value suficiente para afirmar que existe independencia

#Utilizaremos Point-biserial correlation coefficient que es util para encontrar correlación entre
#una variable dicotómica y otra continua
biserial.cor(full_data_xDate_xStore$venta,full_data_xDate_xStore$IsHoliday)
#La correlación existe más no es mayor que las encontradas anteriormente


#Cantidad de tiendas
cant_tiendas=length(unique(full_data_xDate_xStore$Store))
#Cantidad de departamentos únicos
cant_deptos=length(unique(train$Dept))

#Ventas por Tienda
tiendas_xventa<-train_deptgpd %>% 
  group_by(Store) %>%
  summarize(venta = sum(venta))
tiendas_xventa$venta<-tiendas_xventa$venta

#10 Tiendas con mayor venta
top10_ventas= tiendas_xventa %>%
  arrange(desc(venta)) %>%
  slice(1:10) 

#Graficamos las 10 tiendas con mayor venta. Ajustamos el eje y para que los valores sean más legibles
barplot( height=top10_ventas$venta/1000000, names=top10_ventas$Store, main = "Top 10 ventas",
         xlab = "Tienda", ylab = "Monto (Millones)", 
         col = rainbow_hcl(10),
)

#Para hacer un análisis x depto es necesario no agrupar las ventas como habíamos hecho anteriormente
#Juntar nuestros datos por fecha y Tienda, recordando que cada tienda tiene sus deptos.
train_noHoliday=subset(train,select=-IsHoliday)
full_data_dept=merge(merge(feats,  train_noHoliday, by = c( "Date", "Store"), all.x=TRUE),stores, by="Store", all.x=TRUE)

#Si queremos analizar el comportamiento de las variables x departamento es importantes notar que los departamentos
#de distintas locaciones (porque son de diferentes tiendas),es decir las demás variables no se suman.
#En cambio me parece que hacer las medias de cada una podría darnos información útil para ver si existen
#correlaciones importantes para cada departamento
venta_depto_fecha<-full_data_dept %>% 
  group_by(Date, Dept) %>%
  summarize(Venta = sum(Weekly_Sales),Temperatura=mean(Temperature), Size=mean(Size), CPI=mean(CPI), Unemployment=mean(Unemployment))

#Obtendremos las correlaciones entre las variables, es necesario quitar la fecha
venta_depto_fecha <- subset(venta_depto_fecha, select = -Date)
venta_depto_fecha[is.na(venta_depto_fecha)] =0
corr_xdepto = round(cor(venta_depto_fecha), 2)
ggcorrplot(corr_xdepto, hc.order = TRUE, type = "lower",
           lab = TRUE) 
#Podemos observar que Size sigue siendo la variable con mayor correlación


#Para saber si la variable con mayor correlación cambia entre departamentos se hará un loop en el que iremos
#departamento x departamento obteniendo la variable que tenga mayor correlación con las ventas
#finalmente guardándolo en una lista para poder ver los resultados de todos los departamentos
full_data_dept=subset(full_data_dept, select=-c(Store, Date, IsHoliday, Type))
full_data_dept[is.na(full_data_dept)] <- 0
Variable_Con_Mayor_correlacion_Depto=list()
for(i in 1:81) {
  
  dept1=filter(full_data_dept, Dept == i)
  dept1_cor=cor(dept1)
  dept1_cor=abs(dept1_cor[,6][-c(5, 6)])
  dept1_cor=names(dept1_cor)[which.max(dept1_cor)]
  Variable_Con_Mayor_correlacion_Depto=append(Variable_Con_Mayor_correlacion_Depto, dept1_cor)
}
Mayor_Correlacion=data.frame(table(sapply(Variable_Con_Mayor_correlacion_Depto, function(x) x)))
#Obtenemos la tabla donde vemos que Size sigue siendo la variable que mayor correlación tiene con ventas
#como puede ser que esto no sea tan útil para la toma de algunas decisiones haremos el mismo análisis pero
#sin Size
full_data_dept=subset(full_data_dept, select=-c(Store, Date, IsHoliday, Type, Size))
full_data_dept[is.na(full_data_dept)]=0
Variable_Con_Mayor_correlacion_Depto=list()
for(i in 1:81) {
  
  dept1=filter(full_data_dept, Dept == i)
  dept1_cor=cor(dept1)
  dept1_cor=abs(dept1_cor[,6][-c(5, 6)])
  dept1_cor=names(dept1_cor)[which.max(dept1_cor)]
  Variable_Con_Mayor_correlacion_Depto=append(Variable_Con_Mayor_correlacion_Depto, dept1_cor)
}
Mayor_Correlacion_sin_Size=data.frame(table(sapply(Variable_Con_Mayor_correlacion_Depto, function(x) x)))
#Obtenemos que sin la variable Size la que más se correlaciona con ventas x depto es CPI

#Cuántas tiendas hay por tipo de tienda
b=barplot(table(stores$Type),main = "Cantidad de tiendas x tipo",
        xlab = "Tipo", ylab = "Cantidad", 
        col = rainbow_hcl(3) )
text(b, labels =table(stores$Type))

#Histograma tamaño de las tiendas
hist(stores$Size)
