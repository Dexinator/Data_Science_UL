library(dplyr)
library(ggcorrplot)
install.packages("RcolorBrewer")
library(RcolorBrewer)
install.packages("colorspace")
library(colorspace)

feats=as.data.frame(read.csv("A:\\Downloads\\Prueba\\Examen\\walmart-recruiting-store-sales-forecasting\\features.csv"))
stores=read.csv("A:\\Downloads\\Prueba\\Examen\\walmart-recruiting-store-sales-forecasting\\stores.csv")
train=read.csv("A:\\Downloads\\Prueba\\Examen\\walmart-recruiting-store-sales-forecasting\\train.csv")

colSums(is.na(train))
feats$Date=as.Date(feats$Date,format="%m/%d/%Y")
train$Date=as.Date(train$Date,format="%m/%d/%Y")

train_deptgpd<-train %>% 
  group_by(Date, Store) %>%
  summarize(venta = sum(Weekly_Sales),.groups = "drop")


full_data=merge(feats,  train_deptgpd, by = c( "Date", "Store"), all.x=TRUE)
full_data2=merge(full_data,  stores, by="Store", all.x=TRUE)
head(full_data2)

linear_model=lm(formula = venta ~ Temperature + Fuel_Price  + CPI + Unemployment +
                  factor(IsHoliday) + factor(Type) + Size, data = full_data2, na.action = na.omit)
lm_residuals=linear_model$residuals
hist(lm_residuals)#No parece normal
qqnorm(lm_residuals)
qqline(lm_residuals)#Definitivamente no se comportan como normal

#Multicollinearity 
reduced_data <- subset(full_data2, select = -c(venta, MarkDown1 ,MarkDown2, MarkDown3, MarkDown4, MarkDown5, IsHoliday, Type, Date))
reduced_data[is.na(reduced_data)] <- 0
corr_matrix = round(cor(reduced_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)


#Analicemos p values de variuables
summary(linear_model)

#
#plots residuales
res <- resid(linear_model)
plot(fitted(linear_model), res)
abline(0,0)


plot(x=predict(linear_model), y= na.omit(full_data2$venta),
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)

#Cantidad de tiendas
cant_tiendas=length(unique(full_data2$Store))
#Cantidad de departamentos únicos
cant_deptos=length(unique(train$Dept))

#cantidad de deptos por tienda

tiendas_xventa<-train_deptgpd %>% 
  group_by(Store) %>%
  summarize(venta = sum(venta),.groups = "drop")
tiendas_xventa$venta<-tiendas_xventa$venta

top10_ventas= tiendas_xventa %>%
  arrange(desc(venta)) %>%
  slice(1:10) 

barplot( height=top10_ventas$venta/1000000, names=top10_ventas$Store, main = "Top 10 ventas",
         xlab = "Tienda", ylab = "Monto (Millones)", 
         col = rainbow_hcl(10),
)


reduced_data2 <- subset(full_data2, select = -c(Store, MarkDown1 ,MarkDown2, MarkDown3, MarkDown4, MarkDown5, IsHoliday, Type, Date))
reduced_data2[is.na(reduced_data2)] <- 0
corr_matrix2 = round(cor(reduced_data2), 2)
ggcorrplot(corr_matrix2, hc.order = TRUE, type = "lower",
           lab = TRUE)                    
cor(reduced_data2)

cor.test(reduced_data2$venta,reduced_data2$Temperature)

sales_xstore_xdepts<-train %>% 
  group_by(Store,Date, Dept) %>%
  summarize(venta = sum(Weekly_Sales),.groups = "drop")
full_data_dept=merge(feats,  sales_xstore_xdepts, by = c( "Date", "Store"), all.x=TRUE)
full_data2_dept=merge(full_data_dept,  stores, by="Store", all.x=TRUE)

head(full_data2_dept)

venta_depto_fecha<-full_data2_dept %>% 
  group_by(Date, Dept) %>%
  summarize(Venta = sum(venta),Temperatura=mean(Temperature), Size=mean(Size), CPI=mean(CPI), Unemployment=mean(Unemployment))




venta_depto_fecha <- subset(venta_depto_fecha, select = -Date)
venta_depto_fecha[is.na(venta_depto_fecha)] <- 0


corr_matrix3 = round(cor(venta_depto_fecha), 2)
ggcorrplot(corr_matrix3, hc.order = TRUE, type = "lower",
           lab = TRUE) 

par(mfrow=c(2,2))


head(reduced_dept)


full_data2_dept=subset(full_data2_dept, select=-c(Store, Date, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, IsHoliday, Type))
full_data2_dept[is.na(full_data2_dept)] <- 0
Variable_Con_Mayor_correlacion_Depto=list()
for(i in 1:81) {
  
  dept1=filter(full_data2_dept, Dept == i)
  dept1_cor=cor(dept1)
  dept1_cor=abs(dept1_cor[,6][-c(5, 6)])
  dept1_cor=names(dept1_cor)[which.max(dept1_cor)]
  Variable_Con_Mayor_correlacion_Depto=append(Variable_Con_Mayor_correlacion_Depto, dept1_cor)
}
summary(Variable_Con_Mayor_correlacion_Depto)

data.frame(table(sapply(Variable_Con_Mayor_correlacion_Depto, function(x) x)))
