library(caret)
library(dplyr)
library(arm)
library(kernlab)

#importamos los datos, en caso no los tengamos aún
dataset<-read.csv("A:\\Downloads\\Prueba\\Examen\\Tooltest-dataset.csv", header=TRUE)

#Quitaremos la variable density, pues como vimos en el ejercicio anterior tiene correlaciones altas con más
#variables. Además, haremos un análisis de correlación tras quitarla para comprobar que no exista alguna más
dataset <- subset(dataset,select=-density)
head(dataset)
correlation_0 = round(cor(dataset), 2)
ggcorrplot(correlation_0, hc.order = TRUE, type = "lower",
           lab = TRUE)
#No tenemos ninguna correlación arriba de .75

#Comenzamos con el modelo logístico, este modelo puede ser útil para estimar valores entre 0 y 1. Además
#cuando un valor es muy grande (positivo o negativo), tiende a 1 o a cero (respectivamente).
log_model <- glm(Target ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
                   free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = dataset, family = "binomial")
summary(log_model)
#Vemos que el p-values de fixed.acidity es mayor a .05 por lo que no es estadísticamente significante
#Lo quitaremos y comprobaremos si el modelo mejora

#Modelo logístico 2, quitamos la variable fixed.acidity
log_model2 <- glm(Target ~  volatile.acidity + citric.acid + residual.sugar + chlorides +
                  free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = dataset, family = "binomial")
log_model$df.residual
log_model2$df.residual
log_model$null.deviance
log_model2$null.deviance
log_model$aic
log_model2$aic
#No hay suficiente evidencia de una mejora significativa con estos indicadores. Tampoco de algo peor.

#Para comprobar cuál de los dos es mejor, haremos una prueba dividiendo nuestro dataset en train(80%) y test(20%)
#Con esto podríamos ver cuál de los dos se desempeña mejor con la variable dependiente.
split = round(nrow(dataset) * 0.80)
Train_Data=dataset[1:split, ]
Test_Data=dataset[(split + 1):nrow(dataset), ]
log_model <- glm(Target ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
                   free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = Train_Data, family = "binomial")
Test_Data$model_prob <- predict(log_model, Test_Data, type = "response")
Test_Data <- Test_Data  %>% mutate(model_pred = 1*(model_prob > .5) + 0,
                         target_binary = 1*(Target = 1) + 0)
head(Test_Data)
Test_Data <- Test_Data %>% mutate(accurate = 1*(model_pred == Target))
Acclogmodel=sum(Test_Data$accurate)/nrow(Test_Data)
#Tras darle solo el 80% de los datos a log_model cuando probamos este modelo con el 20% restante
#Tenemos un accuracy de 78.26%
log_model2 <- glm(Target ~  volatile.acidity + citric.acid + residual.sugar + chlorides +
                    free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = Train_Data, family = "binomial")
Test_Data$model_prob2 <- predict(log_model2, Test_Data, type = "response")
Test_Data <- Test_Data  %>% mutate(model_pred2 = 1*(model_prob2 > .5) + 0,
                                   target_binary = 1*(Target = 1) + 0)
head(Test_Data)
Test_Data <- Test_Data %>% mutate(accurate2 = 1*(model_pred2 == Target))
Acclogmodel2=sum(Test_Data$accurate2)/nrow(Test_Data)
#Hicimos lo mismo con el log_model2 y tenemos un accuracy 78.67
#Es una mejora muy pequeña pero al comparar los modelos también tenemos más certeza ya de que funcionan

#Con un Binned Residual Plot podemos crear bandas de confianza para modelos logísticos y comprobar de una
#manera visual que la mayoría de nuestros residuos están dentro de ellas
binnedplot(fitted(log_model2), 
           residuals(log_model2, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

