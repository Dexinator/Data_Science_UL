# Install and load the ggcorrplot package
install.packages("ggcorrplot")
library(ggcorrplot)

dataset<-read.csv("A:\\Downloads\\Prueba\\Examen\\Tooltest-dataset.csv", header=TRUE)
head(dataset)
dim(dataset)

plot(dataset)
linear_model=lm(formula = Target ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
                  free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = dataset)
summary(linear_model)

#Check Normalidad


lm_residuals=linear_model$residuals
hist(lm_residuals)#No parece normal
qqnorm(lm_residuals)
qqline(lm_residuals)#Definitivamente no se comportan como normal

#Multicollinearity 
reduced_data <- subset(dataset, select = -Target)
corr_matrix = round(cor(reduced_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

reduced_data_no_density <- subset(dataset, select = -c(Target, density))
corr_matrix_no_density = round(cor(reduced_data_no_density), 2)
ggcorrplot(corr_matrix_no_density, hc.order = TRUE, type = "lower",
           lab = TRUE)

linear_train_model <- train(
  Target ~ fixed.acidity + volatile.acidity + residual.sugar + chlorides +
    free.sulfur.dioxide + density + pH + sulphates, data = dataset,   method = "lm",family = "binomial")

linear_train_model$results


#voy a quitar por encima de .75, Es decir alcohol y residual.sugar
second_linear_model=lm(formula = Target ~ fixed.acidity + volatile.acidity + citric.acid + chlorides +
                  free.sulfur.dioxide + total.sulfur.dioxide +alcohol+residual.sugar + pH + sulphates, data = dataset)
#Check Normalidad 2
lm_residuals2=second_linear_model$residuals
summary(second_linear_model)

hist(lm_residuals2)#No parece normal
qqnorm(lm_residuals2)
qqline(lm_residuals2)#Definitivamente no se comportan como normal

par(mfrow=c(2,2))
hist(lm_residuals)#No parece normal
hist(lm_residuals2)#No parece normal
qqnorm(lm_residuals)
qqline(lm_residuals)#Definitivamente no se comportan como normal
qqnorm(lm_residuals2)
qqline(lm_residuals2)#Definitivamente no se comportan como normal


#Multicollinearity 
reduced_data2 <- subset(dataset, select = -c(Target, alcohol, residual.sugar))
corr_matrix2 = round(cor(reduced_data2), 2)
ggcorrplot(corr_matrix2, hc.order = TRUE, type = "lower",
           lab = TRUE)

#Analicemos las varianzas
var_an=anova(linear_model, second_linear_model)
var_an$`Pr(>F)`
#Mucho menos de .05 rechazamos la hipótesis de que el segundo modelo sea mejor

#Análisis de R cuadradas
summary1=summary(linear_model)
summary2=summary(second_linear_model)
summary1$r.squared
summary2$r.squared
#Concluimos que el primer modelo es mejor

#Analicemos los p-values de las variables del primer modelo
summary1
#Quitemos todos los que sean mayores a .05
linear_model_3=lm(formula = Target ~ fixed.acidity + volatile.acidity +free.sulfur.dioxide + 
                     density + pH + sulphates, data = dataset)
#Check Normalidad 3
lm_residuals3=linear_model_3$residuals
hist(lm_residuals3)#No parece normal
qqnorm(lm_residuals3)
qqline(lm_residuals3)#Definitivamente no se comportan como normal

#ANOVA lm1 vs lm3
anova(linear_model, linear_model_3)
summary(linear_model_3)$r.squared
summary1$r.squared

#plots residuales
res <- resid(linear_model)
plot(fitted(linear_model), res)
abline(0,0)

par(mfrow=c(1,1))

res <- resid(second_linear_model)
plot(fitted(second_linear_model), res)
abline(0,0)

res <- resid(linear_model_3)
plot(fitted(linear_model_3), res)
abline(0,0)

plot(x=predict(linear_model), y= dataset$Target,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)

