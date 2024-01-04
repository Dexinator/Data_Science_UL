install.packages("caret")
install.packages("ggcorrplot")
library(ggcorrplot)
library(caret)

#Asignamos nuestros datos y hacemos una primera observación de los mismos
dataset<-read.csv("A:\\Downloads\\Prueba\\Examen\\Tooltest-dataset.csv", header=TRUE)
head(dataset)
dim(dataset)
#Desde aquí podemos observar cómo la variable Target parece comportarse de forma binaria

#Comprobamos que Target solo está compuesta por ceros y unos. Con esto ya podemos intuir que un modelo lineal
#no será lo más adecuado.
table(dataset$Target)

#Antes de comenzar a modelar, revisamos las gráficas de todas las variables cruzadas. Con esto podemos ir
#dándonos una idea de su comportamientos y relación con la variable a predecir "Target"
plot(dataset)
#Como se observa en la imagen podemos notar como algunas variables parece que no aportarán mucho, tales como
#Alcohol-Sulfato, en las que vemos puntos por toda el área gráficada, también vemos algunas con cierta correlación
#como free.sulfur.dioxide y total.sulfur dioxide, lo cual hace sentido

#Revisaremos que los valores no tengan algún NA
table(is.na(dataset))
#Ningún NA presente

#Haremos un análisis visual de correlación de las variables
correlation_0 = round(cor(dataset), 2)
ggcorrplot(correlation_0, hc.order = TRUE, type = "lower",
           lab = TRUE)
#Parece que sí existen variables con alta correlación como son density-residual.sugar y density-alcohol
#Una decisión que podríamos tomar en este punto es quitar la variable density, al tener una correlación
#inversa y otra directa podemos creer que sería mejor que quitar residual.sugar o alcohol.
#No lo haremos para comprobarlo con el modelo.
#Por último vemos como Target no parece estar altamente correlacionado con alguna de las demás variables
#Esto nos permite pensar que podríamos tener valores p significativos más adelante

#Procedemos a crear un primer modelo líneal con todas las variables para estimar Target
linear_model=lm(formula = Target ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
                  free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = dataset)
summary(linear_model)
#El resultado no es sorprendente, nuestro mayor indicador de rendimiento en un modelo lineal es R-cuadrada
#Aquí vemos un valor de .17, esto indicaría que nuestro model es capaz de explicar solo un 17% de la
#variabilidad observada en Target
#Dependiendo de la aplicación un buen modelo lineal se esperaría que explique al menos el 50%
#Por último al observar los valores p vemos que algunas variables no serían tan significativas para este modelo
#Como son citric.acid y total.sulfur.dioxide. Tampoco es muy relevante pues el modelo no está siendo efectivo



#Por continuar el ejercicio, haremos un análisis de los residuales. Veremos si se comportan similar a una normal
lm_residuals=linear_model$residuals
hist(lm_residuals)
qqnorm(lm_residuals)
qqline(lm_residuals)
#Con solo el histograma podemos notar que no se comportan de manera normal, además creamos un gráfico 
#Cuantil-Cuantil para ver si los puntos siguen la distribución normal. Muchos quedan fuera y bastante lejos


#A continuación utilizaremos una herramienta de Machine Learning que nos generará un modelo lineal que debería
#ser parecido al que generamos más arriba
linear_train_model <- train(
  Target ~ fixed.acidity + volatile.acidity + residual.sugar + chlorides +
    free.sulfur.dioxide + density + pH + sulphates, data = dataset,   method = "lm",family = "binomial")

linear_train_model$results
#Los resultados son esperados, un valor de R-cuadrada muy parecido al que nosotros generamos


#Por último, haré un segundo modelo sin la variable densidad para ver si existe una mejora significativa
#contra el primer modelo creado
second_linear_model=lm(formula = Target ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
                         free.sulfur.dioxide + total.sulfur.dioxide  + pH + sulphates + alcohol, data = dataset)
summary(second_linear_model)
#El valor de R-cuadrada es muy parecido, no es un modelo apropiado tampoco.

#Repetiremos las pruebas de normalidad en residuos
lm_residuals2=second_linear_model$residuals
hist(lm_residuals2)
qqnorm(lm_residuals2)
qqline(lm_residuals2)
#Comprobamos que no se comportan de manera Normal

#Haremos una prueba de análisis de varianza entre los dos modelos. Este test es útil cuando queremos
#probar si un modelo más complejo justifica mayor cantidad de variables aún con una R-cuadrada mejor
var_an=anova(second_linear_model,linear_model)
var_an$`Pr(>F)`
# El valor p nos indica que nuestro primer modelo es más adecuado
