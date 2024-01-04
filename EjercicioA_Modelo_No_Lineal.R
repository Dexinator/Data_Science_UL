library(caret)
dataset<-read.csv("A:\\Downloads\\Prueba\\Examen\\Tooltest-dataset.csv", header=TRUE)
head(dataset)
dim(dataset)
log_model <- glm(Target ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
                   free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = dataset, family = "binomial")

summary(log_model)


log_model2 <- glm(Target ~ fixed.acidity + volatile.acidity + residual.sugar + chlorides +
                   free.sulfur.dioxide + density + pH + sulphates, data = dataset, family = "binomial")
summary(log_model2)


install.packages("arm")
library("arm")

cv_model1 <- train(
  as.factor(Target) ~ fixed.acidity + volatile.acidity + residual.sugar + chlorides +
    free.sulfur.dioxide + density + pH + sulphates, data = dataset,   method = "glm",family = "binomial")

cv_model1$results$Accuracy
cv_model1$results

cv_model2 <- train(
  Attrition ~ MonthlyIncome + OverTime, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

cv_model1$pred


summary(model1 = cv_model1)
resamples(cv_model1)


binnedplot(fitted(log_model2), 
           residuals(log_model2, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")


install.packages("lmtest")
library("lmtest")
lrtest(log_model, log_model2)

install.packages("caret")
library("caret")
confusionMatrix(as.factor(ifelse(fitted(log_model) > .5, "1", "0")), as.factor(dataset$Target))$table
confusionMatrix(as.factor(ifelse(fitted(log_model2) > .5, "1", "0")), as.factor(dataset$Target))$table

#Accuracy Comparisions
#Modelo 1
1-((763+205)/(3633+763+205+297))
#Modelo 2
1-((766+215)/(3623+766+215+294))

install.packages("pROC")
library("pROC")

invisible(plot(roc(dataset$Target,
                   fitted(log_model)),
               col = "red", 
               main = "ROC curves: logistic model 1 (red) vs. logistic model 2 (blue)"))

invisible(plot(roc(dataset$Target,
                   fitted(log_model2)),
               print.auc = T, 
               col = "blue", 
               add = T))


