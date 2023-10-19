############--------------- Author: Victor Diallen ------------############

### ---- Working Directory ---- ###
setwd("C:/Users/Victor/Desktop/data_science_academy/formacao_cientista_de_dados/Big Data Analytics com R e Microsoft Azure Machine Learning/Projeto2")
getwd()



### ---- Libraries ---- ###
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyverse)
library(ggcorrplot)
library(GGally)
library(caret)
library(ROCR) 
library(e1071) 
library(class)
library(ISLR)
library(randomForest)
library(pROC)
library(mice)
library(multiROC)


### ---- Loading Dataset ---- ###
df <- read.csv('Acoustic_Extinguisher_Fire_Dataset.csv')
View(df)
str(df)
summary(df)
dim(df)
names(df)


### ---- Checking for NA Values ---- ###
sum(is.na(df))


### ---- Converting Columns Fuel and Status to Factor ---- ###
df$FUEL <- as.factor(df$FUEL)
df$STATUS <- as.factor(df$STATUS)
df$SIZE <- as.factor(df$SIZE)


### ---- Check Correlation Between Numerical Variables ---- ###
df_nums = select_if(df, is.numeric)
ggcorrplot(cor(df_nums), tl.cex = 5, lab=TRUE, lab_size = 2)


### ---- Exploratory Analysis ---- ###

# -- Bar Graphs -- #
lapply(names(df), function(x){
  if(is.factor(df[,x])) {
    ggplot(df, aes_string(x)) +
      geom_bar() + 
      facet_grid(. ~ STATUS) + 
      ggtitle(paste("STATUS by",x))}})


# -- Scatter Plots -- #
Scatter_Matrix <- ggpairs(df, columns = names(df_nums),
                          title = "Scatter Plot Matrix for mtcars Dataset",
                          axisLabels = "show")
Scatter_Matrix

ggplot(df, aes(x=DESIBEL, y=AIRFLOW)) + 
  geom_point(aes(color=STATUS), alpha=0.6) + 
  geom_smooth(method='lm') + 
  ggtitle("Relationship Between Desibel and Airflow") +
  xlab("DESIBEL") + 
  ylab("AIRFLOW")

ggplot(df, aes(x=DESIBEL, y=FREQUENCY)) + 
  geom_point(aes(color=STATUS), alpha=0.6) + 
  geom_smooth(method='lm') + 
  ggtitle("Relationship Between Desibel and Frequency") +
  xlab("DESIBEL") + 
  ylab("FREQUENCY")

ggplot(df, aes(x=AIRFLOW, y=DISTANCE)) + 
  geom_point(aes(color=STATUS), alpha=0.6) + 
  geom_smooth(method='lm') + 
  ggtitle("Relationship Between Desibel and Frequency") +
  xlab("DESIBEL") + 
  ylab("FREQUENCY")


# -- Box Plots -- #
boxplot(df_nums)


### ---- Finding the Most Important Variables Using Generalized Linear Models ---- ###
?glm
imp_variables_glm <- glm(STATUS ~ ., family = binomial(link = 'logit'), data = df)
summary(imp_variables_glm)


### ---- Finding the Most Important Variables Using Random Forest ---- ###
?randomForest
imp_variables_rf <- randomForest( STATUS ~ ., 
                        data = df, 
                        ntree = 100, nodesize = 10, importance = T)

varImpPlot(imp_variables_rf)


### ---- Scaling Numeric Variables ---- ###
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center = T, scale = T)
  }
  return(df)
}
df_scaled <- scale.features(df, names(df_nums))
View(df_scaled)

### ---- Splitting Dataset Into Train and Test ---- ###
set.seed(100)
sample <- sample(c(TRUE, FALSE), nrow(df_scaled), replace=TRUE, prob=c(0.70,0.30))
df_train <- df_scaled[sample, ]
df_test <- df_scaled[!sample, ]



### ---- Machine Learning ---- ###

# -- Generalized Linear Models -- #

levels(df_train$STATUS) <- c("0", "1")
levels(df_test$STATUS) <- c("0", "1")

model_v1_glm <- glm(STATUS ~ ., family = binomial(link = 'logit'), data = df_train)
summary(model_v1_glm)
pred_model_v1_glm <- predict(model_v1_glm, df_test, type = "response")
pred_model_v1_glm_round <- round(pred_model_v1_glm)
View(pred_model_v1_glm_round)
cm_v1_glm <- confusionMatrix(table(data = pred_model_v1_glm_round, reference = df_test$STATUS), positive = '1')
cm_v1_glm

model_v2_glm <- glm(STATUS ~ SIZE + FUEL + FREQUENCY, family = binomial(link = 'logit'), data = df_train)
summary(model_v2_glm)
pred_model_v2_glm <- predict(model_v2_glm, df_test, type = "response")
pred_model_v2_glm_round <- round(pred_model_v2_glm)
View(pred_model_v2_glm_round)
cm_v2_glm <- confusionMatrix(table(data = pred_model_v2_glm_round, reference = df_test$STATUS), positive = '1')
cm_v2_glm

model_v3_glm <- glm(STATUS ~ . -DESIBEL, family = binomial(link = 'logit'), data = df_train)
summary(model_v3_glm)
pred_model_v3_glm <- predict(model_v3_glm, df_test, type = "response")
pred_model_v3_glm_round <- round(pred_model_v3_glm)
View(pred_model_v2_glm_round)
cm_v3_glm <- confusionMatrix(table(data = pred_model_v3_glm_round, reference = df_test$STATUS), positive = '1')
cm_v3_glm

# Plot ROC Curve 
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col = "black",lty = 1, lwd = 2,
       main = title.text, cex.main = 0.6, cex.lab = 0.8,xaxs = "i", yaxs = "i")
  abline(0,1, col = "red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend = c(paste0("AUC: ",auc)), cex = 0.6, bty = "n", box.col = "white")
}

par(mfrow = c(1, 2))
final_predictions <- prediction(pred_model_v3_glm, df_test$STATUS)
plot.roc.curve(final_predictions, title.text = "Curva ROC")



# -- KNN Models (K Nearest Neighbors) -- #

levels(df_train$STATUS) <- c("not_extinguished", "extinguished")
levels(df_test$STATUS) <- c("not_extinguished", "extinguished")

ctrl <- trainControl(method = "repeatedcv", repeats = 3) 
model_v1_knn <- train(STATUS ~ ., 
                data = df_train, 
                method = "knn", 
                trControl = ctrl, 
                tuneLength = 20)
model_v1_knn
plot(model_v1_knn)
pred_model_v1_knn <- predict(model_v1_knn, newdata = df_test)
pred_model_v1_knn
cm_v1_knn <- confusionMatrix(pred_model_v1_knn, df_test$STATUS, positive = 'extinguished')
cm_v1_knn

ctrl2 <- trainControl(method = "repeatedcv", 
                     repeats = 3, 
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
model_v2_knn <- train(STATUS ~ ., 
                data = df_train, 
                method = "knn", 
                trControl = ctrl2, 
                metric = "ROC",
                tuneLength = 20)
model_v2_knn
plot(model_v2_knn)
pred_model_v2_knn <- predict(model_v2_knn, newdata = df_test)
pred_model_v2_knn
cm_v2_knn <- confusionMatrix(pred_model_v2_knn, df_test$STATUS, positive = 'extinguished')
cm_v2_knn

model_v3_knn <- train(STATUS ~ . -DESIBEL, 
                      data = df_train, 
                      method = "knn", 
                      trControl = ctrl, 
                      tuneLength = 20)
plot(model_v3_knn)
pred_model_v3_knn <- predict(model_v3_knn, newdata = df_test)
pred_model_v3_knn
cm_v3_knn <- confusionMatrix(pred_model_v3_knn, df_test$STATUS, positive = 'extinguished')
cm_v3_knn



# -- SVM Models -- #
?svm
model_v1_svm <- svm(STATUS ~ ., data = df_train)
summary(model_v1_svm)
print(model_v1_svm)
pred_model_v1_svm <- predict(model_v1_svm, newdata = df_test)
cm_v1_svm <- confusionMatrix(pred_model_v1_svm, df_test$STATUS)
cm_v1_svm

model_grid1 <- tune(svm, 
                     STATUS ~ .-DESIBEL, 
                     data = df_train, 
                     kernel = 'linear',
                     ranges = list(cost = c(0.05, 0.1, 0.5, 1, 2)))
summary(model_grid1)
model_grid1$best.parameters
model_grid1$best.model

model_v2_svm <- model_grid1$best.model 
summary(model_v2_svm)
pred_model_v2_svm <- predict(model_v2_svm, df_test)
cm_v2_svm <- confusionMatrix(pred_model_v2_svm, df_test$STATUS)
cm_v2_svm

model_grid2 <- tune(svm, 
                    STATUS ~ .-DESIBEL, 
                    data = df_train, 
                    kernel = 'linear',
                    ranges = list(cost = c(0.05, 0.1, 0.5, 1, 2)))
summary(model_grid2)
model_grid2$best.parameters
model_grid2$best.model

model_v3_svm <- model_grid2$best.model 
summary(model_v3_svm)
pred_model_v3_svm <- predict(model_v3_svm, df_test)
cm_v3_svm <- confusionMatrix(pred_model_v3_svm, df_test$STATUS)
cm_v3_svm

model_grid3 <- tune(svm, 
                    STATUS ~ .-DESIBEL, 
                    data = df_train, 
                    kernel = 'radial',
                    ranges = list(cost = c(0.05, 0.1, 0.5, 1, 2)))
summary(model_grid3)
model_grid3$best.parameters
model_grid3$best.model

model_v4_svm <- model_grid3$best.model 
summary(model_v4_svm)
pred_model_v4_svm <- predict(model_v4_svm, df_test)
cm_v4_svm <- confusionMatrix(pred_model_v4_svm, df_test$STATUS)
cm_v4_svm



### ---- Choosing Best Model ---- ###
models_comparison <- data.frame(Models = c('model_v1_glm','model_v2_glm','model_v3_glm','model_v1_knn','model_v2_knn','model_v3_knn','model_v1_svm','model_v2_svm','model_v3_svm','model_v4_svm'),
                      Accuracy = c(cm_v1_glm$overall['Accuracy'],cm_v2_glm$overall['Accuracy'],cm_v3_glm$overall['Accuracy'],cm_v1_knn$overall['Accuracy'],cm_v2_knn$overall['Accuracy'],cm_v3_knn$overall['Accuracy'], cm_v1_svm$overall['Accuracy'],cm_v2_svm$overall['Accuracy'],cm_v3_svm$overall['Accuracy'],cm_v4_svm$overall['Accuracy'])
                      )
models_comparison

final_model <- model_v3_knn


### ---- Conclusion ---- ###

## -- Comparing techniques, KNN would be chosen -- ##
## -- Accuracy was the metric chosen to evaluate the best method -- ##



### ---- Creating New Data for Predictions ---- ###

table(df$DESIBEL)

# -- New Data -- #
SIZE <- c(3, 5, 2, 2, 7, 1, 6, 1)
FUEL <- c('gasoline', 'kerosene', 'thinner', 'gasoline', 'kerosene', 'lpg', 'thinner', 'lpg')
DISTANCE <- c(10, 50, 110, 50, 140, 120, 160, 20)
DESIBEL <- c(90, 80, 112, 105, 95, 83, 90, 100)
AIRFLOW <- c(10.9, 8.5, 11.8, 2.7, 4.3, 16.1, 1.5, 6.3)
FREQUENCY <- c(20, 46, 55, 70, 72, 4, 13, 22)


# -- Create New Dataframe -- #
new_df <- data.frame(SIZE,FUEL, DISTANCE, DESIBEL, AIRFLOW, FREQUENCY)
View(new_df)
new_df$FUEL <- as.factor(new_df$FUEL)
new_df$SIZE <- as.factor(new_df$SIZE)
new_df_nums <- select_if(new_df, is.numeric)
new_df_scaled <- scale.features(new_df, names(new_df_nums))

# -- Predictions -- #
?predict
pred_new_df <- predict(final_model, newdata = new_df_scaled, type = "prob")
pred_new_df