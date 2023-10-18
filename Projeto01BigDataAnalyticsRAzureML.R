### ---- Working Directory ---- ###
setwd("C:/Users/Victor/Desktop/data_science_academy/formacao_cientista_de_dados/Big Data Analytics com R e Microsoft Azure Machine Learning/Projeto1")
getwd()

### ---- Libraries ---- ###
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyverse)
library(ggcorrplot)
library(caret)
library(neuralnet)
library(rpart)
library(rpart.plot)
library(RWeka)
library(kernlab)


### ---- Creating Dataframe ---- ###
df <- read.csv('dataset.csv')
View(df)
str(df)
summary(df)
dim(df)
is.na(df)
names(df)


### ---- Changing Columns Names ---- ###
columns <- c('CarName', 'Brand', 'Model', 'MinimalPrice',
             'EnginePower', 'MaximumTorque', 'TypeBrakes',
             'DriveType', 'BatteryCapacity', 'RangeWLTP', 'Wheelbase',
             'Length','Width', 'Height', 'MinEmptyWeight',
             'PermissableGrossWeight', 'MaxLoadCapacity', 'NumberSeats', 'NumberDoors',
             'TireSize', 'MaxSpeed', 'BootCapacity(VDAl)',
             'Acceleration', 'MaxDCChargingPower',
             'MeanEnergyConsumption')
names(df) <- columns
names(df)
View(df)


### ---- Checking for NA Values ---- ###
### ---- Dealing with NA Values (imputation method was chosen due to low volume of data ) ---- ###
### ---- Selecting Numeric Columns ---- ###
colSums(is.na(df))
df_nums = select_if(df, is.numeric)
View(df_nums)

summary(df$PermissableGrossWeight)
df_nums$PermissableGrossWeight[is.na(df$PermissableGrossWeight)] <- median(df$PermissableGrossWeight, na.rm=TRUE)
df_nums$MaxLoadCapacity[is.na(df$MaxLoadCapacity)] <- median(df$MaxLoadCapacity, na.rm=TRUE)
df_nums$Acceleration[is.na(df$Acceleration)] <- median(df$Acceleration, na.rm=TRUE)
df_nums$BootCapacity[is.na(df$BootCapacity)] <- median(df$BootCapacity, na.rm=TRUE)
df_nums$MeanEnergyConsumption[is.na(df$MeanEnergyConsumption)] <- median(df$MeanEnergyConsumption, na.rm=TRUE)
sum(is.na(df_nums))


### ---- Checking Correlation Between Variables ---- ###
ggcorrplot(cor(df_nums), tl.cex = 5, lab=TRUE, lab_size = 2)


### ---- Scaling Predictor Variables ---- ###
dim(df_nums)
X = df_nums[c(1:19)]
y = df_nums[20]
X_scaled = scale(X)
X_scaled
df_v1 = cbind(X_scaled, y)
View(df_v1)


### ---- Linear Regression Models ---- ###
model_v1 <- lm(MeanEnergyConsumption ~ ., data = df_v1)
summary(model_v1)

    ## -- Choosing Relevant Columns According to model_v1 Summary -- ## 
columns_relevant = c('BatteryCapacity', 'RangeWLTP', 'Wheelbase',
                     'Length', 'Height', 'PermissableGrossWeight',
                     'NumberSeats', 'TireSize', 'Acceleration',
                     'MeanEnergyConsumption')

df_v1_reduced = df_v1[ , names(df_v1) %in% columns_relevant]
names(df_v1_reduced)

ggcorrplot(cor(df_v1_reduced), tl.cex = 8, lab=TRUE, lab_size = 2)

    ## -- Dropping Low Correlation Columns -- ##
columns_low_corr = c('RangeWLTP', 'NumberSeats')

df_v1_reduced2 = df_v1_reduced[ , !names(df_v1_reduced) %in% columns_low_corr]
names(df_v1_reduced2)

ggcorrplot(cor(df_v1_reduced2), tl.cex = 5, lab=TRUE, lab_size = 2)

model_v2 <- lm(MeanEnergyConsumption ~ ., data = df_v1_reduced2)
summary(model_v2)

    ## -- Choosing Relevant Columns According to model_v2 Summary -- ##
columns_relevant2 = c('PermissableGrossWeight', 'TireSize',
                      'Acceleration', 'MeanEnergyConsumption')

df_final = df_v1_reduced2[ , names(df_v1_reduced2) %in% columns_relevant2]
names(df_final)

    ## -- Splitting Dataset Into Train and Test -- ##
set.seed(100)
sample <- sample(c(TRUE, FALSE), nrow(df_final), replace=TRUE, prob=c(0.85,0.15))
df_train <- df_final[sample, ]
df_test <- df_final[!sample, ]

ggcorrplot(cor(df_final), tl.cex = 5, lab=TRUE, lab_size = 2)

model_v3 <- lm(MeanEnergyConsumption ~ . , data = df_final)
summary(model_v3)

model_v4 <- lm(MeanEnergyConsumption ~ PermissableGrossWeight + TireSize, data = df_final)
summary(model_v4)

    ## -- Choosing model_v3 Variables Since it had Better Performance and Creating model_v5 to Use Train and Test Datasets -- ##
model_v5 <- lm(MeanEnergyConsumption ~ ., data = df_train)
pred_model_v5 <- predict(model_v5, df_test)

    ## -- Comparing Results and Metrics of model_v5 -- ##
comparing_results <- cbind(df_test$MeanEnergyConsumption, pred_model_v5)
comparing_results
cor_lm_model <- cor(df_test$MeanEnergyConsumption, pred_model_v5)
rmse_lm_model <- RMSE(pred_model_v5, df_test$MeanEnergyConsumption)
mae_lm_model <- MAE(pred_model_v5, df_test$MeanEnergyConsumption)


### ---- Neural Networks Models ---- ###

?neuralnet
model_v1_neuralnet <- neuralnet(MeanEnergyConsumption ~ ., data = df_train, hidden = 1)
str(model_v1_neuralnet)
plot(model_v1_neuralnet)

model_v1_neuralnet$result.matrix

pred <- predict(model_v1_neuralnet, df_test)

    ## -- Comparing Results and Metrics of model_v1_neuralnet -- ##
comparing_results <- cbind(df_test$MeanEnergyConsumption, pred)
comparing_results
cor_neuralnet_model <- cor(df_test$MeanEnergyConsumption, pred)
rmse_neuralnet_model <- RMSE(pred, df_test$MeanEnergyConsumption)
mae_neuralnet_model <- MAE(pred, df_test$MeanEnergyConsumption)


### ---- Regression Trees Models ---- ###

?rpart
model_v1_rpart <- rpart(MeanEnergyConsumption ~ ., data = df_train)
pred_model_v1_rpart <- predict(model_v1_rpart, df_test, type = 'vector')
summary(pred_model_v1_rpart)
summary(df_test$MeanEnergyConsumption)

    ## -- Comparing Results and Metrics of model_v1_rpart -- ##
cor_rpart_model <- cor(pred_model_v1_rpart, df_test$MeanEnergyConsumption)
rmse_rpart_model <- RMSE(pred_model_v1_rpart, df_test$MeanEnergyConsumption)
mae_rpart_model <- MAE(pred_model_v1_rpart, df_test$MeanEnergyConsumption)
rpart.plot(model_v1_rpart, digits = 3)


### ---- M5 Regression Trees Models ---- ###

model_v1_m5 <- M5P(MeanEnergyConsumption ~ ., data = df_train)
pred_model_v1_m5 <- predict(model_v1_m5, df_test)

    ## -- Comparing Results and Metrics of model_v1_rpart -- ##
cor_m5_model <- cor(pred_model_v1_m5, df_test$MeanEnergyConsumption)
rmse_m5_model <- RMSE(pred_model_v1_m5, df_test$MeanEnergyConsumption)
mae_m5_model <- MAE(pred_model_v1_m5, df_test$MeanEnergyConsumption)


### ---- Support Vector Machine (Regression) Models ---- ###

model_v1_svm <- ksvm(MeanEnergyConsumption ~ ., data = df_train,
                     kernel = 'rbfdot', C=1)
pred_model_v1_svm <- predict(model_v1_svm, df_test)

    ## -- Comparing Results and Metrics of model_v1_rpart -- ##
cor_svm_model <- cor(pred_model_v1_svm, df_test$MeanEnergyConsumption)
rmse_svm_model <- RMSE(pred_model_v1_svm, df_test$MeanEnergyConsumption)
mae_svm_model <- MAE(pred_model_v1_svm, df_test$MeanEnergyConsumption)


### ---- Comparing Models Performances ---- ###

models_comparison <- data.frame(Correlation = c(cor_lm_model, cor_neuralnet_model, cor_rpart_model, cor_m5_model, cor_svm_model),
                                RMSE = c(rmse_lm_model, rmse_neuralnet_model, rmse_rpart_model, rmse_m5_model, rmse_svm_model),
                                MAE = c(mae_lm_model, mae_neuralnet_model, mae_rpart_model, mae_m5_model, mae_svm_model)
                                )

rownames(models_comparison) = c('Linear Regression Model', 'Neural Network Model', 'Regression Tree Model', 'M5 Regression Tree Model', 'Support Vector Machine Model')

models_comparison


### ---- Conclusion ---- ###

 ## -- Comparing techniques, Support Vector Machine would be chosen -- ##
 ## -- RMSE and MAE were the metrics chosen to evaluate the best method -- ##
