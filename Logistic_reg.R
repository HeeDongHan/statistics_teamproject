rm(list=ls())
setwd('C:\\Users\\ASUS\\OneDrive\\바탕 화면\\R_HW')

library(tidyverse)
library(readxl)
library(labelled)
library(gridExtra)
library(caret)
library(ModelMetrics)
library(car)

df = read_csv('framingham.csv')
df

nrow(df)

################################## whole feature(15) ###########################

## check column
colnames(df1)

## delete NA
df1 = df %>%
  drop_na()

## check # of obs
nrow(df1)

## Logistic regression
model = glm(TenYearCHD ~., data=df1, family = 'binomial')
summary(model)
coef(model)

## odd ratio
exp(coef(model))

## estimated probability
pred = predict(object = model, newdata = df1,
               type = 'response')
round(pred, 4)


## Threshold = 0.5
ifelse(pred >=0.5 , 1, 0)

pred_class = ifelse(pred >=0.5 , 1, 0)
pred_class


## confusion Matrix (Threshold = 0.5)
# library(caret)
confusionMatrix(table(df1$TenYearCHD, pred_class)) ## table로 input

## ROC curve
# library(ModelMetrics)
auc(df1$TenYearCHD, pred_class) ## auc = 0.5427


########################## variable selection ##################################

## full model
model = glm(TenYearCHD ~., data=df1, family = 'binomial')
summary(model) ## AIC 2786.2


## stepwise AIC
model_AIC = step(model, direction = 'both')
# model_AIC = glm(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol +sysBP + glucose, data= df1, family = 'binomial')
summary(model_AIC) ## AIC 2775.2

## check vif
# library(car)
vif(model_AIC)


## stepwise BIC
n = nrow(df1)
model_BIC = step(model, trace = 0, direction = 'both', k=log(n))
# model_BIC = glm(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol +sysBP + glucose, data= df1, family = 'binomial')
summary(model1) ## AIC 2775.2


## -> AIC and BIC slect same vaiables

######################### final model  ############################
## final model
model_final = glm(TenYearCHD ~ male + age + cigsPerDay +
                  totChol +sysBP + glucose, data= df1, family = 'binomial')


summary(model_final)

vif(model_final)
## odd ratio
exp(coef(model_final))

## estimated probability
pred = predict(object = model_final, newdata = df1,
               type = 'response')
round(pred, 4)


## Threshold = 0.5
ifelse(pred >=0.5 , 1, 0)

pred_class = ifelse(pred >=0.5 , 1, 0)
pred_class

## confusion Matrix (Threshold = 0.5)
# library(caret)
confusionMatrix(table(df1$TenYearCHD, pred_class)) ## table로 input

## ROC curve
# library(ModelMetrics)
auc(df1$TenYearCHD, pred_class) # 0.5339


############################## holdout 8:2 & final model #######################

set.seed(999)
train_idx = sample(1:nrow(df1), size= round(nrow(df1)*0.8)) ## round, floor, ceiling
df1_train = df1[train_idx, ]
df1_test = df1[-train_idx, ]
print(nrow(df1_train))
print(nrow(df1_test))
head(df1_train, 2)

model_HD = glm(TenYearCHD ~ male + age + cigsPerDay +
                 totChol +sysBP + glucose, data= df1_train, family = 'binomial')
summary(model_HD) ## AIC 2203.5

## odd ratio
exp(coef(model_HD))

## estimated probability
pred = predict(object = model_HD, newdata = df1_test,
               type = 'response')
round(pred, 4)


## Threshold = 0.5
ifelse(pred >=0.5 , 1, 0)

pred_class = ifelse(pred >=0.5 , 1, 0)
pred_class

## confusion Matrix (Threshold = 0.5)
# library(caret)
confusionMatrix(table(df1_test$TenYearCHD, pred_class)) ## table로 input

## ROC curve
# library(ModelMetrics)
auc(df1_test$TenYearCHD, pred_class) ## auc = 0.5475

######################################### RESULTS ##############################

####.1 variable selection 
#-> TenYearCHD ~ male + age + cigsPerDay + totChol +sysBP + glucose


####.2 model selection
#-> models with the lowest AIC and BIC  - variables (prevalentStroke + prevalentHyp)
# because these variables were not statistically significant below 0.05 

####.3 interpretation
## odd ratio
exp(coef(model_final))
# results in order of greatest impact on CHD
## male >> age > cigsPErday > sysBP > glucose > totolChol 
## male is critical variables on CHD
## the others are minor variables on CHD

####.4 AUC value comparison
## whole models have similar auc value -> 0.54 
## Low performance? ?
