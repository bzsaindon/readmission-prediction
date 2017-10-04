################################################################################################################
################################################################################################################
#   Code: model-build-logistic-regression-stepwise.R
#   Purpose: the purpose of this code is to:
#               1) build a logistic regression model 
#               2) identify significant predictors using stepwise
#   DS: Brian Saindon
#   Date: 10/1/2017
#   Input: model.data
#   Output: 
################################################################################################################
################################################################################################################

require(caret)
require(dplyr)

################################################################################################################
#   Data Read In

model.data <- readRDS(file = "../derived_variables/model.data.rds")

# Model Vars:
colnames(model.data)
# 2 - 3, 43: Demo (race, gender)
# 4 - 8: Admission / Discharge Info
# 9 - 15: Historical Info
# 16, 17, 42: Glucose Lab info
# 18 - 41: drug information
# 46 - 114: HCC conditions
# 115 - 122: DX groups according to paper
# 44 is dependent variable: readmitted_dv


### ### ### ### ### ### ### ### ### ### ### 
### Model 1 - LR with race, gender, age ### 
### ### ### ### ### ### ### ### ### ### ### 

model.data.reduced <-  model.data[ c(2, 3, 43, 44) ]
set.seed(3456)
trainIndex <- createDataPartition(model.data.reduced$readmitted_dv, p = .80,
                                  list = FALSE,
                                  times = 1)
model1train <- model.data.reduced[ trainIndex,]
model1test  <- model.data.reduced[-trainIndex,]
table(model.data.reduced$readmitted_dv)
3469/66521
#0.05 response
logit.overall = glm(readmitted_dv ~ . ,
                    family = "binomial",
                    data = model1test)

summary(logit.overall)
logit.overall1 <- step(logit.overall)
summary(logit.overall1)
# Model 1 Sig vars:
# 1. gender, age group


### ### ### ### ### ### ### ### ### ### ### 
# Model 2 LR with Admission & Discharge Info
### ### ### ### ### ### ### ### ### ### ### 

model.data.reduced <-  model.data[ c(4:8, 44) ]

set.seed(3456)
trainIndex <- createDataPartition(model.data.reduced$readmitted_dv, p = .80,
                                  list = FALSE,
                                  times = 1)
model1train <- model.data.reduced[ trainIndex,]
model1test  <- model.data.reduced[-trainIndex,]
table(model.data.reduced$readmitted_dv)
3469/66521
#0.05 response
logit.overall = glm(readmitted_dv ~ . ,
                    family = "binomial",
                    data = model1test)

summary(logit.overall)
logit.overall1 <- step(logit.overall)
summary(logit.overall1)

## Model 2 Sig vars:
# 1. discharge_disposition_id
# 2. time_in_hospital
# Notes: Specialty has many categories - consider reducing categories

### ### ### ### ### ### ### ### ### ### ### 
# Model 3 LR - Historical Information
### ### ### ### ### ### ### ### ### ### ### 

model.data.reduced <-  model.data[ c(9:15, 44) ]

set.seed(3456)
trainIndex <- createDataPartition(model.data.reduced$readmitted_dv, p = .80,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)
model1train <- model.data.reduced[ trainIndex,]
model1test  <- model.data.reduced[-trainIndex,]

logit.overall = glm(readmitted_dv ~ . ,
                    family = "binomial",
                    data = model1test)

summary(logit.overall)
logit.overall1 <- step(logit.overall)
summary(logit.overall1)

# Historical Info Important Vars:
# 1. num_lab_procedures
# 2. num_medications
# 3. number_inpatient
# 4. number_diagnoses

### ### ### ### ### ### ### ### ### ### ### 
# Model 4 LR - Drug Info
### ### ### ### ### ### ### ### ### ### ### 


model.data.reduced <-  model.data[ c(18:41, 44) ]

set.seed(3456)
trainIndex <- createDataPartition(model.data.reduced$readmitted_dv, p = .80,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)
model1train <- model.data.reduced[ trainIndex,]
model1test  <- model.data.reduced[-trainIndex,]
table(model.data.reduced$readmitted_dv)
3469/66521
#0.05 response
logit.overall = glm(readmitted_dv ~ . ,
                    family = "binomial",
                    data = model1test)

summary(logit.overall)
logit.overall1 <- step(logit.overall)
summary(logit.overall1)

# Model 4 Sig variables
# 1. metformin
# 2. repaglinide
# 3. insulin
# 4. change

### ### ### ### ### ### ### ### ### ### ### 
# Model 5 LR - HCC Conditions
### ### ### ### ### ### ### ### ### ### ### 


model.data.reduced <-  model.data[ c(46:114, 44) ]

set.seed(3456)
trainIndex <- createDataPartition(model.data.reduced$readmitted_dv, p = .80,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)
model1train <- model.data.reduced[ trainIndex,]
model1test  <- model.data.reduced[-trainIndex,]
table(model.data.reduced$readmitted_dv)
3469/66521
#0.05 response
logit.overall = glm(readmitted_dv ~ . ,
                    family = "binomial",
                    data = model1test)

summary(logit.overall)
logit.overall1 <- step(logit.overall)
summary(logit.overall1)

### Sig Predictors HCCs
# x6
# x7
# x11
# x12
# x13
# x16
# x18
# x19
# x20
# x21
# x25
# x27
# x28
# x30
# x31
# x35
# x36
# x37
# x39
# x40
# x43
# x44
# x45
# x46
# x47
# x48
# x50
# x51
# x56
# x58
# x60
# x62
# x63
# x64
# x68
# x69


### ### ### ### ### ### ### ### ### ### ### 
# Model 6 LR - DX Groups 
### ### ### ### ### ### ### ### ### ### ### 

model.data.reduced <-  model.data[ c(115:122, 44) ]
model.data.reduced[is.na(model.data.reduced)] <- 0

set.seed(3456)
trainIndex <- createDataPartition(model.data.reduced$readmitted_dv, p = .80,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)
model1train <- model.data.reduced[ trainIndex,]
model1test  <- model.data.reduced[-trainIndex,]
table(model.data.reduced$readmitted_dv)
3469/66521
#0.05 response
logit.overall = glm(readmitted_dv ~ . ,
                    family = "binomial",
                    data = model1test)

summary(logit.overall)
logit.overall1 <- step(logit.overall)
summary(logit.overall1)
### Sig Variables in Model 5
# dx_group_diabetes
# dx_group_injury
# dx_group_neoplasms

column_names<-colnames(model.data)
write.csv(column_names, "column_names.csv")


#### Model 7: Keep only the significant variables from previous models and apply stepwise:


model.data.reduced <-  model.data[ c(3, 5, 7, 9, 11, 14, 15, 18, 19, 35, 41, 43, 44, 51, 56, 57, 58, 61, 63, 64, 65, 66, 70, 72, 73, 75, 76, 80, 81, 82, 84, 85, 88, 89, 90, 91, 92, 93, 95, 96, 101, 103, 105, 107, 108, 109, 113, 114, 118, 119, 121
) ]
model.data.reduced[is.na(model.data.reduced)] <- 0

set.seed(3456)
trainIndex <- createDataPartition(model.data.reduced$readmitted_dv, p = .80,
                                  list = FALSE,
                                  times = 1)

model1train <- model.data.reduced[ trainIndex,]
model1test  <- model.data.reduced[-trainIndex,]
table(model.data.reduced$readmitted_dv)

logit.overall = glm(readmitted_dv ~ . ,
                    family = "binomial",
                    data = model1test)

summary(logit.overall)
logit.overall1 <- step(logit.overall)
summary(logit.overall1)

# time in hospital (3), metformin(7) and x28(21) lost sig in final model
### Model Evaluation

#Residual plot for logistic regression with an added loess smoother; we would
#hope that, on average, the residual values are 0.
scatter.smooth(logit.overall1$fit,
               residuals(logit.overall1, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of HCC-19 Model")
abline(h = 0, lty = 2)

library(ROCR)
# Compute AUC for predicting Class with the model
prob <- predict(logit.overall1, newdata=model1test, type="response")
pred <- prediction(prob, model1test$readmitted_dv )
perftrain <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perftrain)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
#C-stat = .72 on test data - this is OK

saveRDS(logit.overall1, "../results/model/lr-model.rds")
             



