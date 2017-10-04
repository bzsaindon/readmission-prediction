################################################################################################################
################################################################################################################
#   Code: model-build-xgboost.R
#   Purpose: the purpose of this code is to:
#               1) build a logistic regression model using xgboost
#   DS: Brian Saindon
#   Date: 10/1/2017
#   Input: model.data
#   Output: 
################################################################################################################
################################################################################################################
library(ROCR)
require(caret)
require(dplyr)
require(xgboost)
require(Matrix)
require(data.table)
if (!require('vcd')) install.packages('vcd')


model.data <- readRDS(file = "../derived_variables/model.data.rds")
model.data.reduced <-  model.data[ c(3, 5, 7, 9, 11, 14, 15, 18, 19, 35, 41, 43, 44, 51, 56, 57, 58, 61, 63, 64, 65, 66, 70, 72, 73, 75, 76, 80, 81, 82, 84, 85, 88, 89, 90, 91, 92, 93, 95, 96, 101, 103, 105, 107, 108, 109, 113, 114, 118, 119, 121
) ]
model.data.reduced[is.na(model.data.reduced)] <- 0

set.seed(3456)
trainIndex <- createDataPartition(model.data.reduced$readmitted_dv, p = .80,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)
model1train <- model.data.reduced[ trainIndex,]
model1test  <- model.data.reduced[-trainIndex,]

# Convert to Data Frame to Matrix

sparse_matrix_train <- sparse.model.matrix(readmitted_dv~.-1, data = model1train)
output_vector_train = model1train[,"readmitted_dv"] == 1

sparse_matrix_test <- sparse.model.matrix(readmitted_dv~.-1, data = model1test)
output_vector_test = model1test[,"readmitted_dv"] == 1

sparse_matrix_all_data <- sparse.model.matrix(readmitted_dv~.-1, data = model.data.reduced)
output_vector_all_data = model.data.reduced[,"readmitted_dv"] == 1


##### Model 2 Random Forest Like Model - OK
#Boosting - Best Fit AUC = .712; rounds = 49; max depth = 4
bst <- xgboost(data = sparse_matrix_train, 
               label = output_vector_train, 
               max_depth = 4, 
               nrounds = 49 , 
               objective = "binary:logistic",
               eval.metric = "auc")

# Compute AUC for predicting Class with the model
prob <- predict(bst, sparse_matrix_test)#predict(logit.overall1, newdata=model1test, type="response")
pred <- prediction(prob, model1test$readmitted_dv )
perftrain <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perftrain)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

xgb.save(bst,"../results/model/xgb.model")

##### Model Evaluation Using Cross Validation

#Boosting - Best Fit AUC =  rounds = 49; max depth = 4
cv <- xgb.cv(data = sparse_matrix_all_data, 
              label = output_vector_all_data, 
              max_depth = 4, 
              nfold = 10,
              nrounds = 49 ,
              objective = "binary:logistic",
              metrics = list("auc"))
print(cv, verbose=TRUE)




