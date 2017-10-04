################################################################################################################
################################################################################################################
#   Code: model-score.R
#   Purpose: the purpose of this code is to:
#               1) score and evaluate logistic regression and xgboost models
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
###### Data Prep
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

# Call XGBoost Model
# load binary model to R

bst2 <- xgb.load("../results/model/xgb.model")
bst_predictions <- predict(bst2, sparse_matrix_test)

lr <- readRDS("../results/model/lr-model.rds")
lr_predictions <- predict(lr, newdata=model1test, type="response")

df<-data.frame(cbind(bst_predictions, lr_predictions, model1test$readmitted_dv))
colnames(df) <- c("xgb_pred", "lr_pred", "dv")
df <- df %>% mutate(decile_lr = ntile(lr_pred, 10), decile_xgb = ntile(xgb_pred, 10))
df$avg_prob<- (df$lr_pred +  df$xgb_pred)/2


table(df$dv)
1252/12746
# DV = .0982 (9.8%)
grp_lr <- group_by(df, decile_lr)
average_pobability_lr<-summarise(grp_lr, meanrisk_lr=round(mean(lr_pred)*100, 2), meanactual=round(mean(dv)*100, 2))

grp_xgb <- group_by(df, decile_xgb)
average_pobability_xgb<-summarise(grp_xgb, meanrisk_xgb=round(mean(xgb_pred)*100, 2), meanactual=round(mean(dv)*100, 2))

cross_tab<-with(df, tapply(dv, list(decile_lr=decile_lr,decile_xgb=decile_xgb), mean) )
write.csv(cross_tab, "../results/scored/cross_tab.csv")
write.csv(average_pobability_lr, "../results/scored/avg_prob_lr.csv")
write.csv(average_pobability_xgb, "../results/scored/avg_prob_xgb.csv")
