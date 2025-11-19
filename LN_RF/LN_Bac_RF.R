library(randomForest)
library(pROC)
library(caret)

bacteria<-readRDS("/home/zyt/16s_JiangXi/model_R/LN_positive_negative/data/bacteria.rds")

#fungi_bacteria
set.seed(123)

#Bac
data_model<-bacteria
data_model<-data_model[,-1]
split_index <- createDataPartition(data_model$group, p = 0.8, list = FALSE)
train_data <- data_model[split_index, ]
test_data <- data_model[-split_index, ]
##
ctrl <- trainControl(method = "cv", number = 5,p = 0.8,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = "final")

fit_rf_cv <- train(
  group ~ ., data = train_data,method = "rf",trControl = ctrl,metric = "ROC",tuneLength = 10,ntree = 1000)
bac_fit_rf_cv<-fit_rf_cv
best_mtry <- fit_rf_cv$bestTune$mtry
final_rf_model <- randomForest(
  group ~ ., data = train_data,
  ntree = 1000, mtry = best_mtry,importance = T,nodesize = 3) # 
final_rf_model
predictions <- predict(final_rf_model, newdata = train_data)
conf_matrix <- confusionMatrix(predictions, train_data$group)
print(conf_matrix)

train_prob <- predict(final_rf_model, newdata = train_data, type = "prob")

bactrain_roc <- roc(response = train_data$group, predictor = train_prob[,"postive"])
auc(bactrain_roc)

test_pred<-predict(final_rf_model, newdata = test_data)
test_prob <- predict(final_rf_model, newdata = test_data, type = "prob")
confusionMatrix(test_pred, test_data$group)
bactest_roc <- roc(response = test_data$group, predictor = test_prob[,"postive"])
auc(bactrain_roc)
auc(bactest_roc)

#plot
plot.roc(
  bactrain_roc$response,bactrain_roc$predictor,col = "#E06681",
  percent = TRUE,lwd = 2,print.auc.cex = 1,
  print.auc = TRUE,print.auc.pattern = "Train(Bac): %.1f%%",
  print.auc.y = 54, print.auc.x = 60,add=T)

plot.roc(
  bactest_roc$response,bactest_roc$predictor,
  col = "#2694ab",percent = TRUE,lwd = 2,
  print.auc.cex = 1,print.auc = TRUE,
  print.auc.pattern = "Test(Bac): %.1f%%",print.auc.y = 30,print.auc.x = 60,add=T)
