#T1 T234group
library(randomForest)
library(pROC)
library(caret)

fungi<-readRDS("/home/zyt/16s_JiangXi/model_R/T1_T234/data/fungi.rds")

#Fungi
data_model<-fungi
data_model<-data_model[,-1]
split_index <- createDataPartition(data_model$group, p = 0.8, list = FALSE)
train_data <- data_model[split_index, ]
test_data <- data_model[-split_index, ]
ctrl <- trainControl(method = "cv", number = 5,p = 0.8,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = "final")

fit_rf_cv <- train(
  group ~ ., data = train_data,method = "rf",trControl = ctrl,metric = "ROC",tuneLength = 10,ntree = 1000)
fit_rf_cv
best_mtry <- fit_rf_cv$bestTune$mtry
final_rf_model <- randomForest(
  group ~ ., data = train_data,
  ntree = 1000, mtry = best_mtry,importance = T,nodesize = 3) 
final_rf_model
predictions <- predict(final_rf_model, newdata = train_data)
conf_matrix <- confusionMatrix(predictions, train_data$group)
print(conf_matrix)

train_prob <- predict(final_rf_model, newdata = train_data, type = "prob")

fugitrain_roc <- roc(response = train_data$group, predictor = train_prob[,"Early"])
auc(fugitrain_roc)

test_pred<-predict(final_rf_model, newdata = test_data)
test_prob <- predict(final_rf_model, newdata = test_data, type = "prob")
confusionMatrix(test_pred, test_data$group)

fugitest_roc <- roc(response = test_data$group, predictor = test_prob[,"Early"])
auc(fugitrain_roc)
auc(fugitest_roc)

roc.test(test_roc,fugitest_roc,method="delong")
fungi_gini<-as.data.frame(importance(final_rf_model))
fungi_model<-final_rf_model
plot.roc(
  fugitrain_roc$response,
  fugitrain_roc$predictor,
  col = "#FF5F5F",percent = TRUE,lwd = 2,print.auc.cex = 1,
  print.auc = TRUE,print.auc.pattern = "Train(Fungi): %.1f%%",print.auc.y = 62)
plot.roc(
  fugitest_roc$response,fugitest_roc$predictor,
  col = "#fb8500", percent = TRUE,lwd = 2,print.auc.cex = 1,print.auc = TRUE,
  print.auc.pattern = "Test(Fungi): %.1f%%",print.auc.y = 38,print.auc.x = 60,add=T)









