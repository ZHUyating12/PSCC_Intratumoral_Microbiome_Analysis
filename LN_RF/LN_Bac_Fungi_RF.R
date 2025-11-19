library(randomForest)
library(pROC)
library(caret)

fungi_bacteria<-readRDS("/home/zyt/16s_JiangXi/model_R/LN_positive_negative/data/fungi_bacteria.rds")

#fungi_bacteria
set.seed(123)
data_model<-fungi_bacteria
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
  group ~ ., data = train_data,method = "rf",trControl = ctrl,metric = "ROC",tuneLength = 10)
bac_fungi_fit_rf_cv<-fit_rf_cv

final_rf_model <- randomForest(
  group ~ ., data = train_data,
  ntree = 1000, mtry = 2,importance = T,nodesize = 3) # 
final_rf_model
predictions <- predict(final_rf_model, newdata = train_data)
conf_matrix <- confusionMatrix(predictions, train_data$group)
print(conf_matrix)

train_prob <- predict(final_rf_model, newdata = train_data, type = "prob")

train_roc <- roc(response = train_data$group, predictor = train_prob[,"postive"])
auc(train_roc)

test_pred<-predict(final_rf_model, newdata = test_data)
test_prob <- predict(final_rf_model, newdata = test_data, type = "prob")
confusionMatrix(test_pred, test_data$group)

test_roc <- roc(response = test_data$group, predictor = test_prob[,"postive"])
auc(train_roc)
auc(test_roc)

##plot 
plot.roc(
  train_roc$response,train_roc$predictor,col = "#A31621",percent = TRUE,
  lwd = 2,print.auc.cex = 1,print.auc = TRUE,
  print.auc.pattern = "Train(Fungi + Bac): %.1f%%",print.auc.y = 70,print.auc.x = 60)
plot.roc(
  test_roc$response,test_roc$predictor,col = "#8134af",
  percent = TRUE,lwd = 2,print.auc.cex = 1,
  print.auc = TRUE,print.auc.pattern = "Test(Fungi + Bac): %.1f%%",
  print.auc.y = 46, print.auc.x = 60,add=T)




