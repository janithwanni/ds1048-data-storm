library(caret)
library(e1071)

train_df <- read.csv("Day3/ensemble_data/train_processed.csv")
train_df$NEXT_MONTH_DEFAULT <- as.factor(train_df$NEXT_MONTH_DEFAULT)
test_df <- read.csv("Day3/ensemble_data/test_processed.csv")

train_index <- createDataPartition(train_df$NEXT_MONTH_DEFAULT, p = 0.7, list = FALSE)
train_split <- train_df[ train_index,]
validation_split  <- train_df[-train_index,]

svmodel_best <- svm(NEXT_MONTH_DEFAULT ~ .,
                    data=subset(train_split,select=-c(Client_ID)), 
                    type="C-classification",kernel="radial",
                    cost=4, 
                    gamma=1,class.weights=c("0" = 0.30,"1" = 0.70))

predictions <- predict(svmodel_best,subset(validation_split,select=-c(Client_ID,NEXT_MONTH_DEFAULT)))
cm <- CrossTable(predictions,validation_split$NEXT_MONTH_DEFAULT)
F1_Score(predictions,validation_split$NEXT_MONTH_DEFAULT)

train_predictions <- predict(svmodel_best,subset(train_df,select=-c(Client_ID,NEXT_MONTH_DEFAULT)))
# train_predictions <- ifelse(train_predictions >= 0.5,1,0)
train_ensemble <- subset(train_df,select=Client_ID)
train_ensemble$svm_pred <- train_predictions

test_predictions <- predict(svmodel_best,subset(test_df,select=-c(Client_ID)))
# test_predictions <- ifelse(test_predictions >= 0.5,1,0)
test_ensemble <- subset(test_df,select=Client_ID)
test_ensemble$svm_pred <- test_predictions

write.csv(train_ensemble,"Day3/ensemble_data/svm_ensemble_train.csv",row.names = F)
write.csv(test_ensemble,"Day3/ensemble_data/svm_ensemble_test.csv",row.names = F)

