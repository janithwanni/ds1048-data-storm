library(caret)
library(randomForest)

train_df <- read.csv("Day3/ensemble_data/train_processed.csv")
train_df$NEXT_MONTH_DEFAULT <- as.factor(train_df$NEXT_MONTH_DEFAULT)
test_df <- read.csv("Day3/ensemble_data/test_processed.csv")

train_index <- createDataPartition(train_df$NEXT_MONTH_DEFAULT, p = 0.7, list = FALSE)
train_split <- train_df[ train_index,]
validation_split  <- train_df[-train_index,]

set.seed(2017)
forest <- randomForest(NEXT_MONTH_DEFAULT ~ ., 
                       data = subset(train_split,select=-Client_ID), 
                       localImp = FALSE,
                       classwt=c(0.2,0.8))

predictions <- predict(forest,subset(validation_split,select=-c(Client_ID,NEXT_MONTH_DEFAULT)))
cm <- CrossTable(predictions,validation_split$NEXT_MONTH_DEFAULT)
F1_Score(predictions,validation_split$NEXT_MONTH_DEFAULT)

train_predictions <- predict(forest,subset(train_df,select=-c(Client_ID,NEXT_MONTH_DEFAULT)))
train_ensemble <- subset(train_df,select=Client_ID)
train_ensemble$rf_pred <- train_predictions

test_predictions <- predict(forest,subset(test_df,select=-c(Client_ID)))
test_ensemble <- subset(test_df,select=Client_ID)
test_ensemble$rf_pred <- test_predictions

write.csv(train_ensemble,"Day3/ensemble_data/rf_ensemble_train.csv",row.names = F)
write.csv(test_ensemble,"Day3/ensemble_data/rf_ensemble_test.csv",row.names = F)

