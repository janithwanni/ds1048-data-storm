train <- read.csv(file = 'F:/datastorm/credit_card_default_train.csv/credit_card_default_train.csv',header = TRUE)

dim(train)
train = na.omit(train)
dim(train)
names(train)

print(class(train$Balance_Limit_V1))
print(class(train$NEXT_MONTH_DEFAULT))

train$NEXT_MONTH_DEFAULT <- as.factor(train$NEXT_MONTH_DEFAULT)

summary(train)

group0 <- subset.data.frame(train,train$NEXT_MONTH_DEFAULT== 0)
group1 <- subset.data.frame(train,train$NEXT_MONTH_DEFAULT== 1)

library(ggplot2)
library(tidyverse)

new <- train %>% 
  gather(DUE_AMT_JULY , DUE_AMT_AUG , DUE_AMT_SEP , DUE_AMT_OCT , DUE_AMT_NOV , DUE_AMT_DEC , key = "Due_Month" , 
         value = "Amount")
new01 <- data.frame(new$Due_Month , new$Amount , new$NEXT_MONTH_DEFAULT)
ggplot(new01, aes(x=new.Due_Month, y=new.Amount, fill=new.NEXT_MONTH_DEFAULT)) + geom_boxplot()

new1 <- train %>%
  gather(PAY_JULY , PAY_AUG , PAY_SEP , PAY_OCT , PAY_NOV , PAY_DEC , key = "PAY_MONTH",value = "AMOUNT_PAY")
new02 <- data.frame(new1$NEXT_MONTH_DEFAULT , new1$PAY_MONTH , new1$AMOUNT_PAY)
ggplot(new02, aes(x = new1.PAY_MONTH , y = new1.AMOUNT_PAY , fill = new1.NEXT_MONTH_DEFAULT)) + 
  geom_boxplot()