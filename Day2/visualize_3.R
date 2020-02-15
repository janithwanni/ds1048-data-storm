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
  gather(PAID_AMT_JULY,PAID_AMT_AUG,PAID_AMT_SEP,PAID_AMT_OCT,PAID_AMT_NOV,PAID_AMT_DEC, key = "PAID_MONTH",value = "AMOUNT_PAID")

new02 <- data.frame(new1$NEXT_MONTH_DEFAULT , new1$PAID_MONTH , new1$AMOUNT_PAID)
ggplot(new02, aes(x = new1.PAID_MONTH , y = new1.AMOUNT_PAID , fill = new1.NEXT_MONTH_DEFAULT)) + 
  geom_boxplot()

#Analysis of due months 

new2 <- train %>%
  gather(PAY_JULY , PAY_AUG , PAY_SEP , PAY_OCT , PAY_NOV , PAY_DEC , key = "PAY_MONTH",value = "NO_MONTHS_PAY")
new03 <- data.frame(new2$NEXT_MONTH_DEFAULT,new2$PAY_MONTH,new2$NO_MONTHS_PAY)

library(ggridges)
library(ggplot2)
ggplot(new03, aes(x = new2.NO_MONTHS_PAY, y = new2.PAY_MONTH, fill = new2.NEXT_MONTH_DEFAULT)) +
  geom_density_ridges() +
  theme_ridges()

#Analysis of the percetage of credit limit vs due amount.

train$duepaid_JULY <- train$PAID_AMT_JULY/train$DUE_AMT_JULY
train$duepaid_AUG <-  train$PAID_AMT_AUG/train$DUE_AMT_AUG
train$duepaid_SEP <-  train$PAID_AMT_SEP/train$DUE_AMT_SEP
train$duepaid_OCT <-  train$PAID_AMT_OCT/train$DUE_AMT_OCT
train$duepaid_NOV <-  train$PAID_AMT_NOV/train$DUE_AMT_NOV
train$duepaid_DEC <-  train$PAID_AMT_DEC/train$DUE_AMT_DEC

new04 <- data.frame(train$duepaid_NOV,train$duepaid_OCT,train$duepaid_SEP,train$duepaid_AUG,train$duepaid_JULY,train$duepaid_DEC,train$NEXT_MONTH_DEFAULT)
new05 <- new04 %>%
  gather(train.duepaid_DEC,train.duepaid_NOV,train.duepaid_OCT,train.duepaid_SEP,train.duepaid_AUG,train.duepaid_JULY, key = "month_paid_due",value = "ratio")

new06 <- na.omit(new05)
new06$month_paid_due <- factor(new06$month_paid_due,levels=c("train.duepaid_JULY","train.duepaid_AUG","train.duepaid_SEP","train.duepaid_OCT","train.duepaid_NOV","train.duepaid_DEC"))

ggplot(new06, aes(x = month_paid_due , y = ratio , fill = train.NEXT_MONTH_DEFAULT)) + 
  geom_boxplot()

