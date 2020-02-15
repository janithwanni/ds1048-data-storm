train <- read.csv(file = "./credit_card_default_train.csv",header = TRUE)

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
new01 <- data.frame(new$Due_Month , new$Amount , new$NEXT_MONTH_DEFAULT) %>% mutate(
  new.Due_Month = factor(new.Due_Month,levels=c("DUE_AMT_JULY","DUE_AMT_AUG",
                                                "DUE_AMT_SEP","DUE_AMT_OCT",
                                                "DUE_AMT_NOV","DUE_AMT_DEC"))
)
ggplot(new01, aes(x=new.Due_Month, y=new.Amount, fill=new.NEXT_MONTH_DEFAULT)) + 
  geom_boxplot()+
  coord_cartesian(ylim = c(-500000, 800000))

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

