set.seed(2020)
df<-data.frame(
  pay_1 = factor(rbinom(5,10,0.6),levels = seq(0,10)),
  pay_2 = factor(rbinom(5,10,0.2),levels = seq(0,10)),
  pay_3 = factor(rbinom(5,10,0.4),levels = seq(0,10)),
  paid_1 = abs(rnorm(5,10,4)),
  paid_2 = abs(rnorm(5,10,8)),
  paid_3 = abs(rnorm(5,10,2)),
  response = as.factor(as.numeric(rbernoulli(5,0.5)))
)
df
df <- df %>%
  gather(pay_key,pay_value,c("pay_1","pay_2","pay_3"))%>%
  gather(paid_key,paid_value,c("paid_1","paid_2","paid_3"))

ggplot(df,aes(x=pay_value,y=paid_value,fill=response))+
  geom_bar(stat="identity",position="dodge")+
  facet_grid(pay_key ~ .)
