germandata <-load("C:/Users/Ann/Desktop/R final project/germandata.rda")
summary(german)
install.packages("dplyr")

library(ggplot2)
#性別與婚姻狀態(sex)
ggplot(german, aes(sex, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs(y='人數', title='性別與婚姻狀態(sex)')

#age
ggplot(german) +
  geom_boxplot(aes(x = response, y = age)) +
  labs( title = "年齡與信用好壞的關係", x = "信用", y = "年齡" )

#foreigner
ggplot(german, aes(foreign, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs( title = "是否為外籍", x = "外籍", y = "人數" )

#purpose
ggplot(german, aes(purpose, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs( title = "貸款目的", x = "目的", y = "人數" )

#duration
ggplot(german) +
  geom_boxplot(aes(x = response, y = duration)) +
  labs( title = "貸款期間", x = "信用", y = "期間" )

#amount
ggplot(german, aes(x=amount)) + geom_density(aes(group=response, colour = response, fill=response), alpha=0.3)+
  labs( title = "貸款總額", x = "總額")

#n_credits
ggplot(german, aes(n_credits, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs( title = "在該行信貸數量", x = "數量", y = "人數" )

#credit_his
ggplot(german, aes(credit_his, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs( title = "信貸紀錄", y = "人數" )

#installment_rate
ggplot(german, aes(installment_rate, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs( title = "分期費率在可支配收入比", x= "費率比", y = "人數")

#ckt_account
ggplot(german, aes(chk_acct, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs( title = "支票帳戶使用情形", y = "人數")

#saving
ggplot(german, aes(saving_acct, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs( title = "存款情形", y = "人數")

#property
ggplot(german, aes(property, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs( title = "財產", y = "人數")

#present_emp
ggplot(german, aes(present_emp, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs( title = "就職期間", y = "人數")

#job
ggplot(german, aes(job, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge") +
  labs( title = "工作型態", y = "人數")

#glm regression
install.packages("caret")
library(caret)
set.seed(1234)
in.train <- createDataPartition(as.factor(german$response), p=0.75, list=FALSE)
german.train <- german[in.train,]
german.test <- german[-in.train,]

#modelwe pick variables
german.glm.wepick <- glm(response ~ sex + duration + credit_his + chk_acct + saving_acct
                   , family = binomial, german.train)

summary(german.glm.wepick)

#model machine pick for us
german.glm.machinepick <- glm(response ~ age + duration + credit_his + chk_acct + saving_acct
                         , family = binomial, german.train)


#ROC
##ROC we pick
prob.glm1.insample <- predict(german.glm.wepick, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.1667
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)
mean(ifelse(german.train$response != predicted.glm1.insample, 1, 0))

##ROC machine pick
prob.glm1.insample <- predict(german.glm.machinepick, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.1667
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)
mean(ifelse(german.train$response != predicted.glm1.insample, 1, 0))

#Confusion matrix
table(german.train$response, predicted.glm1.insample, dnn = c("Truth", "Predicted"))

library("ggplot2")
#roc plot
install.packages("verification")
library("verification")

#AUC we pick
roc.plot(german.train$response == "1", prob.glm1.insample)
roc.plot(german.train$response == "1", prob.glm1.insample)$roc.vol$Area

#AUC machine pick
roc.plot(german.train$response == "1", prob.glm1.insample)
roc.plot(german.train$response == "1", prob.glm1.insample)$roc.vol$Area

AIC(german.glm.machinepick)
