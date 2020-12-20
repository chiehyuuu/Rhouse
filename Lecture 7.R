##7.1
loan.df=read.csv("UniversalBank.csv")
plot(as.factor(Personal.Loan)~Age,data=loan.df)
plot(as.factor(Personal.Loan)~CCAvg,data=loan.df)

###logistic regression
logit.reg=glm(Personal.Loan~Income,data=loan.df,
              binomial(link="logit"))
options(scipen=999)
summary(logit.reg)

#--
exp(-6.13+0.037*2)/(1+exp(-6.13+0.037*2)) - exp(-6.13+0.037*1)/(1+exp(-6.13+0.037*1))
exp(-6.13+0.037*31)/(1+exp(-6.13+0.037*31)) - exp(-6.13+0.037*30)/(1+exp(-6.13+0.037*30))

y=loan.df$Personal.Loan
x=loan.df$Income

logitloglik=function(par){
            loglik=0
            for(i in 1:nrow(loan.df)){
              loglik=loglik+log(dbinom(y[i],1,exp(par[1]+par[2]*x[i])/
                                  (1+exp(par[1]+par[2]*x[i]))))
            }
            -loglik
}

nlminb(c(0,0),logitloglik)


#--
logit.reg0=update(logit.reg,
                  formula=.~1)
summary(logit.reg0)

anova(logit.reg,logit.reg0,test="Chisq")


loan2.df=loan.df[,-c(1,5)] #drop ID and zip code columns for regression 
#naming factor variable
loan2.df$Education=factor(loan2.df$Education, levels = c(1, 2, 3), 
                          labels = c("Undergrad", "Graduate", "Advanced/Professional"))

#split data as training and validation
nrow(loan2.df)
set.seed(1)  # Set the seed for the random number generator for reproducing the 
# partition.
train.index=sample(1:nrow(loan2.df),
                   3000,replace=FALSE)
training=loan2.df[train.index,]
testing=loan2.df[-train.index,]

logit.reg1=glm(Personal.Loan~.,data=training,family="binomial")
summary(logit.reg1)

#performance metrix
table(true=training$Personal.Loan,pred=round(fitted(logit.reg1)))
#round????????????????????????0.5??????1,???????????????0
#?????????????????????->0.2
#???ifselse(fitted(logit.reg1)>0.2, 1, 0)
library(ROCR)
# Prediction function
ROCpred=prediction(fitted(logit.reg1), training$Personal.Loan)
# Performance function
ROCRperf = performance(ROCpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
#threshold on the right
plot(ROCRperf, colorize=TRUE)
#Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))
#
performance(ROCpred , "auc")

logit.reg2=glm(Personal.Loan~.+I(CCAvg^2),data=training,family="binomial")
summary(logit.reg2)


pred.out.1=predict(logit.reg1,
                   newdata=testing,
                   type="response")
pred.out.2=predict(logit.reg2,
                   newdata=testing,
                   type="response")

pred.1=prediction(pred.out.1,
                  testing$Personal.Loan)
pred.2=prediction(pred.out.2,
                  testing$Personal.Loan)

x11(width=8,height=5)
plot(performance(pred.1,"tpr","fpr"),col='red',lty=2,lwd=2)
plot(performance(pred.2,"tpr","fpr"),add=T,col='green',lty=3,lwd=2)

abline(0,1,lty=2)
performance(pred.1,"auc")
performance(pred.2,"auc")





##Multinominal logistic regression
library(AER)
data(BankWages)
edcat=factor(BankWages$education)
levels(edcat)[3:10]=rep(c("14-15", "16-18", "19-21"),
                        c(2, 3, 3))
plot(job ~ edcat, data=BankWages,off=0)
#
#install.packages("nnet")
#detach(package:car, unload=TRUE)
library(nnet)
library(AER)
bank_mlogit=multinom(job ~ education, 
                     data=BankWages)
coeftest(bank_mlogit)
summary(bank_mlogit) #?????????equation????????????????????????equation
table(predict(bank_mlogit))#????????????????????????custodial?????????
table(BankWages$job)


y=BankWages$job
table(y)
x=BankWages$education

mlogitloglik=function(par){
  loglik=0
  for(i in 1:nrow(BankWages)){
     p.ad=exp(par[1]+par[2]*x[i])/(1+exp(par[1]+par[2]*x[i])+
                                    exp(par[3]+par[4]*x[i]))
     p.ma=exp(par[3]+par[4]*x[i])/(1+exp(par[1]+par[2]*x[i])+
                                    exp(par[3]+par[4]*x[i]))
     p.cu=1/(1+exp(par[1]+par[2]*x[i])+
               exp(par[3]+par[4]*x[i]))
     loglik=loglik+(y[i]=="admin")*log(p.ad)+
                   (y[i]=="manage")*log(p.ma)+
                   (y[i]=="custodial")*log(p.cu)
     }
     -loglik
}

nlminb(c(0,0,0,0),mlogitloglik)


#
bank_mlogit.2=multinom(job ~ education +minority, 
                       data=BankWages)
coeftest(bank_mlogit.2)

table(predict(bank_mlogit.2))
table(BankWages$job)
#????????????????????????????????????????????????

#Can you try the training & testing splits?



