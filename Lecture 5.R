##5.1 Simple linear regression
data()
attach(cars)
plot(cars)
#
L1=lm(dist~speed)
summary(L1)
#calculate without ?????????????????????????????????
errorMSE=function(par){
  obj=0
  for(i in 1:nrow(cars)){
    obj=(dist[i]-(par[1]+par[2]*speed[i]))^2+obj
  }
  obj
}
#
nlminb(c(-5,1),errorMSE)
#
errorMLE=function(par){
  loglik=0
  for(i in 1:nrow(cars)){
    loglik=dnorm(dist[i],(par[1]+par[2]*speed[i]),par[3],log=T)+loglik
  }
  -loglik
}
#
nlminb(c(-5,1,2),errorMLE,lower=c(-Inf,-Inf,1e-5)) #????????????????????????
#

#
res=residuals(L1)
mean(res)
yhat=fitted(L1) #??????L1????????????
plot(yhat,res)
plot(L1,which=1)
#
L2=lm(dist~0+speed)
summary(L2)



##5.2 Multiple linear regression & predictive modeling
detach(cars)
#Use the wine.csv file
wine.df = read.csv("wine.csv")
str(wine.df)
summary(wine.df)
#relationship between variables
library(gpairs)
gpairs(wine.df[2:7])
#???????????????????????????????????????????????????
#??????????????????:??????????????????????????????_??????dummy
cor(wine.df[2:7])
library(corrplot) # for correlation plot, install if needed
corrplot.mixed(corr=cor(wine.df[2:7], use="complete.obs"),
                 upper="ellipse", tl.pos="lt")

m1 = lm(Price ~ AGST + HarvestRain, data=wine.df)
summary(m1)
#--
yhat_m1=fitted(m1)
(cor(wine.df$Price, yhat_m1))^2
#--
m2 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine.df)
summary(m2)
m3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine.df)
summary(m3)

library(jtools)
plot_summs(m3,scale=TRUE)

##dummy variables ???forloop???????????????
ageCat=rep(0,length(CPS1988$ethnicity))
for (i in c(1:length(CPS1988$ethnicity))){
  if (CPS1988$ethnicity[i] == cauc){
    ageCat[i]=0
  } else if (CPS1988$ethnicity[i] == afam)
    {
      ageCat[i]=1
  }else{
      ageCat[i]=2
  }
}
wine.new.df=cbind(wine.df,ageCat)
#
m4 = lm(Price ~ AGST + HarvestRain + WinterRain+as.factor(ageCat), data=wine.new.df)
summary(m4)
#what as.factor does
age2=ifelse(wine.new.df$ageCat==2,1,0)
age3=ifelse(wine.new.df$ageCat==3,1,0)
m5=lm(Price ~ AGST + HarvestRain + WinterRain+age2+age3, data=wine.new.df)
summary(m5)




##Predictive Regression Modeling
##Analyze the ToyotaCorolla file
car.df = read.csv("ToyotaCorolla.csv")
# use first 1000 rows of data
car.df = car.df[1:1000, ]
# select variables for regression
selected.var = c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)

##cross validation: Toyota example
car.df.new = car.df[,c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)]
names(car.df.new) #examine data
table(car.df.new$Automatic) #check variable
car.lma = lm(Price~.,data=car.df.new )
#Price~.?????????????????????????????????????????????????????????
#interaction
car.lmb=lm(Price~ Age_08_04+Automatic+Age_08_04:Automatic+KM+Fuel_Type+
         HP+Met_Color+CC+Doors+Quarterly_Tax+Weight,data=car.df.new)

#install.packages('DAAG')
library(DAAG)
cv.lm(data=car.df.new, car.lma, m=2)
cv.lm(data=car.df.new, car.lmb, m=2)


set.seed(5566)  # set seed for reproducing the partition
train.index = sample(c(1:1000), 600, replace=FALSE)  
train.df = car.df[train.index, selected.var]
valid.df = car.df[-train.index, selected.var]
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm = lm(Price ~ ., data = train.df)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)
#evaluate prediction accuracy
install.packages('forecast')
library(forecast)
# use predict() to make predictions on a new set. 
car.lm.pred = predict(car.lm, valid.df)
# use accuracy() to compute common accuracy measures.
accuracy(car.lm.pred, valid.df$Price)

options(scipen=999, digits = 0)
some.residuals = valid.df$Price[1:20] - car.lm.pred[1:20] 
data.frame("Predicted" = car.lm.pred[1:20], 
           "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)
options(scipen=999, digits = 3)



##5.4 Quantile regression
library(quantreg)
library(AER)
data("CPS1988")
cps_f=log(wage) ~ experience + I(experience^2) + education
cps_lad=rq(cps_f, data=CPS1988)
cps_ols=lm(cps_f, data=CPS1988)
summary(cps_lad)
summary(cps_ols)
#tau(t)(S&33]*:.I-T!A7|9w3]0.5
cps_rq=rq(cps_f, tau=c(0.25, 0.75), data=CPS1988)
summary(cps_rq)
#
cps_rq25=rq(cps_f, tau=0.25, data=CPS1988)
cps_rq75=rq(cps_f, tau=0.75, data=CPS1988)
anova(cps_rq25, cps_rq75)
#
cps_rqbig=rq(cps_f, tau=seq(0.05, 0.95, 0.05), data=CPS1988)
cps_rqbigs=summary(cps_rqbig)
plot(cps_rqbigs)


attach(CPS1988)
tau=0.75
errorMAE=function(par){
  obj=0
  for(i in 1:nrow(CPS1988)){
    ei=log(wage[i])-(par[1]+par[2]*experience[i]+par[3]*experience[i]^2
                     +par[4]*education[i])
    obj=(tau*ei*(ei>0)+(1-tau)*abs(ei)*(ei<0))+obj
  }
  obj
}
#
fit=optim(c(0.5,0.5,0.5,0.5),errorMAE)

errorMAE(rep(0.5,4))
errorMAE(fit$par)
coef(cps_rq75)
errorMAE(coef(cps_rq75))