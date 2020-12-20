#Q1 poisson ~ tweedie (poisson + gamma)
r <- c()
r[1] <- (1-a)*g
r[j+1] <- (j-1+a/j+1)*g*r[j]

p = c()
p[0] <- function(a, b, g){
  if(a =! 0){
  po = exp^(beta*[((1-g)^a)-1])/a  
  }
  else if(a == 0){
  po = (1-g)^b
  }
}
  print(p[1])
  
p[1] <- b*g*p[0]
j*r[k+1]*p[j]

p[k+1] <- (1/k+1)*(b*g*p[k])

#???summation?????????
  
PTF <- function(k, a, b, g){
  
}

#Q2
library(AER)
data("CPS1988")
CPS1988
require(dummies)

#???????????????dummy
#method 1 does not work, why?
require(dummies)
CPS1988$ethnicity <- factor(CPS1988$ethnicity)
CPS1988$ethnicity
level = c(unique(CPS1988$ethnicity))
          
library(dummies)
CPS1988 <- dummy.data.frame(CPS1988)

#method 2 forloop provided  by prof
ethnicityCat=rep(0,length(CPS1988$ethnicity))
for (i in c(1:length(CPS1988$ethnicity))){
  if (CPS1988$ethnicity==cauc){
    ethnicityCat[i]= 0
  } else (CPS1988$ethnicity[i] == afam)
  {
    ethnicityCat[i]=1
  }
  
}
CPS1988=cbind(CPS1988, ethnicityCat)

#a

library("AER")

lm_formula = lm(log(wage)~experience+I(experience^2)+education+as.factor(ethnicityCat),
                data = CPS1988)

#Error in eval(predvars, data, env) : invalid 'envir' argument of type 'character'
##results the same after trimming data

summary(lm_formula)


#c
glm_formula = glm(log(wages)~experience+experience^2+education+as.factor(ethnicity),
                data = "CPS1988", family="gaussian")
#factor & ethnicityCat??????????????????????????????

#e
library(quantreg)
quantile_formula = log(wage)~experience+education+experience^2+as.factor(ethnicity)
cps_rqbig=rq(quantile_formula, tau=seq(0.05, 0.95, 0.05), data=CPS1988)
cps_rqbigs=summary(cps_rqbig)
plot(cps_rqbigs)

#Q3
#a
car.accident = c(rep(0, 109), rep(1, 65), rep(2, 22), rep(3, 3), 4)
car.accident
#b
library (MASS)
fitdistr(car.accident,"Poisson")# 0.61000000 
fitdistr(car.accident,"Poisson")$loglik#-206.106

#c
lambda=0.61000000 
for(i in 0:4){
  p=dpois(i,lambda)
  predicted.frequency=200*p


(d)
for(i in 0:4){
 
 
#Q4
set.seed(1234)
S=500000
n=0
j=1
for(j in 1:500000){
  for (n in 1:20)
    
  }
  print(n/500000) 