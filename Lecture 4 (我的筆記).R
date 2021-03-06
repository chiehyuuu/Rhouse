#4.1 Basics of Probability
##Simulating coin-tossing experiments
#e$'f8f3e	 he0e
!g>f(!f,
x=sample(c("H","T"),10,replace=TRUE)
table(x)
table(x)/10
#
x=sample(c("H","T"),100,replace=TRUE)
table(x)/100

#d<0h(e <variabilityi+o<f/f,!e:d>g5fd8
d8 f(#,d?!e?ff8d=
#
x=sample(c("H","T"),10000,replace=TRUE)
table(x)/10000


#Simulating IC chips inspection




##4.2 Discrete probability distributions
##Attacks to computing center example
attack.data=c(rep(0,9),rep(1,14),rep(2,13),
              rep(3,9),rep(4,4),rep(5,2),rep(6,1))
attack.data
table(attack.data)
attacks=c(9,14,13,9,4,2,1)
probability=attacks/sum(attacks)
pdf=round(probability,2)

x11(height=5,width=12)
par(mfrow=c(1,2))
Attacks=0:6
plot(Attacks,pdf,xlab="attacks/week",
     ylab="f(x)",type='h')

cumprob=cumsum(pdf)
plot(Attacks,cumprob,
     xlab="attacks/week",ylab="F(x)",
     type='S')

library(fitdistrplus)
attack.fit=fitdist(attack.data,
                   "pois")
plot(attack.fit)
#e7&i
f/f 9fg6i)o<g4h	2g:gh+ e9+e?fe0f d=3ef8e <limbda
summary(attack.fit)

#pdf, cdff2h>&f3eg-f-7e2d8
f2f	go< 7d;%d8
f;f
gf)g=0
#h&i
f0i8f(!eo<eg.limbda
#poissonf/i&i8h(g.f;f
f,!f8

##MLE - a discrete probability distribution
x11(width=8,height=5)
n=40
y=9
theta=seq(0,1,0.01)
lik=theta^y*(1-theta)^(n-y)
par(mar=c(3,4,1,1))
plot(theta,lik,type='l',lwd=3,col='blue',xlab="",ylab="likelihood",
     xaxt="n")
axis(1,seq(0,1,0.1))
abline(v=y/n,col='red',lwd=2,lty=2)
mtext(expression(paste(theta)),
      side=1,line=2,cex=1.3,font=3)



##4.2 Bernoulli and binomial distributions
sum(dbinom(5562:15687,15687,0.35))
pbinom(5562,15687,0.35)
1-pbinom(5562,15687,0.35)

#f

e.i;pdfe
 g8=
sum(dbinom(4513:12567,12567,0.35))


##4.3 Airline revenue management 
seats=19
demand=seq(14,25,1)
demand.prob=c(0.03,0.05,0.07,0.09,0.11,0.15,
              0.18,0.14,0.08,0.05,0.03,0.02)
p.show=0.9
price=150
cost=325
opened.spots=seq(19,25)

Eprofits=c()
S=5000
sim.demand=sample(demand,S,replace=    ,prob=demand.prob)

for(i in 1:length(opened.spots)){
  profits.i=c()
  for(s in 1:S){
    d=sim.demand[s]
    reserved=
    show=
    profits.i[s]=
      
  }
  Eprofits[i]=round(mean(profits.i),2)
}

Eprofits
which.max(Eprofits)
opened.spots[which.max(Eprofits)]

x11(width=8,height=6)
plot(opened.spots,Eprofits,yaxt="n",ylim=c(2720,2800),
     type='l',lwd=3,xlab="Opennings",ylab="E[Profit]")
points(opened.spots,Eprofits)
axis(2,seq(2720,2800,10))


##4.4 Continuous probability distributions
##On Normal distribution
curve(dnorm(x,1.5,0.5),from=0,to=3,ylab="f(x)")

x=seq(0.5,3,len=101)
y=cbind(dnorm(x,1.65,0.25),dnorm(x,1.85,0.25))
matplot(x,y,type='l',xlab="",ylab="f(x)")

x=seq(5,15,len=101)
y=cbind(dnorm(x,10,1),dnorm(x,10,2))
matplot(x,y,type='l',xlab="",ylab="f(x)")

z1=rnorm(10000,1,1)
z2=rnorm(10000,1,2)
z=z1+z2
par(las=1)
hist(z,breaks=seq(-10,14,0.2),freq=F)
x=seq(-10,14,0.1)
lines(x,dnorm(x, , ),col='red',lwd=3)


##On Gamma distribution
x=seq(0,30,0.1)
plot(x,dexp(x,0.2),type='l',lwd=3,ylab="f(x)")
lines(x,dgamma(x,1,0.2),col='red',lty=2,lwd=2)
lines(x,dgamma(x,2,0.2),col='blue',lty=3,lwd=2)
lines(x,dgamma(x,5,0.2),col='green',lty=4,lwd=2)


#MLE- a continuous probability distribution
#read kew.txt first
summary(kew)
kew[,2:13]=kew[,2:13]/10
hist(kew[,8])

jul.rain=kew[,8]
jul.rain=jul.rain[jul.rain>0]
#
library(fitdistrplus)
jul.rain.fit=fitdist(jul.rain,
                     "gamma")
plot(jul.rain.fit)
jul.rain.fit$estimate

#
gammaloglik=function(par){
  loglik=0
  for(i in 1:length(jul.rain)){
    loglik.i=ifelse(log(dgamma(jul.rain[i],par[1],par[2]))==-Inf,
                    0,log(dgamma(jul.rain[i],par[1],par[2])))
    loglik=loglik+loglik.i     
  }
  -loglik
}
#
mle=nlminb(c(1,1),gammaloglik,
           lower=c(1e-5,1e-5))
#
alpha.mle=mle$par[1]
lambda.mle=mle$par[2]
#

x11(width=8,height=5)
par(las=1)
hist(kew[,8],breaks=20,freq=F,xlab="rainfall(mm)",ylab="density",main="",
     ylim=c(0,0.02))
t=seq(0,max(kew[,8]),0.5)
lines(t,dgamma(t,alpha.mle,lambda.mle),col='green',lwd=2)
#
normallik=function(par){
  loglik=0
  for(i in 1:length(jul.rain)){
    loglik.i=ifelse(log(dnorm(jul.rain[i],par[1],par[2]))==-Inf,
                    0,log(dnorm(jul.rain[i],par[1],par[2])))
    loglik=loglik+loglik.i     
  }
  -loglik
}
mle2=nlminb(c(mean(jul.rain),sd(jul.rain)),normallik,lower=c(1e-5,1e-5))
lines(t,dnorm(t,mle2$par[1],mle2$par[2]),col='blue',lty=2,lwd=2)
#
exponenlik=function(par){
  loglik=0
  for(i in 1:length(jul.rain)){
    loglik.i=ifelse(log(dexp(jul.rain[i],par[1]))==-Inf,
                    0,log(dexp(jul.rain[i],par[1])))
    loglik=loglik+loglik.i     
  }
  -loglik
}
mle3=nlminb(c(lambda.mle),exponenlik,lower=c(1e-5))
lines(t,dexp(t,mle3$par[1]),col='red',lty=3,lwd=2)

legend(120,0.015,c("Gamma","Normal","Exponential"),
       lty=c(1,2,3),lwd=c(2,2,2),bty="n",cex=1.1,col=c('green','blue','red'))



##4.5 Morty's retirement fund
#find x(candidates)such that prob(Broke) <= 0.05
#????????????/????????????
savings=1000000
inflation=0.03
withdraw=seq(50000,80000,500)
#???????????????20???
life.mu=20
life.sd=10
#?????????????????????s???
cale.est=10^2/20
shape.est=life.mu/scale.est

growth.mu=0.08
growth.sd=0.02

S=5000
prob.broke=c()

life=rgamma(S,shape=shape.est,scale=scale.est)
sim.growth=list()
for(s in 1:S){
  sim.growth[[ ]]=rnorm(         , growth.mu, growth.sd)
}


for(i in 1:length(withdraw)){
  broke.i=0
  for(s in 1:S){
    years.togo=round(life[s],0)
    j=1
    spending=withdraw[i]
    savings.left=savings
    while(j<=years.togo & savings.left>0){
      #cat("life=",j,"$left=",savings.left,"\n")
      savings.lef
    if(savings.left<=0){broke.i=broke.i+1}
    if(s%%1000==0){cat("Withdraw=",withdraw[i],
                       "S=",s,"\n")}
  }
  prob.broke[i]=broke.i/S
  
  list

prob.broke=
which(prob.broke<0.05)
withdraw[wh??=ich(prob.broke<0.05)]
#list?????
plot(withdraw,prob.broke,type='l',lwd=3)
points(withdraw,prob.broke,pch=1)
abline(h=0.05,col='green',lty=2,lwd=2)
