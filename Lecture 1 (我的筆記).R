#1.1
1+2+3
3*7*2
6+7*3/2
(4*6)+5
4*(6+5)
17/5
#
17%%5
17%/%5

round(98.562,1)
round(1778,-2)
round(1234,-3)

help(round)

ceiling(pi)
floor(pi)

x<-2
y=5
rm(x)
x
rm(list=ls())

##1.2
x=c(1,2,3,4)
x=1:4
x=seq(1,4,by=1)

unique(x)
length(x)

sort(x,decreasing=FALSE)
sort(x,decreasing=TRUE)

x
x<=2
x>2
any(x<2)
all(x>2)
which(x==2)
which(x!=2)
x%in%c(4,5,6,7,8)

x[1]
x[1:2]
x[c(1,4)]

x[which(x>=2 & x<=3)]
x[which(x>=2 | x<=3)]

x=rep(0,4)
x
x=x+(1:4)
x

log(x)

factorial(x)
min(x)
max(x)
sum(x)

cumsum(x)
cumprod(x)
cummax(x)
cummin(x)

X=x^2

rm(X)
X=c("Hello","R","World")
length(X)
nchar(X)

##1.3
A=matrix(1:15,nrow=5)
A
rm(A)
A=matrix(1:15,nrow=5,dimnames=
           list(c("r1","r2","r3","r4","r5"),
                c("c1","c2","c3")))
A
nrow(A)
ncol(A)
dim(A)
colnames(A)
rownames(A)

head(A,n=2)
tail(A,n=2)


B=matrix(16:30,nrow=5)
A+B
cbind(A,B)
rbind(A,B)

row.means=apply(B,1,mean)
col.means=apply(A,2,mean)


A*B
A%*%t(B)
t(B)
A%o%t(B)




theArray=array(1:12,dim=c(2,3,2))
theArray

theArray[1,,]
theArray[1,,1]
theArray[,,1]

##Optional
list(1,2,3)
list(c(1,2,3))

list5=list(A,B,1:10,list(c(1,2,3),3:7))
list5
names(list5)=c("Amat","Bmat","vector","list2")
list5
list5[[1]]
list5[[1]][,2]
list5[[1]][,2,drop=FALSE]

dim(list5[[1]][,2])
dim(list5[[1]][,2,drop=FALSE])


##1.4
theDF=data.frame(10:1,-4:5,letters[1:10])
colnames(theDF)=c("First","Second","Third")

rm(list=ls())
results=read.table("...",header=T)
results$arch1[5]
#
attach(results)
names(results)
arch1
arch1[]
detach(results)
arch1
#
mean(arch1)
median(arch1)

var(arch1)
sd(arch1)
summary(arch1)


##
##1.5
plot(arch1,prog1)

#
names(results)

boxplot(prog1,xlab="Programming Semester 1")
boxplot(arch1~gender,xlab="Architecture Semester 1",xlab="gender",
ylab="Marks(%)")

bins=c(0,40,60,80,100)
hist(arch1,breaks=bins,xlab="Marks(%)",
     ylab="Number of Students",
     main="Architecture Semester 1")
#

x11(width=12,height=5)
par(mar=c(3,4,1,1))
par(mfrow=c(2,2))
hist(arch1,xlab="Architecture", main="Semester 1",ylim=c(0,35))
hist(arch2,xlab="Architecture", main="Semester 2",ylim=c(0,35))
hist(prog1,xlab="Programming", main="Semester 1",ylim=c(0,35))
hist(prog2,xlab="Programming", main="Semester 2",ylim=c(0,35))

xmin=min(arch1,na.rm=TRUE)
xmax=max(arch1,na.rm=TRUE) 
ymin=min(prog1,na.rm=TRUE) 
ymax=max(prog1,na.rm=TRUE)
xhist=hist(arch1, breaks=10, plot=FALSE)
yhist=hist(prog1, breaks=10, plot=FALSE)
top=max(c(xhist$counts, yhist$counts))
xrange=c(xmin, xmax); yrange=c(ymin, ymax)
m=matrix(c(2, 0, 1, 3), 2, 2, byrow=TRUE) 
x11(width=12,height=8)
layout(m, c(3, 1), c(1, 3), respect=TRUE)
par(mar=c(3, 4, 1, 1))
plot(arch1, prog1, xlim=xrange, ylim=yrange, xlab="", ylab="")
par(mar=c(0, 4, 1, 1))
barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
par(mar=c(3, 0, 1, 1))
barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
detach(results)


#1.6
f=expression(x^3*sin(x/3)*log(sqrt(x)))
#method1
g=D(f,"x")
gfun=function(x){eval(g)}
x0=1
gfun(x0)

#method2
library(numDeriv)
ffun=function(x){x^3*sin(x/3)*log(sqrt(x))}
grad(ffun, x0) 
#
h=1e-5
(ffun(x0+h)-ffun(x0))/h
#
#
library(PolynomF)
x=polynom()
p=(x-1)^2+10*x^3+5*x^4
integral(p)
#rm(list=ls())
integral(p, limits=c(0, 2))
#
integrate(p, 0, 2)



#In-class 
#grad()??????  intrgral()??????
fn <- sin(x)
gr <- function(x){grad(fn,x)}
F <- function(fn){fn*sqrt(1(x)+gr(x)^2)}
I <- function(F){integrate(F, 0, 2*pi)}
S <- 2*pi*I$value


fn <- sin
gr <- function(x){grad(fn, x)}
E <- function(x){fn(x)*sqrt(1+gr(x)^2)}
I <- integrate(E,0, 2*pi)
S <- 2*pi*I$value

library(PolynomF)
x = polynom()

fn=sin
gr=function(x){grad(fn,x)}
F=function(x){fn(x)*sqrt(1+gr(x)^2)}
I =integrate(F,0,pi)
S <- 2*pi*I$value


#
library(cubature)
f=function(x){1/(1+x[1]^2+x[2]^2)}
adaptIntegrate(f, c(0, 0), c(1, 1))


#1.7
x1=x2=seq(-1.5,1.5,0.1)
z=outer(x1, x2, FUN=function(x1, x2){
  100*(x2-x1^2)^2+(1-x1)^2})
persp(x1,x2,z,theta=150)
#
fr=function(x){
  x1=x[1]
  x2=x[2]
  100*(x2-x1^2)^2+(1-x1)^2
}
#
optim(c(0,0), fr)
#
grr=function(x){
  x1=x[1]
  x2=x[2]
  c(-400*x1*(x2-x1^2)-2*(1-x1),200*(x2-x1^2))
}

optim(c(0,0), fr, grr, method="BFGS")
nlm(fr, c(0,0))
optim(c(4,5), fr, method="L-BFGS-B",
      lower=c(3.5, 3.5), upper=c(5, 5))


