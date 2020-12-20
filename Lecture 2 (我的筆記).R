#2.1
double.num=function(x){
  x*2
}
double.num(5)

double.num=function(x){  
  return(x*2)
  print("Hello")
  return(17)
}
double.num(5)


#2.2
toCheck=1
if(toCheck ==1){
  print("hello")
}

check.bool=function(x){
  if(x==1){
    print("hello")
  }else{
      print("goodbye")
    }
}

check.bool(1)
check.bool(0)



ifelse(1==1, "Yes", "No")
toTest=c(1, 1, 0, 1, 0, 1)
ifelse(toTest==1, toTest*3, 0)

a=c(1, 1, 0, 1)
b=c(2, 1, 0, 1)
ifelse(a==1 & b==1, "Yes", "No")


#2.3
x=1
while(x<=5){
  print(x)           
  x=x+1
}



CMPZ=function(lam,nu,max=1000,
              tol=1e-10){
  j=0
  val.last=-Inf
  val=lam^j*factorial(j)^-nu
  while(          ){
    val.last=val
    j=j+1
    val=val.last+
        
  }
  return(val)
}


CMPZ(5,1,max=5000)


#2.4
for(i in 1:10){
    print(i)
}


oddcount=function(x){  
  k=0
  for(n in 1:length(x)){     
    if(   ){   }
  }
  return(k)
}
oddcount(0:10)


mtable=matrix(NA, ncol=9, nrow=9)
for (i in 1:9) {
  for (j in 1:9){
    mtable[ ]=
  }
}
mtable

mtable=function(x){
  output = matrix(NA, ncol=x, nrow=x)
  for (i in 1: ) {
    for (j in 1: ){
      output[ ]=
    }
  }
  return(  )     
}

mtable(9)

mtable2=function(x){
  output = c( )
  for(i in 1: ){
    output=rbind(output, )
  }
  return(  )     
}

mtable2(9)

system.time(mtable(1000))
system.time(mtable2(1000))


findclosest=function(x,y){
  n=length(x)
  min.distance=
    sqrt((x[1]-x[2])^2+(y[1]-y[2])^2)
  i.min=1
  j.min=2
  for(i in 1:  ){
    for(j in ){
      distance=
        sqrt((x[ ]-x[ ])^2+
               (y[ ]-y[ ])^2)
      if(distance<min.distance){
        min.distance=distance
        i.min=i
        j.min=j
      }
    }
  } 
  return(c(min.distance,i.min,j.min))
}


xcord=c(17,6,20,-2,7)
ycord=c(-6,-3,4,1,6)
plot(xcord,ycord,type='p',
     col='red',ylim=c(-10,10),
     xlim=c(-10,20),
     xlab="x",ylab="y")
findclosest(xcord,ycord)
points(xcord[ ],ycord[ ],
       pch=19,col='green')
lines(xcord[ ],ycord[ ],
      lty=2,lwd=2,col='green')


