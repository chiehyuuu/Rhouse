#Q1
#(a),(b)
fn1 = 1
fn2 = 1
k = 0
n = 2
while(k<100){
  k = fn1 +fn2
  fn1 = fn2
  fn2 = k
  n = n+1
  print(c("k"=k,"index_n"=n))
} 
# ans:k= 144, n=12

#(c)
print.Fib = function(k){
  fn =c()
  fn[1] = 1
  fn[2] = 1
  fn_final = c()
  if(k<=1){
    print(fn)
  }else if(k<=2){
    fn = c(fn,fn[1]+fn[2])
    print(fn)
  }else{
    for(i in 3:k){
      fn[i] = fn[i-1] +fn[i-2]
      if(fn[i]<=k){
        fn_final = fn
      }
    }
    print(fn_final)
  } 
}
print.Fib(1) #ans:1 1
print.Fib(2) #ans:1 2
print.Fib(100) #ans:1 1 2 3 5 8 13 21 34 55 89



#Q2
second.smallest = function(x){
  y = sort(unique(x),decreasing=FALSE)
  ifelse(is.na(y[2]),"there is no second smallest number",y[2])
}
second.smallest(x=c(2, 8, 8, 2, 5, 2, 5, 2)) #ans:5
second.smallest(x=c(1, 1, 1, 1)) #ans:"there is no second smallest number"

#Q3
f.exist = function(z,x){
  ifelse(z %in% x,TRUE,FALSE)
}
f.exist(z=10, x=c(1:10)) #ans:true
f.exist(z=10, x=c(9, 3, 1)) #ans:false

#Q4
f.divide =function(z){
  x=2
  l=0 
  while (x<z) {
    if(z%%x==0){
      l = l+1
    }
  x = x+1
  }
  return(l)
}

f.divide(100) #ans:7

#Q5
#(a)
fn=expression(x^7+10000*x^6+1.06*x^5+10600*x^4+0.0605*x^3+605*x^2+0.0005*x+5)
ffun=function(x){eval(fn)}
g=D(fn,"x") #微分
gfun=function(x){eval(g)}

#(b)
f <- function(x)
{
  x^7+10000*x^6+1.06*x^5+10600*x^4+0.0605*x^3+605*
    x^2+0.0005*x+5   
}

g <- function(x) {
  7 * x^6 + 10000 * (6 * x^5) + 1.06 * (5 * x^4) + 10600 * (4 * 
                                                              x^3) + 0.0605 * (3 * x^2) + 605 * (2 * x) + 5e-04
}

root <- function(f, g, guess, tolerance=0.00001) {
  x = guess
  i=0
  while (abs(f(x)) > tolerance) {
    x = x - f(x)/g(x)
    i = i+1
  }
  print(c(x, i))
}
root(f, g, 3, tol=0.00001)
#ans -10000 233446

#(c)
root(f, g, 0, tol=0.00001)
#ans:-10000 1


#Q6
BesselI_Gen = function(a, v, z, max, tolerance){
  m = 0
  I.last = -Inf #上次加總,這個起始值讓I-I.last不會收斂太快
  I = 1/((gamma(m+a+1)*factorial(m))^v)*((z/2)^(2*m+a)) #這次加總
  while (m < max & abs(I-I.last)>tolerance) { 
    I.last=I
    m = m+1
    I =I.last+ 1/((gamma(m+a+1)*factorial(m))^v)*((z/2)^(2*m+a))
  }
  return(I)
}

BesselI_Gen(5, 1, 10, 1000, 1e-5) 
besselI(10, 5)

#SAME, ans:777.1883
