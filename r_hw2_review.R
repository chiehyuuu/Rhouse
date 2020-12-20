#Q1 (a)

f = numeric(100)
f[1] = 1
f[2] = 1
k = 0
n = 3
while(k<100){
  f[n] = f[n-1] +f[n-2]
  k= f[n]
  n = n+1
  print(c(k,n))
} 
# ans:k= 144, n=13

#Q1(c)
print.Fib <-function(k){
  f = numeric(100)  #100隨便設
  f[1] = 1
  f[2] = 1
  n = 3
  for(n in seq_along(f)){ #這邊沒有代入3:k因k不屬於費氏數列之一，為一比較值，100是隨便設的
    f[n] =  f[n-1] + f[n-2]
    n = n+1}
  if(f[n]>= k){ #碰到大於等於k就離開迴圈
    break
  }else{
    print(f)
  }
}

print.Fib(100)

#Q3
f.exist <- function(x, z){
  ifelse(z%in%x, TRUE, FALSE)
}
f.exist(z=10, x=c(1:10))
f.exst(z=10, x=c(9, 3, 1))

#Q4 

f.divide =function(z){
  x=2
  i=0
  while (x<z) {
    if(z%%x==0)
      x = x+1
      i = i+1
  }
  return(i)
}
f.divide(100)  

#method2
f.divide<-function(z) {
  y <- seq_len(z)
  length(y[ z%%y == 0 ]) -2
}

f.divide(100)
