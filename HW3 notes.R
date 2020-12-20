dice1 <- c(1:6)
dice2 <- c(1:6)
eventA <- (dice1+dice2)>=5
#ProbA:A{(1,4),(2,3),(3,2),(4,1),(5,)}
#P(eventA): 4/36 
eventB <- dice1>dice2
#P(eventB):B{65 64 63 62 61 54 53 52 51 43 42 41 32 31 21}
#15/36 
eventC <- dice1 == 4 
#P(eventC) 1/36 
a1 <- 
a2 <- 15/36 +???1/36 - ((15/36)*(1/36))   
a3 <- a1*a2

setseed(247)
S=1000
n=0
for(s in 1:S){
  select <- sample(1:6, 2, replace = TRUE)
  if(select[1]+select[2] >= 5 & select[1] == 4){
    n=n+1
    }
}
  print(n/1000)
  
  
commitee <- c(rep("Chicano",5), rep("Asian",2), rep("African", 3), rep("Caucasian", 2))
S=5000
n=0
subcommitee = 0
for(s in 1:S){
 select <- sample(commitee, 4, replace= FALSE)
 if(length(unique(select))==4){
   n=n+1
 }
}
print(n/5000)


#(b)
x1 <- sample(c(1:6),1000,replace=TRUE)
table(x)/1000

x2 <- sample(c(1:6),1000,replace=TRUE)
table(x)/1000

#2 a pair is chosen
(choose(7, 2)+choose(8, 2)+choose(9,2))/choose(24,2)
#a black pair is chosen
choose(7, 2)/choose(24,2)

#b 
b <- sample(c(1:6),1000,replace=TRUE)

#4
choose(4, 1)*choose(16, 1)/choose(52,2)

#5 ?????????=????????????????????????
((lfactorial(10000)-lfactorial((10000-10)))/(10000^10))

choose(10000, 100)*factorial(100)/10000^100

((10000*log(10000)-10000)-(9900*log(9900)-9900)) / 100*log(10000)


choose(6561, 100)*lfactorial(100)
lchoose(10000, 100)/log(10000)
     