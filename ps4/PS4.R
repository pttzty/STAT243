#Problem 1
set.seed(0) 
runif(1)
save(.Random.seed, file = 'tmp.Rda')
runif(1)
load('tmp.Rda') 
runif(1)
tmp = function() { 
  load('tmp.Rda',env = .GlobalEnv) 
  runif(1)
} 
tmp()

###Problem 2
eval_deno<-function(n=10,p=0.3,phi=0.5){
  compute_deno<-function(k){
    part_two<-k*log(k)+(n-k)*log(n-k)-n*log(n)
    main_log=lchoose(n,k)+(1-phi)*part_two+k*phi*log(p)+(n-k)*phi*log(1-p)
    return(exp(main_log))
  }
  ##Case k=0
  first_ele<-n*phi*log(1-p)
  ## Case k=n
  last_ele<-n*phi*log(p)
  ## This vector contains all components from k value 1 to n-1
  main_vec<-sapply(1:(n-1),compute_deno)
  ## This is the denominator
  deno<-sum(main_vec)+exp(first_ele)+exp(last_ele)
  print(main_vec)
  return(deno)
}

vec_deno<-function(n=10,p=0.3,phi=0.5){
  vec<-1:(n-1)
  part_two<-vec*log(vec)+(n-vec)*log(n-vec)-n*log(n)
  main_log=lchoose(n,vec)+(1-phi)*part_two+vec*phi*log(p)+(n-vec)*phi*log(1-p)
  first_ele<-n*phi*log(1-p)
  last_ele<-n*phi*log(p)
  deno<-sum(exp(main_log))+exp(first_ele)+exp(last_ele)
  return(deno)
}

##problem3
mixedMember<-load("mixedMember.Rda")
##3A
microbenchmark(result_A_A<-sapply(1:length(IDsA),sumA<-function(x) sum(muA[IDsA[[x]]]*wgtsA[[x]])))
head(result_A_A)
summary(result_A_A)
microbenchmark(result_A_B<-sapply(1:length(IDsB),sumB<-function(x) sum(muB[IDsB[[x]]]*wgtsB[[x]])))
head(result_A_B)
summary(result_A_B)
##3B
# ##3C
# lik <- matrix(as.numeric(0), nr = length(wgtsB), nc = max(sapply(wgtsB,length)))
# for(j in 1:length(wgtsB)) lik[j,][1:length(wgtsB[[j]])] <- wgtsB[[j]]
# # sapply(1:nrow(lik),trans<-function(i) lik[i,][1:length(wgtsA[[i]])]<-wgtsA[[i]])
# IDBmatrix<-matrix(as.numeric(0), nr = length(IDsB), nc = max(sapply(IDsB,length)))
# for(j in 1:length(IDsB)) IDBmatrix[j,][1:length(IDsB[[j]])] <- IDsB[[j]]

##3C
##Create an empty matrix to place weight elements, which is the "linear
## transformation" of muB in computation
weightframe <- matrix(0, nr=length(wgtsB), nc=length(muB))
for (i in 1:nrow(weightframe)) {
  weightframe[i, IDsB[[i]]] <- wgtsB[[i]]
}
microbenchmark(result_C<-as.vector(weightframe%*%muB))

##
##Problem4
library(pryr)
rm(list=ls())
x1<-rnorm(1000000)
x2<-rnorm(1000000)
x3<-rnorm(1000000)
y<-rnorm(1000000)
mem_used()
a<-lm(y~x1+x2+x3)
mem_used()
b<-lm.fit(cbind(x1,x2,x3),y)
mem_used()
c<-a$model
mem_used()