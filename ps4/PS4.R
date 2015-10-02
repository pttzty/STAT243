##Problem 1
set.seed(0) 
runif(1)
save(.Random.seed, file = 'tmp.Rda')
# runif(1)
# # load('tmp.Rda') 
# # runif(1)
tmp = function() { 
  load('tmp.Rda') 
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
