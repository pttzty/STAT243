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
.Random.seed[2]
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

vec_revise<-function(n=10,p=0.3,phi=0.5){
  vec<-1:(n-1)
  part_one<-lfactorial(n)-lfactorial(vec)-lfactorial(n-vec)
  part_two<-vec*log(vec)+(n-vec)*log(n-vec)-n*log(n)
  main_log=part_one+(1-phi)*part_two+vec*phi*log(p)+(n-vec)*phi*log(1-p)
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
microbenchmark(result_A_B<-sapply(1:length(IDsB),sumB<-function(x) sum(muB[IDsB[[x]]]*wgtsB[[x]])))
head(result_A_B)
# ##3B

# ##3B
## Set up the WgA_mat matrix
wgA_mat <- matrix(0, nc = length(wgtsA), nr = max(sapply(wgtsA,length)))
for(j in 1:length(wgtsA)) wgA_mat[,j][1:length(wgtsA[[j]])] <- wgtsA[[j]]

IDAmatrix<-matrix(NA, nc = length(IDsA), nr = max(sapply(IDsA,length)))
for(i in 1:length(IDsA)) IDAmatrix[,i][1:length(IDsA[[i]])] <- IDsA[[i]]

microbenchmark(result_B<-colSums(wgA_mat*muA[IDAmatrix],na.rm=TRUE),times=10L)
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
##4A
##By adding the print memory statement in the raw code of the lm function, we can inspect
## the memory used at the point that lm.fit is called by adding a print statement right before
## the lm.fit is called. We can inspect that there is approximately 152 Mb, compared to 61 MB in
## the global environment.

##4B
library(pryr)
rm(list=ls())
x1<-rnorm(1000000)
x2<-rnorm(1000000)
x3<-rnorm(1000000)
y<-rnorm(1000000)
mem_used()
# a<-lm(y~x1+x2+x3)
# mem_used()
# b<-lm.fit(cbind(x1,x2,x3),y)
# mem_used()
# c<-a$model
# mem_used()
lmrevised<-function (formula, data, subset, weights, na.action, method = "qr", 
          model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
          contrasts = NULL, offset, ...) 
{
  mem1<-mem_used()
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", 
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
## The memroy used after mf is created
  print(mem_used()-mem1)
  print(object_size(mf))
  if (method == "model.frame") 
    return(mf)
  else if (method != "qr") 
    warning(gettextf("method = '%s' is not supported. Using 'qr'", 
                     method), domain = NA)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
##
  print(head(.Internal(inspect(y))))
  print("The object size of y is")
  print(object_size(y))
  print("The object size of y names is")
  print(object_size(attributes(y)$names))
  print("The memroy use after y is created")
  print(mem_used()-mem1)
#    attributes(y)$dimnames<-NULL  
##
  w <- as.vector(model.weights(mf))
  if (!is.null(w) && !is.numeric(w)) 
    stop("'weights' must be a numeric vector")
  offset <- as.vector(model.offset(mf))
  if (!is.null(offset)) {
    if (length(offset) != NROW(y)) 
      stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                    length(offset), NROW(y)), domain = NA)
  }
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(coefficients = if (is.matrix(y)) matrix(, 0, 
                                                      3) else numeric(), residuals = y, fitted.values = 0 * 
                y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
                                                                                0) else if (is.matrix(y)) nrow(y) else length(y))
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }
  }
  else {
    print("The memory use after x is created")
    print(mem_used()-mem1)
    x <- model.matrix(mt, mf, contrasts)
    #########
    print(head(.Internal(inspect(x))))
    print("The size of x becomes")
    print(object.size(x))
    print("The size of dimnames of x is")
    print(object_size(attributes(x)$dimnames))
#    attributes(x)$dimnames<-NULL
    #########
    print(head(x))
    z <- if (is.null(w)){
      print("The memory used at the point lm.fit is called")
      print(mem_used()-mem1)
      lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
             ...)}
    else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
                 ...)
  }
  class(z) <- c(if (is.matrix(y)) "mlm", "lm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model) 
    z$model <- mf
  if (ret.x) 
    z$x <- x
  if (ret.y) 
    z$y <- y
  if (!qr) 
    z$qr <- NULL
  z
}


model1<-lmrevised(y~x1+x2+x3)

##4B
## By inspecting the object size, we found that the memory use before the call of lm.fit
## is mainly contributed by object the creating of mf, y, and x.
model2<-lm.fit(cbind(x1,x2,x3),y)
sapply(model1,object.size)
sapply(model2,object.size)