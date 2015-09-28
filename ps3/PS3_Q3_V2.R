#PS3 Q3
set.seed(11)
randomwalk<-function(nstep=10,start=c(0,0),fullpath=TRUE){
  if (nstep%%1==0 & nstep>0){
    randomvector=sample(c("Up","Down","Right","Left"),nstep,replace=TRUE)
    Updown=rep(0,nstep)
    Updown[randomvector=="Up"]=1
    Updown[randomvector=="Down"]=-1
    leftright=rep(0,nstep)
    leftright[randomvector=="Right"]=1
    leftright[randomvector=="Left"]=-1
    xcoordinates<-cumsum(leftright)+start[1]
    ycoordinates<-cumsum(Updown)+start[2]
    finalpos<-c(xcoordinates[nstep],ycoordinates[nstep])
    finalpath<-cbind(xcoordinates,ycoordinates)
    finalpath<-rbind(start,finalpath)
    rownames(finalpath)<-NULL
    if(fullpath==FALSE){
      return(finalpos)
    }
    else{
      return(finalpath)
    }
  }
  else{
    if(nstep%%1!=0){
      stop("Your input should be an integer")
    }
    if(nstep<=0){
      stop("Your input should be positive")
    }
    else{
      stop("Your input should be a positive integer")
    }
  }
}

library(methods)
walk <- function(nstep=10,start=c(0,0)){
  # constructor for 'rw' class
  path<-randomwalk(nstep,fullpath=TRUE)
  finalpos<-path[nrow(path),]
  obj <- list(finalpos=finalpos,path=path)
  class(obj) <- 'rw' 
  return(obj)
}
walk1<-walk(10000)
attributes(walk1)
print.rw<-function(obj){
  cat("The starting point is:", toString(obj$path[1,]),"\n")
  cat("The end point is: ", toString(obj$path[nrow(obj$path),]))
}
print(walk1)

plot.rw<-function(obj){
  plot(0,type="n",xlab="xcoordinate",ylab="ycoordinate",main="Random Walk Plot",
       xlim=range(obj$path[,1]),ylim=range(obj$path[,2]))
  lines(obj$path[,1],obj$path[,2])
  points(cbind(obj$path[1,1],obj$path[1,2]),col="red",pch=23)
  points(cbind(obj$path[nrow(obj$path),1],obj$path[nrow(obj$path),2]),col="blue",pch=24)
}
plot(walk1)

`start<-` <- function(x, ...) UseMethod("start<-")
`start<-.rw` <- function(obj, value){
  obj$path[,1]=obj$path[,1]+value[1]-obj$path[1,1]
  obj$path[,2]=obj$path[,2]+value[2]-obj$path[1,2]
  return(obj)
}
start(walk1)<-c(5,7)
walk1$path

'[.rw'<-function(object,i){
  obj<-object$path
  class(obj)<-"matrix"
  return(obj[i+1,])
}
walk1[3]
