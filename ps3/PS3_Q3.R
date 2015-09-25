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

##3C
library(methods)
walk <- function(nstep=10,start=c(0,0),fullpath=TRUE){
  # constructor for 'rw' class
  obj <- randomwalk(nstep,start,fullpath)
  class(obj) <- 'rw' 
  return(obj)
}

##The method printrw will display the final position of the random walk
print <- function(object, ...) 
  UseMethod("print") 
print.rw<-function(obj){
  cat("The starting point is:", toString(obj[1,]),"\n")
  cat("The end point is: ", toString(obj[nrow(obj),]))
}

plot<-function(object,...)
  UseMethod("plot")
plot.rw<-function(obj){
  plot(0,type="n",xlab="xcoordinate",ylab="ycoordinate",main="Random Walk Plot",
       xlim=range(obj[,1]),ylim=range(obj[,2]))
  lines(obj[,1],obj[,2])
  points(cbind(obj[1,1],obj[1,2]),col="red",pch=23)
  points(cbind(obj[nrow(obj),1],obj[nrow(obj),2]),col="blue",pch=24)
}

`start<-` <- function(x, ...) UseMethod("start<-")
`start<-.rw` <- function(obj, value){ 
  obj[,1]=obj[,1]+value[1]
  obj[,2]=obj[,2]+value[2]
  return(obj)
}

'[.rw'<-function(object,i){
  obj<-object
  class(obj)<-"matrix"
  return(obj[i+1,])
}
walk1<-walk(10,fullpath=TRUE)