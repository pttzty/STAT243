##Stat 243 Problem Set 2
set.seed(0)
##How to select only selected columns
selectnames<-c("ST", "NP", "BDSP", "BLD", "RMSP", "TEN", "FINCP","FPARC", "HHL", "NOC", "MV", "VEH", "YBL")
##make the first file connection to that zipped file.
confirst=bzfile("ss13hus.csv.bz2", "r")
##This command will read the column names to be a vector called Allnames
Allnames<-read.csv(confirst,header=FALSE,nrow=1)
close(confirst)

##Set the columnclass to be an empty vector, and this for loop will make all positions 
## of selected columns to be NA and others to be Null
columnclass=c()
for (i in Allnames){
  if(is.element(i,selectnames))
     columnclass=c(columnclass,NA)
  else
    columnclass=c(columnclass,"NULL")
}

##A
set.seed(0)
randomsample<-sort(sample(2:7219001,10000))
randomvector<-rep(FALSE,7300000)
randomvector[randomsample]<-TRUE

Allnamesvector<-as.vector(as.matrix(Allnames))
nameposition<-match(selectnames,Allnamesvector)
rightorder<-Allnamesvector[sort(nameposition)]

csvread<-function(filename,blocksize,numcolumns){
  con<-bzfile(filename,open="r")
  sampledata<-data.frame(matrix(numeric(0),ncol=13, nrow = 10000),stringsAsFactors=FALSE)
  position_record=0
  for (i in 1:ceiling(numcolumns/blocksize)){
    chunck<-read.csv(con,nrows=blocksize,header=FALSE,colClasses=columnclass,stringsAsFactors=FALSE)
    samplefromchunck<-chunck[randomvector[((i-1)*blocksize+1):(i*blocksize)],]
    sampledata[(position_record+1):(position_record+nrow(samplefromchunck)),]<-samplefromchunck
    position_record=position_record+nrow(samplefromchunck)
    rm(chunck)
    rm(samplefromchunck)
    print(i)
  }
  colnames(sampledata)<-rightorder
  return(sampledata)
  close(con)
}

##Readlines Approach
randomsample<-sort(sample(2:7219001,10000))
randomvector<-rep(FALSE,7300000)
randomvector[randomsample]<-TRUE
library(stringr)
splitvector<-function(x){
  str_split(x, ",")
}

lineread<-function(filename,blocksize,numcolumns){
  for (i in 1:ceiling(numcolumns/blocksize)){
    con2<-bzfile(filename, "r")
    sampledata<-data.frame(matrix(numeric(0),ncol=13, nrow = 10000),stringsAsFactors=FALSE)
    position_record=0
    chunck<-readLines(con2,blocksize)
    
    samplefromchunck<-chunck[randomvector[((i-1)*blocksize+1):(i*blocksize)]]
    
    samplelines<-lapply(samplefromchunck,splitvector)
   
    sampledataframe<- data.frame(matrix(unlist(samplelines), nrow=length(samplefromchunck), byrow=TRUE))[, sort(nameposition)]
  
    sampledata[(position_record+1):(position_record+nrow(sampledataframe)),]<-sampledataframe
    position_record=position_record+nrow(sampledataframe)
    rm(chunck)
    rm(sampledataframe)
    rm(samplefromchunck)
    rm(samplelines)
    print(i)
  }
  close(con2)
  colnames(sampledata)<-rightorder
  return(sampledata)
}

##bash Approach, function
selectnames<-c("ST", "NP", "BDSP", "BLD", "RMSP", "TEN", "FINCP","FPARC", "HHL", "NOC", "MV", "VEH", "YBL")
confirst=bzfile("ss13hus.csv.bz2", "r")
Allnames<-read.csv(confirst,header=FALSE,nrow=1)
close(confirst)
Allnamesvector<-as.vector(as.matrix(Allnames))
nameposition<-match(selectnames,Allnamesvector)
write(nameposition,file="nameposition.txt")

randomsample<-sort(sample(2:7219001,10000))
randomvector<-rep(FALSE,7300000)
randomvector[randomsample]<-TRUE
rightorder<-Allnamesvector[sort(nameposition)]

bashread<-function(filename,blocksize,numcolumns){
  con1<-file(filename,open="r")
  sampledata<-data.frame(matrix(numeric(0),ncol=13, nrow = 10000),stringsAsFactors=FALSE)
  position_record=0
  for (i in 1:ceiling(numcolumns/blocksize)){
    chunck<-read.csv(con1,nrows=blocksize,header=FALSE,stringsAsFactors=FALSE)
    samplefromchunck<-chunck[randomvector[((i-1)*blocksize+1):(i*blocksize)],]
    sampledata[(position_record+1):(position_record+nrow(samplefromchunck)),]<-samplefromchunck
    position_record=position_record+nrow(samplefromchunck)
    rm(chunck)
    rm(samplefromchunck)
    print(i)
}
  close(con1)
  colnames(sampledata)<-rightorder
  return(sampledata)
}


plot(sampledata$BDSP,sampledata$RMSP)
