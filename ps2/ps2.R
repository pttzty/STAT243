##Stat 243 Problem Set 2
set.seed(0)
selectcolumn<-function(filename){   
  selectnames<-c("ST", "NP", "BDSP", "BLD", "RMSP", "TEN", "FINCP","FPARC", "HHL", "NOC", "MV", "VEH", "YBL")   
  confirst=file(filename, "r")   
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
  return(columnclass)
} 


selectnames<-c("ST", "NP", "BDSP", "BLD", "RMSP", "TEN", "FINCP","FPARC", "HHL", "NOC", "MV", "VEH", "YBL")

##make the first file connection to that zipped file. 
confirst=bzfile("ss13hus.csv.bz2", "r") 
##This command will read the column names to be a vector called Allnames 
Allnames<-read.csv(confirst,header=FALSE,nrow=1)
close(confirst) 

## We created a vector called nameposition which records what are the corresponding
## positions in the Allnames, so it's easy for us to add colnames back to the sample.
Allnamesvector<-as.vector(as.matrix(Allnames))
nameposition<-match(selectnames,Allnamesvector)
## The right order vector is the colnames we add back in each step.
rightorder<-Allnamesvector[sort(nameposition)]
print(rightorder)


##A
set.seed(0)
randomsample<-sort(sample(2:7219001,10000)) 
randomvector<-rep(FALSE,7300000) 
randomvector[randomsample]<-TRUE

csvread<-function(filename,blocksize,numcolumns){
  con<-file(filename,open="r")
  ##Creating the empty sample dataframe.
  sampledata<-data.frame(matrix(numeric(0),ncol=13, nrow = 10000),stringsAsFactors=FALSE)
  ##Initialize the position called position_record so that I know 
  ## where to insert in that sample dataframe. 
  position_record=0
  columnclass<-selectcolumn(filename)
  for (i in 1:ceiling(numcolumns/blocksize)){
    chunck<-read.csv(con,nrows=blocksize,header=FALSE,colClasses=columnclass,stringsAsFactors=FALSE)
    ##We extract the sample from this specific chunk, and the upper bound is 
    ## i*blocksize, and the good thing is we can directly use the logical 
    ## vector to take the subset.
    samplefromchunck<-chunck[randomvector[((i-1)*blocksize+1):(i*blocksize)],]
    ## We subsistute the part of the pre-created data frame to be the one just extracted.
    sampledata[(position_record+1):(position_record+nrow(samplefromchunck)),]<-samplefromchunck
    ## Update the Position.
    position_record=position_record+nrow(samplefromchunck)
    ## Remove used data to save memory
    rm(chunck)
    rm(samplefromchunck)
  }
  colnames(sampledata)<-rightorder
  return(sampledata)
  close(con)
}

system.time(test<-csvread("ss13hus.csv.bz2",100000,7219001))
head(test)
##Readlines Approach
print(nameposition)
##Here is the function that "breaks" every element in the row in to another vector.
library(stringr) 
splitvector<-function(x){   
  str_split(x, ",") 
} 
lineread<-function(filename,blocksize,numcolumns){ 
  con2<-bzfile(filename, "r") 
  con2<-bzfile(filename, "r")     
  sampledata<-data.frame(matrix(numeric(0),ncol=13, nrow = 10000),stringsAsFactors=FALSE)     
  position_record=0   
  for (i in 1:ceiling(numcolumns/blocksize)){        
    chunck<-readLines(con2,blocksize)    
    samplefromchunck<-chunck[randomvector[((i-1)*blocksize+1):(i*blocksize)]]     
    samplelines<-lapply(samplefromchunck,splitvector) 
    ## This command transforms a large list, with all elements listed vertically to a dataframe,
    ## We know the number of rows should be equal to the number of samples in this chunk, thus
    ## we just set nrow=length and we are done. 
    sampledataframe<- data.frame(matrix(unlist(samplelines), nrow=length(samplefromchunck), byrow=TRUE),stringsAsFactors=FALSE)[,sort(nameposition)]     
    sampledata[(position_record+1):(position_record+nrow(sampledataframe)),]<-sampledataframe     
    position_record=position_record+nrow(sampledataframe)     
    rm(chunck)     
    rm(sampledataframe)     
    rm(samplefromchunck)     
    rm(samplelines)    
  }   
  close(con2)
  colnames(sampledata)<-rightorder  
  return(sampledata) 
}
system.time(b<-lineread("ss13hus.csv.bz2",100000,7219001))
head(b)

##bash Approach, function

write(nameposition,file="nameposition.txt")
system.time(test3<-csvread("newdata.csv",100000,7219001))
head(test3)

##D
plot(test3$BDSP,test3$RMSP)
plot(test3$NP,test3$NOC)