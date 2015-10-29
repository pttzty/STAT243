# install.packages("RSQLite")
# install.packages("stringr")
# library(RSQLite)
# library(stringr)
# filename<-"airline_data.db"
# drv<-dbDriver("SQLite")
# db<-dbConnect(drv,dbname=filename)
# 
# year_vec=seq(1987,2008)
# system.time(
# for (i in year_vec){
#   bzname=paste(toString(i),".csv.bz2",sep="")
#   datacsv<-read.csv(bzname)
#   dbWriteTable(conn=db,value=datacsv,name="year",row.names=FALSE,append=TRUE)
#   rm(datacsv)
#   print(i)
# }
# )

##  The size of the database is 9399877632 bytes
## 7009728 count 2008
# auth<-dbSendQuery(db,"select count(*) from year")
# fetch(auth)

###
# wget http://www.stat.berkeley.edu/share/paciorek/1987-2008.csvs.tgz
# tar -xvzf 1987-2008.csvs.tgz
# bunzip2 *.csv.bz2
# install.packages("data.table")
# install.packages("RSQLite")
# install.packages("stringr")
library(data.table)
library(RSQLite)
filename<-"airline_data.db"
drv<-dbDriver("SQLite")
db<-dbConnect(drv,dbname=filename)
initExtension(db)
year_vec=seq(1987,2008)
system.time(
  for (i in year_vec){
    bzname=paste("bunzip2 -c ",toString(i),".csv.bz2",sep="")
    datacsv<-fread(bzname,header=TRUE,colClasses=c(rep("numeric", 8), "factor", "numeric", "factor", rep("numeric", 5),
                                                   rep("factor", 2), rep("numeric", 4),
                                                   "factor", rep("numeric", 6)))
    dbWriteTable(conn=db,value=datacsv,name="year",row.names=FALSE,append=TRUE)
    rm(datacsv)
    print(i)
  }
)

### Question 2 (SQLite Part)
Delay_noNA<-dbSendQuery(db,"create view Delay_noNA as select * from year where DepDelay!='NA'")

# sample<-dbSendQuery(db,"select round(CRSDepTime/100) as hour from Delay_noNA limit 100")
# fetch(sample)
sample_88<-dbSendQuery(db,"create view sample_88 as select * from Delay_noNA where Year=1988")

a<-dbSendQuery(db,"create view sample_stat as select UniqueCarrier, Origin, Dest,
               Month, DayOfWeek, FLOOR(CRSDepTime/100) as hour,
               sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) as prop_30,
               sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) as prop_60,
               sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) as prop_80,
               count(DepDelay) as Time_Delay from Delay_noNA GROUP BY UniqueCarrier, 
               Origin, Dest, Month, DayOfWeek, hour")
result<-"select * from sample_stat where Time_Delay>=150 order by prop_30 desc"
system.time(b<-dbGetQuery(db,result))

# ##1988 exp
filename<-"airline_1988.db"
drv<-dbDriver("SQLite")
db<-dbConnect(drv,dbname=filename)
i=1988


## First add a column of integer hours
Update_hour<-dbSendQuery(db,"Update year set CRSDepTime=FLOOR(CRSDepTime/100)")

###Create an index
system.time(
index_Q<-dbSendQuery(db,"create INDEX Unique_Index ON year (UniqueCarrier, Origin,
                     Dest, Month, DayofWeek, CRSDepTime)")
)
system.time(
stat_index<-dbSendQuery(db,"create table stat_index as select UniqueCarrier, Origin, Dest, Month, DayOfWeek,CRSDepTime,
                       sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) as prop_30,
                       sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) as prop_60,
                       sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) as prop_180,
                       count(DepDelay) as Time_Delay from year where DepDelay!='NA' GROUP BY UniqueCarrier,
                       Origin, Dest, Month, DayOfWeek, CRSDepTime")
)
system.time(
stat_150<-dbGetQuery(db,"select * from stat_index where Time_Delay>=150 order by prop_30 desc")
)


### Problem 3
install.packages("foreach")
library(parallel) # one of the core R packages
library(doParallel)
library(foreach)
library(iterators)
taskFun <- function(i){
  if(i==1){
    db1<-dbConnect(drv,dbname=filename)
    stat_1<-dbGetQuery(db1,"select UniqueCarrier, Origin, Dest, Month, DayOfWeek,CRSDepTime,
                       sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) as prop_30,
                       sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) as prop_60,
                       sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) as prop_180,
                       count(DepDelay) as Time_Delay from year where DepDelay!='NA' and Month in (1,2,3) GROUP BY UniqueCarrier,
                       Origin, Dest, Month, DayOfWeek, CRSDepTime")
    return(stat_1)
  }
  if(i==2){
    db2<-dbConnect(drv,dbname=filename)
    stat_2<-dbGetQuery(db2,"select UniqueCarrier, Origin, Dest, Month, DayOfWeek,CRSDepTime,
                       sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) as prop_30,
                       sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) as prop_60,
                       sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) as prop_180,
                       count(DepDelay) as Time_Delay from year where DepDelay!='NA' and Month in (4,5,6) GROUP BY UniqueCarrier,
                       Origin, Dest, Month, DayOfWeek, CRSDepTime")
    return(stat_2)
  }
  if(i==3){
    db3<-dbConnect(drv,dbname=filename)
    stat_3<-dbGetQuery(db3,"select UniqueCarrier, Origin, Dest, Month, DayOfWeek,CRSDepTime,
                       sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) as prop_30,
                       sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) as prop_60,
                       sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) as prop_180,
                       count(DepDelay) as Time_Delay from year where DepDelay!='NA' and Month in (7,8,9) GROUP BY UniqueCarrier,
                       Origin, Dest, Month, DayOfWeek, CRSDepTime")
    return(stat_3)
  }
  if(i==4){
    db4<-dbConnect(drv,dbname=filename)
    stat_4<-dbGetQuery(db4,"select UniqueCarrier, Origin, Dest, Month, DayOfWeek,CRSDepTime,
                       sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) as prop_30,
                       sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) as prop_60,
                       sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) as prop_180,
                       count(DepDelay) as Time_Delay from year where DepDelay!='NA' and Month in (10,11,12) GROUP BY UniqueCarrier,
                       Origin, Dest, Month, DayOfWeek, CRSDepTime")
    return(stat_4)
  }
}
nCores <- 4
system.time(
  res1 <- mclapply(0:23, taskFun, mc.cores = 4) 
)

### month, 4 tasks
taskFun <- function(i){
    Qtask<-paste("select UniqueCarrier, Origin, Dest, Month, DayOfWeek,CRSDepTime,sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) as prop_30,sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) as prop_60,sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) as prop_180,count(DepDelay) as Time_Delay from year where DepDelay!='NA' and Month in ",
               "(",toString(3*i-2),',',toString(3*i-1),',',toString(3*i),")",
               " GROUP BY UniqueCarrier,Origin, Dest, Month, DayOfWeek, CRSDepTime",sep='')
    db1<-dbConnect(drv,dbname=filename)
    stat_1<-dbGetQuery(db1,Qtask)
    return(stat_1)
  }
# user  system elapsed 
# 2.260   0.456 353.606 

## month, 12 tasks
taskFun <- function(i){
  Qtask<-paste("select UniqueCarrier, Origin, Dest, Month, DayOfWeek,CRSDepTime,sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) as prop_30,sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) as prop_60,sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) as prop_180,count(DepDelay) as Time_Delay from year where DepDelay!='NA' and Month= ",
               i,
               " GROUP BY UniqueCarrier,Origin, Dest, Month, DayOfWeek, CRSDepTime",sep='')
  db1<-dbConnect(drv,dbname=filename)
  stat_1<-dbGetQuery(db1,Qtask)
  return(stat_1)
}

nCores <- 4
registerDoParallel(nCores) 
system.time(
out <- foreach(i = 1:4) %dopar% {
  cat('Starting ', i, 'th job.\n', sep = '')
  outSub <- taskFun(i)
  cat('Finishing ', i, 'th job.\n', sep = '')
  outSub # this will become part of the out object
})

## Carrier tasks
carrier<-dbGetQuery(db,"select UniqueCarrier from year group by UniqueCarrier")
carrier<-carrier[,1]
taskFun <- function(i){
  Qtask<-paste("select UniqueCarrier, Origin, Dest, Month, DayOfWeek,CRSDepTime,sum(case when DepDelay>30 then 1 else 0 end)*1.0/count(*) as prop_30,sum(case when DepDelay>60 then 1 else 0 end)*1.0/count(*) as prop_60,sum(case when DepDelay>180 then 1 else 0 end)*1.0/count(*) as prop_180,count(DepDelay) as Time_Delay from year where DepDelay!='NA' and UniqueCarrier in ",
               "('",carrier[i],"')",
               " GROUP BY UniqueCarrier,Origin, Dest, Month, DayOfWeek, CRSDepTime",sep='')
  db1<-dbConnect(drv,dbname=filename)
  stat_1<-dbGetQuery(db1,Qtask)
  return(stat_1)
}

nCores <- 4
registerDoParallel(nCores) 
system.time(
  out <- foreach(i = 1:length(carrier)) %dopar% {
    cat('Starting ', i, 'th job.\n', sep = '')
    outSub <- taskFun(i)
    cat('Finishing ', i, 'th job.\n', sep = '')
    outSub # this will become part of the out object
  })


system.time(
  res1 <- mclapply(1:length(carrier), taskFun, mc.cores = 4) 
)

# user  system elapsed 
# 290.716  26.716 156.231
