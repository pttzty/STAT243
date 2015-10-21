# install.packages("RSQLite")
library(RSQLite)
library(stringr)
filename<-"airline_data.db"
drv<-dbDriver("SQLite")
db<-dbConnect(drv,dbname=filename)

system.time(
for (i in 1987:1988){
  bzname=paste(toString(i),".csv.bz2",sep="")
  con1<-file(bzname,open="r")
  datacsv<-read.csv(con1,header=TRUE)
  dbWriteTable(conn=db,value=datacsv,name="year",row.names=FALSE)
  # auth<-dbSendQuery(db,"select * from year where Month=10 or 11 limit 100")
  close(con1)
  rm(datacsv)
}
)