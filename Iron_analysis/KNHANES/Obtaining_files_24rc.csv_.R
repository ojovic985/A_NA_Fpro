library(haven)
data<-read_sas("hn23_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn23_24rc.csv",row.names=FALSE)

data<-read_sas("hn22_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn22_24rc.csv",row.names=FALSE)

data<-read_sas("hn21_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn21_24rc.csv",row.names=FALSE)

data<-read_sas("hn20_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn20_24rc.csv",row.names=FALSE)

data<-read_sas("hn19_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn19_24rc.csv",row.names=FALSE)

data<-read_sas("hn18_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn18_24rc.csv",row.names=FALSE)

data<-read_sas("hn17_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn17_24rc.csv",row.names=FALSE)

data<-read_sas("hn16_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn16_24rc.csv",row.names=FALSE)

data<-read_sas("hn15_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn15_24rc.csv",row.names=FALSE)

data<-read_sas("hn14_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn14_24rc.csv",row.names=FALSE)

data<-read_sas("hn13_24rc.sas7bdat", encoding= "CP949")
write.csv(data,"hn13_24rc.csv",row.names=FALSE)

# One then Obtains files: hn23_24rc.csv, hn22_24rc.csv, ... hn13_24rc.csv

