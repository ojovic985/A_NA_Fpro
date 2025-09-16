library(haven)
data<-read_sas("hn23_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn23_all.csv",row.names=FALSE)

data<-read_sas("hn22_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn22_all.csv",row.names=FALSE)

data<-read_sas("hn21_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn21_all.csv",row.names=FALSE)

data<-read_sas("hn20_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn20_all.csv",row.names=FALSE)

data<-read_sas("hn19_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn19_all.csv",row.names=FALSE)

data<-read_sas("hn18_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn18_all.csv",row.names=FALSE)

data<-read_sas("hn17_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn17_all.csv",row.names=FALSE)

data<-read_sas("hn16_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn16_all.csv",row.names=FALSE)

data<-read_sas("hn15_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn15_all.csv",row.names=FALSE)

data<-read_sas("hn14_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn14_all.csv",row.names=FALSE)

data<-read_sas("hn13_all.sas7bdat", encoding= "CP949")
write.csv(data,"hn13_all.csv",row.names=FALSE)

# One then Obtains files: hn23_all.csv, hn22_all.csv, ... hn13_all.csv

