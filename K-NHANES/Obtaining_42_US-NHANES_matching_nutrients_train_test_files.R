A<-read.csv("FNDDS_2009-10_Training_Data.csv",header=TRUE)

fe<-c()
for (i in 1:nrow(A)) {
if ((A[i,"novaclass"]>0)&&(A[i,"novaclass"]<5)) {
fe<-cbind(fe,c(i))}}

Mat<-A[fe,]

Nat<-cbind(Mat[,1:62],Mat[,"novaclass"])
colnames(Nat)[ncol(Nat)]<-"target"
write.csv(Nat,"Train_62_All2971.csv")

Mat2<-A[-fe,]

B<-read.csv("Data_9450_food_codes_g_per_100g.csv",header=TRUE)
B<-B[,-1]
colnames(Mat)[100]<-"Food.code"
RT<-merge(Mat,B,by=c("Food.code"))
RT2<-RT[,c(1,101:171)]
RT2<-RT2[!colnames(RT2) %in% c("pythonlabel","max_p")]
P2<-RT2[,7:68]
V2<-log(P2)
y_train<-RT2[,"novaclass"]
x_train<-V2
B2<-B[!(B$Food.code %in% Mat$Food.code),]
B2[,"class"]<-B2[,"class"]+1
y_test<-B2[,"class"]
x_test<-log(B2[,1:62])
TEST<-cbind(x_test,y_test)
TRAIN<-cbind(x_train,y_train)
colnames(TRAIN)[1:62]<-colnames(TEST)[1:62]
colnames(TRAIN)[ncol(TRAIN)]<-"target"
colnames(TEST)[ncol(TEST)]<-"target"

write.csv(TRAIN,"Train.csv")
write.csv(TEST,"Test.csv")

A<-read.csv("Train.csv",header=TRUE)
A1<-read.csv("Test.csv",header=TRUE)

A<-A[!colnames(A) %in% c('Alcohol','Caffeine','Theobromine','Copper','Selenium','Carotene..alpha','Cryptoxanthin..beta','Lycopene','Lutein...zeaxanthin','Vitamin.B.6','Folate..total','Vitamin.B.12','Choline..total','Vitamin.K..phylloquinone.','Folate..food','Vitamin.E..added','Vitamin.B.12..added','X4.00','X6.00','X8.00')]
A<-A[,-1]
A1<-read.csv("Test.csv",header=TRUE)
A1<-A1[,-1]
A1<-A1[!colnames(A1) %in% c('Alcohol','Caffeine','Theobromine','Copper','Selenium','Carotene..alpha','Cryptoxanthin..beta','Lycopene','Lutein...zeaxanthin','Vitamin.B.6','Folate..total','Vitamin.B.12','Choline..total','Vitamin.K..phylloquinone.','Folate..food','Vitamin.E..added','Vitamin.B.12..added','X4.00','X6.00','X8.00')]

write.csv(A,"Train_42nutr.csv")
write.csv(A1,"Test_42nutr.csv")



