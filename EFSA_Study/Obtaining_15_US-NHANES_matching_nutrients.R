A<-read.csv("FNDDS_2009-10_Training_Data.csv",header=TRUE)

fe<-c()
for (i in 1:nrow(A)) {
if ((A[i,"novaclass"]>0)&&(A[i,"novaclass"]<5)) {
fe<-cbind(fe,c(i))}}

Mat<-A[fe,]

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

C1<-read.csv("Train.csv",header=TRUE)
C1<-C1[,-1]
C1<-C1[colnames(C1) %in% c('Vitamin.K..phylloquinone.','Calcium','Iron','Copper','Selenium','Magnesium','Phosphorus','Potassium','Zinc','Vitamin.E..alpha.tocopherol.','Thiamin','Riboflavin','Niacin','Vitamin.B.6','Vitamin.B.12','target')]


C2<-read.csv("Test.csv",header=TRUE)
C2<-C2[,-1]
C2<-C2[colnames(C2) %in% c('Vitamin.K..phylloquinone.','Calcium','Iron','Copper','Selenium','Magnesium','Phosphorus','Potassium','Zinc','Vitamin.E..alpha.tocopherol.','Thiamin','Riboflavin','Niacin','Vitamin.B.6','Vitamin.B.12','target')]

write.csv(C1,"Train_15nutr.csv")
write.csv(C2,"Test_15nutr.csv")

