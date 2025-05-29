
B<-read.csv("Xnum_and_Xfum_1hotencoded_Merged_FullSer_C-P_minBPX.csv",header=TRUE)
B<-B[,-1]

C<-B[,1:9]
C<-C[,-c(5,6,7,8)]

GT<-cbind(B[,"ridreth1_1"],B[,"ridreth1_2"],B[,"ridreth1_3"],B[,"ridreth1_4"],B[,"ridreth1_5"],B[,"huq010_1"])
colnames(GT)<-c("ridreth1_1","ridreth1_2","ridreth1_3","ridreth1_4","ridreth1_5","huq010_1")

BR<-cbind(C,GT)

write.csv(BR,"Confounder_var_set.csv")


