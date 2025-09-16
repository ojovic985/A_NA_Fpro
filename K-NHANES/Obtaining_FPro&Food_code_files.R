
A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn23_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn23_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn23_24rc.csv')


A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn22_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn22_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn22_24rc.csv')


A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn21_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn21_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn21_24rc.csv')

A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn20_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn20_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn20_24rc.csv')


A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn19_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn19_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn19_24rc.csv')



A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn18_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn18_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn18_24rc.csv')


A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn17_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn17_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn17_24rc.csv')

A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn16_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn16_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn16_24rc.csv')


A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn15_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn15_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn15_24rc.csv')

A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn14_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn14_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn14_24rc.csv')


A<-read.csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn13_24rc.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_prediction_F_codes_nutr_cont_of_hn13_24rc.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"
colnames(G1)[ncol(G1)-1]<-"N_FCODE"
write.csv(G1,'N_Code_and_FPro_of_hn13_24rc.csv')



