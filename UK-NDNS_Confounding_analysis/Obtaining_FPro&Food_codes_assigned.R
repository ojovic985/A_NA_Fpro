A<-read.csv("F_codes_ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_pred_nutr_cont­_of_ndns_rp_yr1-4a.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,'N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr1-4a.csv')



A<-read.csv("F_codes_ndns_rp_yr5-6a_foodleveldietarydata_v2.tab.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_pred_nutr_cont­_of_ndns_rp_yr5-6a.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,'N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr5-6a.csv')



A<-read.csv("F_codes_ndns_rp_yr7-8a_foodleveldietarydata.tab.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_pred_nutr_cont­_of_ndns_rp_yr7-8a.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,'N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr7-8a.csv')



A<-read.csv("F_codes_ndns_rp_yr9a_foodleveldietarydata_uk_20210831.tab.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_pred_nutr_cont­_of_ndns_rp_yr9a.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,'N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr9a.csv')



A<-read.csv("F_codes_ndns_rp_yr10a_foodleveldietarydata_uk_20210831.tab.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_pred_nutr_cont­_of_ndns_rp_yr10a.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,'N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr10a.csv')



A<-read.csv("F_codes_ndns_rp_yr11a_foodleveldietarydata_uk_20210831.tab.csv",header=TRUE)
A<-A[,-1]
B<-read.csv("FPro_pred_nutr_cont­_of_ndns_rp_yr11a.csv",header=TRUE)

G<-cbind(A,B)
G1<-G[,(ncol(G)-1):ncol(G)]
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,'N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr11a.csv')
