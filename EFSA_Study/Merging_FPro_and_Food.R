A1<-read.csv("Italy_15_nutr_for_FPro.csv",header=TRUE)
A1<-A1[,-1]
B<-read.csv("FPro_pred_Italy_15_nutr_for_FPro.csv",header=TRUE)
G1<-cbind(A1,B)
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,"FPro_predicted_plus_food_Italy_15_nutr.csv")

A1<-read.csv("Finland_15_nutr_for_FPro.csv",header=TRUE)
A1<-A1[,-1]
B<-read.csv("FPro_pred_Finland_15_nutr_for_FPro.csv",header=TRUE)
G1<-cbind(A1,B)
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,"FPro_predicted_plus_food_Finland_15_nutr.csv")


A1<-read.csv("Sweden_15_nutr_for_FPro.csv",header=TRUE)
A1<-A1[,-1]
B<-read.csv("FPro_pred_Sweden_15_nutr_for_FPro.csv",header=TRUE)
G1<-cbind(A1,B)
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,"FPro_predicted_plus_food_Sweden_15_nutr.csv")

A1<-read.csv("France_15_nutr_for_FPro.csv",header=TRUE)
A1<-A1[,-1]
B<-read.csv("FPro_pred_France_15_nutr_for_FPro.csv",header=TRUE)
G1<-cbind(A1,B)
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,"FPro_predicted_plus_food_France_15_nutr.csv")


A1<-read.csv("Germany_15_nutr_for_FPro.csv",header=TRUE)
A1<-A1[,-1]
B<-read.csv("FPro_pred_Germany_15_nutr_for_FPro.csv",header=TRUE)
G1<-cbind(A1,B)
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,"FPro_predicted_plus_food_Germany_15_nutr.csv")


A1<-read.csv("Netherlands_15_nutr_for_FPro.csv",header=TRUE)
A1<-A1[,-1]
B<-read.csv("FPro_pred_Netherlands_15_nutr_for_FPro.csv",header=TRUE)
G1<-cbind(A1,B)
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,"FPro_predicted_plus_food_Netherlands_15_nutr.csv")


A1<-read.csv("United.Kingdom_15_nutr_for_FPro.csv",header=TRUE)
A1<-A1[,-1]
B<-read.csv("FPro_pred_United.Kingdom_15_nutr_for_FPro.csv",header=TRUE)
G1<-cbind(A1,B)
colnames(G1)[ncol(G1)]<-"FPro"

write.csv(G1,"FPro_predicted_plus_food_United.Kingdom_15_nutr.csv")



