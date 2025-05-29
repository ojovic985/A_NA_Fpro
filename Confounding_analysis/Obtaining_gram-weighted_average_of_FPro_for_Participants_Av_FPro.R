C<-read.csv("../Food_code_vs_WWEIA_code_without_duplicates.csv",header=TRUE)
C<-C[,-1]

W<-read.csv("../Data_9450_food_codes_g_per_100g.csv")
W<-W[,-1]

### Dataset produced using Code_to_create_C1C2_files_from_Driff_datasets_and_food_sets.R split into 7 equal parts

A1<-read.csv("C1C2_all_values_part_1_of_7_F1_07.csv",header=TRUE)
A2<-read.csv("C1C2_all_values_part_2_of_7_F1_07.csv",header=TRUE)
A3<-read.csv("C1C2_all_values_part_3_of_7_F1_07.csv",header=TRUE)
A4<-read.csv("C1C2_all_values_part_4_of_7_F1_07.csv",header=TRUE)
A5<-read.csv("C1C2_all_values_part_5_of_7_F1_07.csv",header=TRUE)
A6<-read.csv("C1C2_all_values_part_6_of_7_F1_07.csv",header=TRUE)
A7<-read.csv("C1C2_all_values_part_7_of_7_F1_07.csv",header=TRUE)

### Dataset produced using Code_to_create_C1C2_files_from_Driff_datasets_and_food_sets.R split into 7 equal parts

A<-rbind(A1,A2,A3,A4,A5,A6,A7)
A<-A[,-1]

colnames(A)<-gsub("X","",colnames(A))
XA<-cbind(A[,1:3],A[,4:ncol(A)]/2)
Cons_FPro<-rep(0,nrow(A))
Av_FPro<-rep(0,nrow(A))


for (i in 1:nrow(A)) {
F_code<-as.numeric(colnames(t(apply(A[i,4:ncol(A)], 1, function(x) x[x != 0]))))
if (any(is.na(F_code))==TRUE) {

F_code[match(NA,F_code)]<-94000000}
F_code_consumption_val<-as.vector(t(apply(XA[i,4:ncol(XA)], 1, function(x) x[x != 0])))
k<-0
for (j in 1:length(F_code)) {
WWEIA_class<-C[match(F_code[j],C[,1]),2]
if ((WWEIA_class<7001)||(WWEIA_class>7805)) {
Cons_FPro[i]<-Cons_FPro[i]+(F_code_consumption_val[j])* W[match(F_code[j],W[,ncol(W)]),"min_FPro"]
k<-k+F_code_consumption_val[j]}}
Av_FPro[i]<-Cons_FPro[i]/k
}

if (any(is.na(Av_FPro))==TRUE) {
Av_FPro[which(is.na(Av_FPro))]<-mean(Av_FPro[-which(is.na(Av_FPro))])}

At<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907.csv",header=TRUE)
At<-At[,-1]
At2<-cbind(At,t(t(Av_FPro)))
colnames(At2)[ncol(At2)]<-"Av_FPro"
write.csv(At2,"Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro.csv") #################### MARK for Obtaining_gram_sums_per_day_of_nutrients_AFTER_obtaining_Av_FPro


### Dataset produced using Code_to_create_C1C2_files_from_Driff_datasets_and_food_sets.R split into 8 equal parts

A1<-read.csv("C1C2_all_values_part_1_of_8_F5_07.csv",header=TRUE)
A2<-read.csv("C1C2_all_values_part_2_of_8_F5_07.csv",header=TRUE)
A3<-read.csv("C1C2_all_values_part_3_of_8_F5_07.csv",header=TRUE)
A4<-read.csv("C1C2_all_values_part_4_of_8_F5_07.csv",header=TRUE)
A5<-read.csv("C1C2_all_values_part_5_of_8_F5_07.csv",header=TRUE)
A6<-read.csv("C1C2_all_values_part_6_of_8_F5_07.csv",header=TRUE)
A7<-read.csv("C1C2_all_values_part_7_of_8_F5_07.csv",header=TRUE)
A8<-read.csv("C1C2_all_values_part_8_of_8_F5_07.csv",header=TRUE)

### Dataset produced using Code_to_create_C1C2_files_from_Driff_datasets_and_food_sets.R split into 8 equal parts

A<-rbind(A1,A2,A3,A4,A5,A6,A7,A8)
A<-A[,-1]

colnames(A)<-gsub("X","",colnames(A))
XA<-cbind(A[,1:3],A[,4:ncol(A)]/2)
Cons_FPro<-rep(0,nrow(A))
Av_FPro<-rep(0,nrow(A))


for (i in 1:nrow(A)) {
F_code<-as.numeric(colnames(t(apply(A[i,4:ncol(A)], 1, function(x) x[x != 0]))))
if (any(is.na(F_code))==TRUE) {

F_code[match(NA,F_code)]<-94000000}
F_code_consumption_val<-as.vector(t(apply(XA[i,4:ncol(XA)], 1, function(x) x[x != 0])))
k<-0
for (j in 1:length(F_code)) {
WWEIA_class<-C[match(F_code[j],C[,1]),2]
if ((WWEIA_class<7001)||(WWEIA_class>7805)) {
Cons_FPro[i]<-Cons_FPro[i]+(F_code_consumption_val[j])* W[match(F_code[j],W[,ncol(W)]),"min_FPro"]
k<-k+F_code_consumption_val[j]}}
Av_FPro[i]<-Cons_FPro[i]/k
}

if (any(is.na(Av_FPro))==TRUE) {
Av_FPro[which(is.na(Av_FPro))]<-mean(Av_FPro[-which(is.na(Av_FPro))])}


At<-read.csv("Female_aged_50_and_above.csv",header=TRUE)
At<-At[,-1]
At2<-cbind(At,t(t(Av_FPro)))
colnames(At2)[ncol(At2)]<-"Av_FPro"
write.csv(At2,"Female_aged_50_and_above_with_Av_FPro.csv") #################### MARK for Obtaining_gram_sums_per_day_of_nutrients_AFTER_obtaining_Av_FPro


