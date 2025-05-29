
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
Cons_FPro<-matrix(rep(0,nrow(A)*62),nrow=nrow(A))
Av_FPro<-matrix(rep(0,nrow(A)*62),nrow=nrow(A))

for (i in 1:nrow(A)) {
F_code<-as.numeric(colnames(t(apply(A[i,4:ncol(A)], 1, function(x) x[x != 0]))))
if (any(is.na(F_code))==TRUE) {

F_code[match(NA,F_code)]<-94000000}
F_code_consumption_val<-as.vector(t(apply(XA[i,4:ncol(XA)], 1, function(x) x[x != 0])))
for (j in 1:length(F_code)) {
Cons_Food[i,]<-Cons_Food[i,]+as.vector(t((F_code_consumption_val[j])* W[match(F_code[j],W[,ncol(W)]),1:62]))}

if (any(is.na(Cons_Food))==TRUE) {
Cons_Food[which(is.na(Cons_Food))]<-mean(Cons_Food[-which(is.na(Cons_Food))])}

At<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro.csv",header=TRUE) ### Dataset produced in Obtaining_gram-weighted_average_of_FPro_for_Participants_Av_FPro
At<-At[,-1]
colnames(Cons_Food)<-colnames(W[,1:62])
At3<-cbind(At,Cons_Food)

write.csv(At3,"Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro_and_SUMS_per_day_of_62_consumed_nutrients_under_1888_removed-codes-terms.csv")  ####### MARK for Confounding_analysis_Part_3_code.R


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
Cons_FPro<-matrix(rep(0,nrow(A)*62),nrow=nrow(A))
Av_FPro<-matrix(rep(0,nrow(A)*62),nrow=nrow(A))

for (i in 1:nrow(A)) {
F_code<-as.numeric(colnames(t(apply(A[i,4:ncol(A)], 1, function(x) x[x != 0]))))
if (any(is.na(F_code))==TRUE) {

F_code[match(NA,F_code)]<-94000000}
F_code_consumption_val<-as.vector(t(apply(XA[i,4:ncol(XA)], 1, function(x) x[x != 0])))
for (j in 1:length(F_code)) {
Cons_Food[i,]<-Cons_Food[i,]+as.vector(t((F_code_consumption_val[j])* W[match(F_code[j],W[,ncol(W)]),1:62]))}

if (any(is.na(Cons_Food))==TRUE) {
Cons_Food[which(is.na(Cons_Food))]<-mean(Cons_Food[-which(is.na(Cons_Food))])}

At<-read.csv("Female_aged_50_and_above_with_Av_FPro.csv",header=TRUE) ### Dataset produced in Obtaining_gram-weighted_average_of_FPro_for_Participants_Av_FPro
At<-At[,-1]
colnames(Cons_Food)<-colnames(W[,1:62])
At3<-cbind(At,Cons_Food)

write.csv(At3,"Female_aged_50_and_above_with_Av_FPro_and_SUMS_per_day_of_62_consumed_nutrients_under_1888_removed-codes-terms.csv")  ####### MARK for Confounding_analysis_Part_3_code.R

