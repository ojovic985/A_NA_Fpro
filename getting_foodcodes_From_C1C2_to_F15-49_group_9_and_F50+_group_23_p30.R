
C1C2<-read.csv("C1C2_30_or_more_non-zero_values_F1_09.csv",header=TRUE)
C1C2<-C1C2[,-1]


zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30) ##################### Importatnt to be checked and changed case-wise!!!
zu<-cbind(zu,i)
}
for (tz in 1:1) {
if (length(zu)>0) {
Mat<-Th[,-zu] }
else {
Mat<-Th}}
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

ter<-c()
ter_Fcod<-c()
p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
br2<-0
br1<-0
for (i in 4:ncol(Anem)) {
if ((sum(Anem[,i])==0)||(t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value<0.30)) { ################# IMPORTANT p<0.20!!!!!!!!!! ###
if (sum(Anem[,i])==0) {
br1<-br1+1}
else {
br2<-br2+1}
ter<-cbind(ter,i)
ter_Fcod<-cbind(ter_Fcod,colnames(Anem)[i])
p_ter<-cbind(p_ter,(t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
}}



Rez<-cbind(t(count_Anem),t(count_NonAnem),t(p_ter))
ter_Fcod_2<-gsub("X","",ter_Fcod)
rownames(Rez)<-t(ter_Fcod_2)
colnames(Rez)<-c("No.Anem","No.NonAnem","p-val") 
Rez


W<-read.csv("Data_9450_food_codes_g_per_100g.csv")
W<-W[,-1]

E<-as.numeric(rownames(Rez))

for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000 # COR

Rez2<-data.frame(cbind(E,Rez)) #COR
colnames(Rez2)[1]<-"Food code"
W1<-cbind(W[,ncol(W)],W[,-ncol(W)])
colnames(W1)[1]<-"Food code"

Rez_nAnem_foods<-gsub("X","",colnames(Anem)[-ter])
Rez_nAnem_foods<-as.numeric(Rez_nAnem_foods)
Rez_nAnem_foods<- Rez_nAnem_foods[!is.na(Rez_nAnem_foods)]
}


else{


Rez2<-cbind(E,Rez)
colnames(Rez2)[1]<-"Food code"
W1<-cbind(W[,ncol(W)],W[,-ncol(W)])
colnames(W1)[1]<-"Food code"

Rez_nAnem_foods<-gsub("X","",colnames(Anem)[-ter])
Rez_nAnem_foods<-Rez_nAnem_foods[-c(1:3)]
Rez_nAnem_foods<-as.numeric(Rez_nAnem_foods)
if (any(is.na(Rez_nAnem_foods))==TRUE) {

Rez_nAnem_foods[match(NA,Rez_nAnem_foods)]<-94000000
Rez_nAnem_foods<- Rez_nAnem_foods[!is.na(Rez_nAnem_foods)]}


}
}

B<-t(t(Rez_nAnem_foods))
colnames(B)[1]<-"Food code"
#A<-read.csv("Data_of_3099_NOVA1-4_NHANES_2003-2020_consistent_foods_in_grams.csv")

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"


Mer2<-function(X1,X2){
X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {   
if (is.na(match(X1["Food code"][i,1],X2[,"Food code"]))==FALSE) {
I<-cbind(I,i)   
J<-cbind(J,match(X1["Food code"][i,1],X2[,"Food code"]))}
}
X1X2<-cbind(X1[I,],X2[J,])
return(X1X2)}

F<-Mer2(Xe,B)
F<-F[,-67]
F<-F[,-67]

A<-F

B<-Rez2

F<-Mer2(Xe,B)
F<-F[,-67]

B<-F

Pop_of_Food<-A[,2:64]
Contr_p05_Anem_Food<-B[,2:64]

NonAnem<-Contr_p05_Anem_Food
Anem<-Pop_of_Food
neg_ln_t_vector<-array(ncol(Anem))
sig_name_vec<-c()
sig_neg_ln_p_vector<-c()
for (i in 1:(ncol(Anem))) {
if (colMeans(NonAnem)[i]>colMeans(Anem)[i]) {
neg_ln_t_vector[i]<--log(t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value)}
else {
neg_ln_t_vector[i]<-log(t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value)}}

sig_name_vec<-colnames(Anem)
sig_neg_ln_p_vector<-neg_ln_t_vector
Mat<-t(t(sig_neg_ln_p_vector))
rownames(Mat)<-t(t(sig_name_vec))

NonAnem<-Contr_p05_Anem_Food
Anem<-Pop_of_Food

write.csv(B,"NonAnem_codes_tu3_NonA_test_p30_F1_09.csv")
write.csv(A,"Anem_codes_tu3_NonA_test_p30_F1_09.csv")



C1C2_1<-read.csv("C1C2_30_or_more_non-zero_values_F5_11_part_1_to_be_rbind_with_part2.csv",header=TRUE)
C1C2_2<-read.csv("C1C2_30_or_more_non-zero_values_F5_11_part_2_to_be_rbind_with_part1.csv",header=TRUE)
C1C2<-rbind(C1C2_1,C1C2_2)
C1C2<-C1C2[,-1]
C1C2<-C1C2[,-1]


zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30) ##################### Importatnt to be checked and changed case-wise!!!
zu<-cbind(zu,i)
}
for (tz in 1:1) {
if (length(zu)>0) {
Mat<-Th[,-zu] }
else {
Mat<-Th}}
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

ter<-c()
ter_Fcod<-c()
p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
br2<-0
br1<-0
for (i in 4:ncol(Anem)) {
if ((sum(Anem[,i])==0)||(t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value<0.30)) { ################# IMPORTANT p<0.20!!!!!!!!!! ###
if (sum(Anem[,i])==0) {
br1<-br1+1}
else {
br2<-br2+1}
ter<-cbind(ter,i)
ter_Fcod<-cbind(ter_Fcod,colnames(Anem)[i])
p_ter<-cbind(p_ter,(t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
}}



Rez<-cbind(t(count_Anem),t(count_NonAnem),t(p_ter))
ter_Fcod_2<-gsub("X","",ter_Fcod)
rownames(Rez)<-t(ter_Fcod_2)
colnames(Rez)<-c("No.Anem","No.NonAnem","p-val") 
Rez


W<-read.csv("Data_9450_food_codes_g_per_100g.csv")
W<-W[,-1]

E<-as.numeric(rownames(Rez))

for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000 # COR

Rez2<-data.frame(cbind(E,Rez)) #COR
colnames(Rez2)[1]<-"Food code"
W1<-cbind(W[,ncol(W)],W[,-ncol(W)])
colnames(W1)[1]<-"Food code"

Rez_nAnem_foods<-gsub("X","",colnames(Anem)[-ter])
Rez_nAnem_foods<-as.numeric(Rez_nAnem_foods)
Rez_nAnem_foods<- Rez_nAnem_foods[!is.na(Rez_nAnem_foods)]
}


else{


Rez2<-cbind(E,Rez)
colnames(Rez2)[1]<-"Food code"
W1<-cbind(W[,ncol(W)],W[,-ncol(W)])
colnames(W1)[1]<-"Food code"

Rez_nAnem_foods<-gsub("X","",colnames(Anem)[-ter])
Rez_nAnem_foods<-Rez_nAnem_foods[-c(1:3)]
Rez_nAnem_foods<-as.numeric(Rez_nAnem_foods)
if (any(is.na(Rez_nAnem_foods))==TRUE) {

Rez_nAnem_foods[match(NA,Rez_nAnem_foods)]<-94000000
Rez_nAnem_foods<- Rez_nAnem_foods[!is.na(Rez_nAnem_foods)]}


}
}

B<-t(t(Rez_nAnem_foods))
colnames(B)[1]<-"Food code"
#A<-read.csv("Data_of_3099_NOVA1-4_NHANES_2003-2020_consistent_foods_in_grams.csv")

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"


Mer2<-function(X1,X2){
X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {   
if (is.na(match(X1["Food code"][i,1],X2[,"Food code"]))==FALSE) {
I<-cbind(I,i)   
J<-cbind(J,match(X1["Food code"][i,1],X2[,"Food code"]))}
}
X1X2<-cbind(X1[I,],X2[J,])
return(X1X2)}

F<-Mer2(Xe,B)
F<-F[,-67]
F<-F[,-67]

A<-F

B<-Rez2

F<-Mer2(Xe,B)
F<-F[,-67]

B<-F

Pop_of_Food<-A[,2:64]
Contr_p05_Anem_Food<-B[,2:64]

NonAnem<-Contr_p05_Anem_Food
Anem<-Pop_of_Food
neg_ln_t_vector<-array(ncol(Anem))
sig_name_vec<-c()
sig_neg_ln_p_vector<-c()
for (i in 1:(ncol(Anem))) {
if (colMeans(NonAnem)[i]>colMeans(Anem)[i]) {
neg_ln_t_vector[i]<--log(t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value)}
else {
neg_ln_t_vector[i]<-log(t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value)}}

sig_name_vec<-colnames(Anem)
sig_neg_ln_p_vector<-neg_ln_t_vector
Mat<-t(t(sig_neg_ln_p_vector))
rownames(Mat)<-t(t(sig_name_vec))

NonAnem<-Contr_p05_Anem_Food
Anem<-Pop_of_Food

write.csv(B,"NonAnem_codes_tu3_NonA_test_p30_F5_11.csv")
write.csv(A,"Anem_codes_tu3_NonA_test_p30_F5_11.csv")









