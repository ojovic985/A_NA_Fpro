#R

#Working directory in R must be '/A_NA_FPro'

A1<-read.csv("C1C2_git_list_F1_F5.csv",header=FALSE)
A2<-read.csv("C1C2_part_file_list.csv",header=FALSE)

g_F1_m1<-c(1,3,5,7,9,11)
g_F1_m2<-c(2,4,6,8,10,12)
g_F5_m1<-c(13,15,17,19,21,23)
g_F5_m2<-c(14,16,18,20,22,24)
part_file_numbers<-c(19,21,23,24)


C_p_value<-c(0.025,0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8)  ################# IMPORTANT - only calculated for 0.025, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, and 0.9.

for (tes in 1:length(C_p_value)) {
if ((C_p_value[tes]<0.025)||(C_p_value[tes]>0.8)) {
write.csv("Method not applicable for p<0.025 or p>0.8","Output.csv") }
else {
if ((C_p_value[tes]>=0.025)&&(C_p_value[tes]<0.05)) {
C1_vec<-c(40,46,48,57)}
else {
if ((C_p_value[tes]>=0.05)&&(C_p_value[tes]<0.10)) { 
C1_vec<-c(32,37,38,45)}
else {
if ((C_p_value[tes]>=0.10)&&(C_p_value[tes]<0.20)) {
C1_vec<-c(30,30,30,34)}
else {
C1_vec<-c(30,30,30,30) }
}}

dir.create(paste0("p_", C_p_value[tes], "_"))

zer<-30 # valid in all cases only for p>=0.2 
br_part_file<--1
for (ii in 1:nrow(A1)) {
if (ii%in%part_file_numbers) {
br_part_file<-br_part_file+2
C1C2_1<-read.csv(A2[br_part_file,1],header=TRUE)
C1C2_2<-read.csv(A2[br_part_file+1,1],header=TRUE)
C1C2<-rbind(C1C2_1,C1C2_2)
C1C2<-C1C2[,-1]}
else {
C1C2<-read.csv(A1[ii,1],header=TRUE)}

C1C2<-C1C2[,-1]

if (ii%in%g_F1_m1) {
zer<-C1_vec[1]}
else {
if (ii%in%g_F1_m2) {
zer<-C1_vec[2]}
else {
if (ii%in%g_F5_m1) {
zer<-C1_vec[3]}
else {
zer<-C1_vec[4]}}}

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<zer) ##################### Importatnt for prior chi-square
zu<-cbind(zu,i)
}
if (length(zu)>0) {
Mat<-Th[,-zu]}
else {
Mat<-Th}
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
if ((sum(Anem[,i])==0)||(t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value<C_p_value[tes])) {
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
logit_vector<-array(ncol(Anem))
sig_name_vec<-c()
for (i in 1:(ncol(Anem))) {
logit_vector[i]<-log(t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value/(1-t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))}


m_diff<-array(ncol(Anem))
margin_err_diff<-array(ncol(Anem))
s_pooled<-array(ncol(Anem))

for (ji in 1:ncol(Anem)) {

m_diff[ji]<-mean(NonAnem[,ji])-mean(Anem[,ji])
margin_err_diff[ji]<-1.96*sqrt( ((nrow(Anem)-1)*(sd(Anem[,ji])**2)+(nrow(NonAnem)-1)*(sd(NonAnem[,ji])**2)) /(nrow(Anem)+nrow(NonAnem)-2) ) *sqrt((1/nrow(Anem))+(1/nrow(NonAnem)))
s_pooled[ji]<-sqrt( ((nrow(Anem)-1)*(sd(Anem[,ji])**2)+(nrow(NonAnem)-1)*(sd(NonAnem[,ji])**2)) /(nrow(Anem)+nrow(NonAnem)-2) )
}
Stan_diff_means<-m_diff/margin_err_diff
Cohens_d<-m_diff/s_pooled
sig_name_vec<-colnames(Anem)
Mat<-t(t(Cohens_d))
Mat<-rbind(dim(NonAnem)[1],dim(Anem)[1],Mat,t(t(Stan_diff_means)),t(t(logit_vector)))
rownames(Mat)<-rbind(t(t("n(NonAnem_foods)")),t(t("n(Anem_foods)")),t(t(paste0("Cohens_d(", sig_name_vec, ")"))),t(t(paste0("Stan_diff_means(", sig_name_vec, ")"))),t(t(paste0("logit_p(", sig_name_vec, ")"))))


if (ii<13) {
if (ii>9) {
setwd(paste0("p_", C_p_value[tes], "_"))
write.csv(Mat,paste0("tu3_NonA_test_p_", C_p_value[tes], "_with_prior_p_", C_p_value[tes], "_chi2test_F1_", ii,".csv"))
setwd("../")}
else {
setwd(paste0("p_", C_p_value[tes], "_"))
write.csv(Mat,paste0("tu3_NonA_test_p_", C_p_value[tes], "_with_prior_p_", C_p_value[tes], "_chi2test_F1_0", ii,".csv"))
setwd("../")}
}
else {
if (ii<25) {
if ((ii-12)>9) {
setwd(paste0("p_", C_p_value[tes], "_"))
write.csv(Mat,paste0("tu3_NonA_test_p_", C_p_value[tes], "_with_prior_p_", C_p_value[tes], "_chi2test_F5_", (ii-12),".csv"))
setwd("../")}
else {
setwd(paste0("p_", C_p_value[tes], "_"))
write.csv(Mat,paste0("tu3_NonA_test_p_", C_p_value[tes], "_with_prior_p_", C_p_value[tes], "_chi2test_F5_0", (ii-12),".csv"))
setwd("../")}}
}
}
}
}


