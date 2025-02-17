###Load C1C2 file...
### Example
C1C2<-read.csv("../C1C2_30_or_more_non-zero_values_M1_07.csv",header=TRUE)
C1C2<-C1C2[,-1]
### (end of example)

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
for (j in 1:1) {
if (length(zu)>0) {
Mat<-Th[,-zu]}
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


W<-read.csv("../Data_9450_food_codes_g_per_100g.csv")
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

A<-read.csv("../Data_9450_food_codes_g_per_100g.csv")
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


write.csv(B,"NonAnem_codes_tu3_NonA_test_p30_M1_07.csv")
write.csv(A,"Anem_codes_tu3_NonA_test_p30_M1_07.csv")





C1C2<-read.csv("../C1C2_30_or_more_non-zero_values_F1_09.csv",header=TRUE)
C1C2<-C1C2[,-1]
### (end of example)

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
for (j in 1:1) {
if (length(zu)>0) {
Mat<-Th[,-zu]}
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


W<-read.csv("../Data_9450_food_codes_g_per_100g.csv")
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

A<-read.csv("../Data_9450_food_codes_g_per_100g.csv")
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


write.csv(B,"NonAnem_codes_tu3_NonA_test_p30_F1_09.csv")
write.csv(A,"Anem_codes_tu3_NonA_test_p30_F1_09.csv")



C1C2_1<-read.csv("../C1C2_30_or_more_non-zero_values_F5_11_part_1_to_be_rbind_with_part2.csv",header=TRUE)
C1C2_2<-read.csv("../C1C2_30_or_more_non-zero_values_F5_11_part_2_to_be_rbind_with_part1.csv",header=TRUE)
C1C2<-rbind(C1C2_1,C1C2_2)
C1C2<-C1C2[,-1]
C1C2<-C1C2[,-1]
### (end of example)

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
for (j in 1:1) {
if (length(zu)>0) {
Mat<-Th[,-zu]}
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


W<-read.csv("../Data_9450_food_codes_g_per_100g.csv")
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

A<-read.csv("../Data_9450_food_codes_g_per_100g.csv")
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


write.csv(B,"NonAnem_codes_tu3_NonA_test_p30_F5_11.csv")
write.csv(A,"Anem_codes_tu3_NonA_test_p30_F5_11.csv")


A1<-read.csv("Anem_codes_tu3_NonA_test_p30_F1_09.csv",header=TRUE)
A2<-read.csv("Anem_codes_tu3_NonA_test_p30_F5_11.csv",header=TRUE)
A3<-read.csv("Anem_codes_tu3_NonA_test_p30_M1_07.csv",header=TRUE)
NA1<-read.csv("NonAnem_codes_tu3_NonA_test_p30_F1_09.csv",header=TRUE)
NA2<-read.csv("NonAnem_codes_tu3_NonA_test_p30_F5_11.csv",header=TRUE)
NA3<-read.csv("NonAnem_codes_tu3_NonA_test_p30_M1_07.csv",header=TRUE)
A1<-A1[,-1]
A2<-A2[,-1]
A3<-A3[,-1]
NA1<-NA1[,-1]
NA2<-NA2[,-1]
NA3<-NA3[,-1]

C<-read.csv("Food_code_vs_WWEIA_code_without_duplicates.csv",header=TRUE)
C<-C[,-1]

Mer2<-function(X1,X2){
X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {   
if (is.na(match(X1["Food.code"][i,1],X2["Food.code"][,1]))==FALSE) {
I<-cbind(I,i)   
J<-cbind(J,match(X1["Food.code"][i,1],X2["Food.code"][,1]))}
}
X1X2<-cbind(X1[I,],X2[J,])
return(X1X2)}

A1C<-Mer2(A1,C)
A2C<-Mer2(A2,C)
A3C<-Mer2(A3,C)
NA1C<-Mer2(NA1,C)
NA2C<-Mer2(NA2,C)
NA3C<-Mer2(NA3,C)



write.csv(A1C,"A1C_F1_09_A.csv")
write.csv(A2C,"A2C_F5_11_A.csv")
write.csv(A3C,"A3C_M1_07_A.csv")
write.csv(NA1C,"NA1C_F1_09_NonA.csv")
write.csv(NA2C,"NA2C_F5_11_NonA.csv")
write.csv(NA3C,"NA3C_M1_07_NonA.csv")



A<-read.csv("A1C_F1_09_A.csv",header=TRUE)
A<-A[,-1]
categ<-nrow(A)
br_categ<-rep(0,16)
for (i in 1:nrow(A)) {
if ( (A[i,ncol(A)]>1001)&&(A[i,ncol(A)]<1823) ) {
categ[i]<-1
br_categ[1]<-br_categ[1]+1}

else {
if ( ( (A[i,ncol(A)]>2001)&&(A[i,ncol(A)]<2011) )||( (A[i,ncol(A)]>2601)&&(A[i,ncol(A)]<2609) ) ) {
categ[i]<-2
br_categ[2]<-br_categ[2]+1}
else {
if ((A[i,ncol(A)]>2201)&&(A[i,ncol(A)]<2207)) {
categ[i]<-3
br_categ[3]<-br_categ[3]+1}
else {
if ( (A[i,ncol(A)]>2401)&&(A[i,ncol(A)]<2405) ) {
categ[i]<-4
br_categ[4]<-br_categ[4]+1}
else {
if (A[i,ncol(A)]==2502) {
categ[i]<-5
br_categ[5]<-br_categ[5]+1}
else {
if (A[i,ncol(A)]==2802) {
categ[i]<-6
br_categ[6]<-br_categ[6]+1}
else {
if ( (A[i,ncol(A)]>2803)&&(A[i,ncol(A)]<2807) ) {
categ[i]<-7
br_categ[7]<-br_categ[7]+1}
else {
if ( (A[i,ncol(A)]>3001)&&(A[i,ncol(A)]<3803) ) {
categ[i]<-8
br_categ[8]<-br_categ[8]+1}
else {
if ( (A[i,ncol(A)]>4001)&&(A[i,ncol(A)]<4805) ) {
categ[i]<-9
br_categ[9]<-br_categ[9]+1}
else {
if ( (A[i,ncol(A)]>5001)&&(A[i,ncol(A)]<5807) ) {
categ[i]<-10
br_categ[10]<-br_categ[10]+1}
else {
if ( (A[i,ncol(A)]>6001)&&(A[i,ncol(A)]<6025) ) {
categ[i]<-11
br_categ[11]<-br_categ[11]+1}
else {
if ( (A[i,ncol(A)]>6401)&&(A[i,ncol(A)]<6807) ) {
categ[i]<-12
br_categ[12]<-br_categ[12]+1}
else {
if ( (A[i,ncol(A)]>7001)&&(A[i,ncol(A)]<7805) ) {
categ[i]<-13
br_categ[13]<-br_categ[13]+1}
else {
if ( (A[i,ncol(A)]>8001)&&(A[i,ncol(A)]<8013) ) {
categ[i]<-14
br_categ[14]<-br_categ[14]+1}
else {
if ( (A[i,ncol(A)]>8401)&&(A[i,ncol(A)]<8413) ) {
categ[i]<-15
br_categ[15]<-br_categ[15]+1}
else {
if ( (A[i,ncol(A)]>8801)&&(A[i,ncol(A)]<8807) ) {
categ[i]<-16
br_categ[16]<-br_categ[16]+1}
}}}}}}}}}}}}}}}}

B<-cbind(A,t(t(categ)))

write.csv(B,"B_A1C_F1_09_A.csv")
write.csv(br_categ,"br_categ_A1C_F1_09_A.csv")


A<-read.csv("NA3C_M1_07_NonA.csv",header=TRUE)
A<-A[,-1]
categ<-nrow(A)
br_categ<-rep(0,16)
for (i in 1:nrow(A)) {
if ( (A[i,ncol(A)]>1001)&&(A[i,ncol(A)]<1823) ) {
categ[i]<-1
br_categ[1]<-br_categ[1]+1}

else {
if ( ( (A[i,ncol(A)]>2001)&&(A[i,ncol(A)]<2011) )||( (A[i,ncol(A)]>2601)&&(A[i,ncol(A)]<2609) ) ) {
categ[i]<-2
br_categ[2]<-br_categ[2]+1}
else {
if ((A[i,ncol(A)]>2201)&&(A[i,ncol(A)]<2207)) {
categ[i]<-3
br_categ[3]<-br_categ[3]+1}
else {
if ( (A[i,ncol(A)]>2401)&&(A[i,ncol(A)]<2405) ) {
categ[i]<-4
br_categ[4]<-br_categ[4]+1}
else {
if (A[i,ncol(A)]==2502) {
categ[i]<-5
br_categ[5]<-br_categ[5]+1}
else {
if (A[i,ncol(A)]==2802) {
categ[i]<-6
br_categ[6]<-br_categ[6]+1}
else {
if ( (A[i,ncol(A)]>2803)&&(A[i,ncol(A)]<2807) ) {
categ[i]<-7
br_categ[7]<-br_categ[7]+1}
else {
if ( (A[i,ncol(A)]>3001)&&(A[i,ncol(A)]<3803) ) {
categ[i]<-8
br_categ[8]<-br_categ[8]+1}
else {
if ( (A[i,ncol(A)]>4001)&&(A[i,ncol(A)]<4805) ) {
categ[i]<-9
br_categ[9]<-br_categ[9]+1}
else {
if ( (A[i,ncol(A)]>5001)&&(A[i,ncol(A)]<5807) ) {
categ[i]<-10
br_categ[10]<-br_categ[10]+1}
else {
if ( (A[i,ncol(A)]>6001)&&(A[i,ncol(A)]<6025) ) {
categ[i]<-11
br_categ[11]<-br_categ[11]+1}
else {
if ( (A[i,ncol(A)]>6401)&&(A[i,ncol(A)]<6807) ) {
categ[i]<-12
br_categ[12]<-br_categ[12]+1}
else {
if ( (A[i,ncol(A)]>7001)&&(A[i,ncol(A)]<7805) ) {
categ[i]<-13
br_categ[13]<-br_categ[13]+1}
else {
if ( (A[i,ncol(A)]>8001)&&(A[i,ncol(A)]<8013) ) {
categ[i]<-14
br_categ[14]<-br_categ[14]+1}
else {
if ( (A[i,ncol(A)]>8401)&&(A[i,ncol(A)]<8413) ) {
categ[i]<-15
br_categ[15]<-br_categ[15]+1}
else {
if ( (A[i,ncol(A)]>8801)&&(A[i,ncol(A)]<8807) ) {
categ[i]<-16
br_categ[16]<-br_categ[16]+1}
}}}}}}}}}}}}}}}}

B<-cbind(A,t(t(categ)))

write.csv(B,"B_NA3C_M1_07_NonA.csv")
write.csv(br_categ,"br_categ_NA3C_M1_07_NonA.csv")

A<-read.csv("NA2C_F5_11_NonA.csv",header=TRUE)
A<-A[,-1]
categ<-nrow(A)
br_categ<-rep(0,16)
for (i in 1:nrow(A)) {
if ( (A[i,ncol(A)]>1001)&&(A[i,ncol(A)]<1823) ) {
categ[i]<-1
br_categ[1]<-br_categ[1]+1}

else {
if ( ( (A[i,ncol(A)]>2001)&&(A[i,ncol(A)]<2011) )||( (A[i,ncol(A)]>2601)&&(A[i,ncol(A)]<2609) ) ) {
categ[i]<-2
br_categ[2]<-br_categ[2]+1}
else {
if ((A[i,ncol(A)]>2201)&&(A[i,ncol(A)]<2207)) {
categ[i]<-3
br_categ[3]<-br_categ[3]+1}
else {
if ( (A[i,ncol(A)]>2401)&&(A[i,ncol(A)]<2405) ) {
categ[i]<-4
br_categ[4]<-br_categ[4]+1}
else {
if (A[i,ncol(A)]==2502) {
categ[i]<-5
br_categ[5]<-br_categ[5]+1}
else {
if (A[i,ncol(A)]==2802) {
categ[i]<-6
br_categ[6]<-br_categ[6]+1}
else {
if ( (A[i,ncol(A)]>2803)&&(A[i,ncol(A)]<2807) ) {
categ[i]<-7
br_categ[7]<-br_categ[7]+1}
else {
if ( (A[i,ncol(A)]>3001)&&(A[i,ncol(A)]<3803) ) {
categ[i]<-8
br_categ[8]<-br_categ[8]+1}
else {
if ( (A[i,ncol(A)]>4001)&&(A[i,ncol(A)]<4805) ) {
categ[i]<-9
br_categ[9]<-br_categ[9]+1}
else {
if ( (A[i,ncol(A)]>5001)&&(A[i,ncol(A)]<5807) ) {
categ[i]<-10
br_categ[10]<-br_categ[10]+1}
else {
if ( (A[i,ncol(A)]>6001)&&(A[i,ncol(A)]<6025) ) {
categ[i]<-11
br_categ[11]<-br_categ[11]+1}
else {
if ( (A[i,ncol(A)]>6401)&&(A[i,ncol(A)]<6807) ) {
categ[i]<-12
br_categ[12]<-br_categ[12]+1}
else {
if ( (A[i,ncol(A)]>7001)&&(A[i,ncol(A)]<7805) ) {
categ[i]<-13
br_categ[13]<-br_categ[13]+1}
else {
if ( (A[i,ncol(A)]>8001)&&(A[i,ncol(A)]<8013) ) {
categ[i]<-14
br_categ[14]<-br_categ[14]+1}
else {
if ( (A[i,ncol(A)]>8401)&&(A[i,ncol(A)]<8413) ) {
categ[i]<-15
br_categ[15]<-br_categ[15]+1}
else {
if ( (A[i,ncol(A)]>8801)&&(A[i,ncol(A)]<8807) ) {
categ[i]<-16
br_categ[16]<-br_categ[16]+1}
}}}}}}}}}}}}}}}}

B<-cbind(A,t(t(categ)))

write.csv(B,"B_NA2C_F5_11_NonA.csv")
write.csv(br_categ,"br_categ_NA2C_F5_11_NonA.csv")

A<-read.csv("NA1C_F1_09_NonA.csv",header=TRUE)
A<-A[,-1]
categ<-nrow(A)
br_categ<-rep(0,16)
for (i in 1:nrow(A)) {
if ( (A[i,ncol(A)]>1001)&&(A[i,ncol(A)]<1823) ) {
categ[i]<-1
br_categ[1]<-br_categ[1]+1}

else {
if ( ( (A[i,ncol(A)]>2001)&&(A[i,ncol(A)]<2011) )||( (A[i,ncol(A)]>2601)&&(A[i,ncol(A)]<2609) ) ) {
categ[i]<-2
br_categ[2]<-br_categ[2]+1}
else {
if ((A[i,ncol(A)]>2201)&&(A[i,ncol(A)]<2207)) {
categ[i]<-3
br_categ[3]<-br_categ[3]+1}
else {
if ( (A[i,ncol(A)]>2401)&&(A[i,ncol(A)]<2405) ) {
categ[i]<-4
br_categ[4]<-br_categ[4]+1}
else {
if (A[i,ncol(A)]==2502) {
categ[i]<-5
br_categ[5]<-br_categ[5]+1}
else {
if (A[i,ncol(A)]==2802) {
categ[i]<-6
br_categ[6]<-br_categ[6]+1}
else {
if ( (A[i,ncol(A)]>2803)&&(A[i,ncol(A)]<2807) ) {
categ[i]<-7
br_categ[7]<-br_categ[7]+1}
else {
if ( (A[i,ncol(A)]>3001)&&(A[i,ncol(A)]<3803) ) {
categ[i]<-8
br_categ[8]<-br_categ[8]+1}
else {
if ( (A[i,ncol(A)]>4001)&&(A[i,ncol(A)]<4805) ) {
categ[i]<-9
br_categ[9]<-br_categ[9]+1}
else {
if ( (A[i,ncol(A)]>5001)&&(A[i,ncol(A)]<5807) ) {
categ[i]<-10
br_categ[10]<-br_categ[10]+1}
else {
if ( (A[i,ncol(A)]>6001)&&(A[i,ncol(A)]<6025) ) {
categ[i]<-11
br_categ[11]<-br_categ[11]+1}
else {
if ( (A[i,ncol(A)]>6401)&&(A[i,ncol(A)]<6807) ) {
categ[i]<-12
br_categ[12]<-br_categ[12]+1}
else {
if ( (A[i,ncol(A)]>7001)&&(A[i,ncol(A)]<7805) ) {
categ[i]<-13
br_categ[13]<-br_categ[13]+1}
else {
if ( (A[i,ncol(A)]>8001)&&(A[i,ncol(A)]<8013) ) {
categ[i]<-14
br_categ[14]<-br_categ[14]+1}
else {
if ( (A[i,ncol(A)]>8401)&&(A[i,ncol(A)]<8413) ) {
categ[i]<-15
br_categ[15]<-br_categ[15]+1}
else {
if ( (A[i,ncol(A)]>8801)&&(A[i,ncol(A)]<8807) ) {
categ[i]<-16
br_categ[16]<-br_categ[16]+1}
}}}}}}}}}}}}}}}}

B<-cbind(A,t(t(categ)))

write.csv(B,"B_NA1C_F1_09_NonA.csv")
write.csv(br_categ,"br_categ_NA1C_F1_09_NonA.csv")

A<-read.csv("A3C_M1_07_A.csv",header=TRUE)
A<-A[,-1]
categ<-nrow(A)
br_categ<-rep(0,16)
for (i in 1:nrow(A)) {
if ( (A[i,ncol(A)]>1001)&&(A[i,ncol(A)]<1823) ) {
categ[i]<-1
br_categ[1]<-br_categ[1]+1}

else {
if ( ( (A[i,ncol(A)]>2001)&&(A[i,ncol(A)]<2011) )||( (A[i,ncol(A)]>2601)&&(A[i,ncol(A)]<2609) ) ) {
categ[i]<-2
br_categ[2]<-br_categ[2]+1}
else {
if ((A[i,ncol(A)]>2201)&&(A[i,ncol(A)]<2207)) {
categ[i]<-3
br_categ[3]<-br_categ[3]+1}
else {
if ( (A[i,ncol(A)]>2401)&&(A[i,ncol(A)]<2405) ) {
categ[i]<-4
br_categ[4]<-br_categ[4]+1}
else {
if (A[i,ncol(A)]==2502) {
categ[i]<-5
br_categ[5]<-br_categ[5]+1}
else {
if (A[i,ncol(A)]==2802) {
categ[i]<-6
br_categ[6]<-br_categ[6]+1}
else {
if ( (A[i,ncol(A)]>2803)&&(A[i,ncol(A)]<2807) ) {
categ[i]<-7
br_categ[7]<-br_categ[7]+1}
else {
if ( (A[i,ncol(A)]>3001)&&(A[i,ncol(A)]<3803) ) {
categ[i]<-8
br_categ[8]<-br_categ[8]+1}
else {
if ( (A[i,ncol(A)]>4001)&&(A[i,ncol(A)]<4805) ) {
categ[i]<-9
br_categ[9]<-br_categ[9]+1}
else {
if ( (A[i,ncol(A)]>5001)&&(A[i,ncol(A)]<5807) ) {
categ[i]<-10
br_categ[10]<-br_categ[10]+1}
else {
if ( (A[i,ncol(A)]>6001)&&(A[i,ncol(A)]<6025) ) {
categ[i]<-11
br_categ[11]<-br_categ[11]+1}
else {
if ( (A[i,ncol(A)]>6401)&&(A[i,ncol(A)]<6807) ) {
categ[i]<-12
br_categ[12]<-br_categ[12]+1}
else {
if ( (A[i,ncol(A)]>7001)&&(A[i,ncol(A)]<7805) ) {
categ[i]<-13
br_categ[13]<-br_categ[13]+1}
else {
if ( (A[i,ncol(A)]>8001)&&(A[i,ncol(A)]<8013) ) {
categ[i]<-14
br_categ[14]<-br_categ[14]+1}
else {
if ( (A[i,ncol(A)]>8401)&&(A[i,ncol(A)]<8413) ) {
categ[i]<-15
br_categ[15]<-br_categ[15]+1}
else {
if ( (A[i,ncol(A)]>8801)&&(A[i,ncol(A)]<8807) ) {
categ[i]<-16
br_categ[16]<-br_categ[16]+1}
}}}}}}}}}}}}}}}}

B<-cbind(A,t(t(categ)))

write.csv(B,"B_A3C_M1_07_A.csv")
write.csv(br_categ,"br_categ_A3C_M1_07_A.csv")

A<-read.csv("A2C_F5_11_A.csv",header=TRUE)
A<-A[,-1]
categ<-nrow(A)
br_categ<-rep(0,16)
for (i in 1:nrow(A)) {
if ( (A[i,ncol(A)]>1001)&&(A[i,ncol(A)]<1823) ) {
categ[i]<-1
br_categ[1]<-br_categ[1]+1}

else {
if ( ( (A[i,ncol(A)]>2001)&&(A[i,ncol(A)]<2011) )||( (A[i,ncol(A)]>2601)&&(A[i,ncol(A)]<2609) ) ) {
categ[i]<-2
br_categ[2]<-br_categ[2]+1}
else {
if ((A[i,ncol(A)]>2201)&&(A[i,ncol(A)]<2207)) {
categ[i]<-3
br_categ[3]<-br_categ[3]+1}
else {
if ( (A[i,ncol(A)]>2401)&&(A[i,ncol(A)]<2405) ) {
categ[i]<-4
br_categ[4]<-br_categ[4]+1}
else {
if (A[i,ncol(A)]==2502) {
categ[i]<-5
br_categ[5]<-br_categ[5]+1}
else {
if (A[i,ncol(A)]==2802) {
categ[i]<-6
br_categ[6]<-br_categ[6]+1}
else {
if ( (A[i,ncol(A)]>2803)&&(A[i,ncol(A)]<2807) ) {
categ[i]<-7
br_categ[7]<-br_categ[7]+1}
else {
if ( (A[i,ncol(A)]>3001)&&(A[i,ncol(A)]<3803) ) {
categ[i]<-8
br_categ[8]<-br_categ[8]+1}
else {
if ( (A[i,ncol(A)]>4001)&&(A[i,ncol(A)]<4805) ) {
categ[i]<-9
br_categ[9]<-br_categ[9]+1}
else {
if ( (A[i,ncol(A)]>5001)&&(A[i,ncol(A)]<5807) ) {
categ[i]<-10
br_categ[10]<-br_categ[10]+1}
else {
if ( (A[i,ncol(A)]>6001)&&(A[i,ncol(A)]<6025) ) {
categ[i]<-11
br_categ[11]<-br_categ[11]+1}
else {
if ( (A[i,ncol(A)]>6401)&&(A[i,ncol(A)]<6807) ) {
categ[i]<-12
br_categ[12]<-br_categ[12]+1}
else {
if ( (A[i,ncol(A)]>7001)&&(A[i,ncol(A)]<7805) ) {
categ[i]<-13
br_categ[13]<-br_categ[13]+1}
else {
if ( (A[i,ncol(A)]>8001)&&(A[i,ncol(A)]<8013) ) {
categ[i]<-14
br_categ[14]<-br_categ[14]+1}
else {
if ( (A[i,ncol(A)]>8401)&&(A[i,ncol(A)]<8413) ) {
categ[i]<-15
br_categ[15]<-br_categ[15]+1}
else {
if ( (A[i,ncol(A)]>8801)&&(A[i,ncol(A)]<8807) ) {
categ[i]<-16
br_categ[16]<-br_categ[16]+1}
}}}}}}}}}}}}}}}}

B<-cbind(A,t(t(categ)))

write.csv(B,"B_A2C_F5_11_A.csv")
write.csv(br_categ,"br_categ_A2C_F5_11_A.csv")

D<-read.csv("Food_classes_shorter.csv",header=TRUE)
A1<-read.csv("br_categ_A1C_F1_09_A.csv",header=TRUE)
A1<-A1[,-1]
NonA1<-read.csv("br_categ_NA1C_F1_09_NonA.csv",header=TRUE)
NonA1<-NonA1[,-1]

D<-read.csv("Food_classes_shorter.csv",header=TRUE)
A2<-read.csv("br_categ_A2C_F5_11_A.csv",header=TRUE)
A2<-A2[,-1]
NonA2<-read.csv("br_categ_NA2C_F5_11_NonA.csv",header=TRUE)
NonA2<-NonA2[,-1]

A3<-read.csv("br_categ_A3C_M1_07_A.csv",header=TRUE)
A3<-A3[,-1]
NonA3<-read.csv("br_categ_NA3C_M1_07_NonA.csv",header=TRUE)
NonA3<-NonA3[,-1]

A<-A1+A2+A3
NonA<-NonA1+NonA2+NonA3

Mat<-cbind(D,t(t(NonA)),t(t(A)))
colnames(Mat)[3]<-"NonA"
colnames(Mat)[4]<-"A"

Mat1<-cbind(Mat,t(t(A/(1341/1276))))
colnames(Mat1)[5]<-"Norm_A"
NonA_perc<-100*NonA/(NonA+Mat1[,5])
A_perc<-100-NonA_perc
Mat2<-cbind(Mat1,A_perc,NonA_perc)
write.csv(Mat2,"Food_classes_A_perc.csv")

D<-read.csv("Food_classes_shorter.csv",header=TRUE)
A1<-read.csv("B_A1C_F1_09_A.csv",header=TRUE)
A1<-A1[,-1]
NonA1<-read.csv("B_NA1C_F1_09_NonA.csv",header=TRUE)
NonA1<-NonA1[,-1]

A2<-read.csv("B_A2C_F5_11_A.csv",header=TRUE)
A2<-A2[,-1]
NonA2<-read.csv("B_NA2C_F5_11_NonA.csv",header=TRUE)
NonA2<-NonA2[,-1]

A3<-read.csv("B_A3C_M1_07_A.csv",header=TRUE)
A3<-A3[,-1]
NonA3<-read.csv("B_NA3C_M1_07_NonA.csv",header=TRUE)
NonA3<-NonA3[,-1]

NonA<-rbind(NonA1,NonA2,NonA3)
A<-rbind(A1,A2,A3)

for (i in nrow(A):1) {
if (any(is.na(A[i,]))==TRUE) {
A<-A[-i,]}}

for (i in nrow(NonA):1) {
if (any(is.na(NonA[i,]))==TRUE) {
NonA<-NonA[-i,]}}

A_ord<-A[order(A[,ncol(A)]),]
NonA_ord<-NonA[order(NonA[,ncol(NonA)]),]

NonA_ord<-NonA_ord[,-c(67:69)]
M<-rbind(A_ord,NonA_ord)
M_ord<-M[order(M[,ncol(M)]),]

t<-rep(0,16)
sd_t<-rep(0,16)
EM<-rep(0,16)
len<-rep(0,16)
for (i in 1:16) {
t[i]<-mean(A_ord[min(which(match(A_ord[,ncol(A_ord)],i)==1)):max(which(match(A_ord[,ncol(A_ord)],i)==1)),"min_FPro"]) 
sd_t[i]<-sd(A_ord[min(which(match(A_ord[,ncol(A_ord)],i)==1)):max(which(match(A_ord[,ncol(A_ord)],i)==1)),"min_FPro"]) 
len[i]<-length(A_ord[min(which(match(A_ord[,ncol(A_ord)],i)==1)):max(which(match(A_ord[,ncol(A_ord)],i)==1)),"min_FPro"]) 
EM[i]<-1.96*sd_t[i]/sqrt(len[i])
}

A_t<-t
A_EM<-EM

t<-rep(0,16)
sd_t<-rep(0,16)
EM<-rep(0,16)
len<-rep(0,16)
for (i in 1:16) {
t[i]<-mean(NonA_ord[min(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)):max(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)),"min_FPro"]) 
sd_t[i]<-sd(NonA_ord[min(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)):max(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)),"min_FPro"]) 
len[i]<-length(NonA_ord[min(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)):max(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)),"min_FPro"]) 
EM[i]<-1.96*sd_t[i]/sqrt(len[i])
}


NA_t<-t
NA_EM<-EM

t<-rep(0,16)
sd_t<-rep(0,16)
EM<-rep(0,16)
len<-rep(0,16)
for (i in 1:16) {
t[i]<-mean(M_ord[min(which(match(M_ord[,ncol(M_ord)],i)==1)):max(which(match(M_ord[,ncol(M_ord)],i)==1)),"min_FPro"]) 
sd_t[i]<-sd(M_ord[min(which(match(M_ord[,ncol(M_ord)],i)==1)):max(which(match(M_ord[,ncol(M_ord)],i)==1)),"min_FPro"]) 
len[i]<-length(M_ord[min(which(match(M_ord[,ncol(M_ord)],i)==1)):max(which(match(M_ord[,ncol(M_ord)],i)==1)),"min_FPro"]) 
EM[i]<-1.96*sd_t[i]/sqrt(len[i])
}

M_t<-t
M_EM<-EM

Mat<-read.csv("Food_classes_A_perc.csv",header=TRUE)
Mat2<-cbind(Mat,t(t(A_t)),t(t(NA_t)),t(t(M_t)),t(t(A_EM)),t(t(NA_EM)),t(t(M_EM)))
colnames(Mat2)[9]<-"m(Fpro_A)"
colnames(Mat2)[10]<-"m(Fpro_NA)"
colnames(Mat2)[11]<-"m(Fpro)"
colnames(Mat2)[12]<-"EM(Fpro_A)"
colnames(Mat2)[13]<-"EM(Fpro_NA)"
colnames(Mat2)[14]<-"EM(Fpro)"
Mat3<-Mat2[order(Mat2[,ncol(Mat2)-3]),]
write.csv(Mat3,"Food_classes_A_perc_sorted_by_Fpro.csv")



A<-read.csv("Food_classes_A_perc_sorted_by_Fpro.csv")
A<-A[,-1]
A<-A[,-1]

dev.new(width=15.86, height=10.2, unit="in")
frame()
plot.window(xlim=c(-0.5,2.6), ylim=c(-12.5,1.5))
#text(0.7,1.5,"Consumption of food categories by anemic and non-anemic participants",cex=1.4)

segments(x0=0.1,y0=1.2,x1=0.1,y1=-10.8,lwd=1)
segments(x0=0.1,y0=-10.72,x1=1.5,y1=-10.72)

for (i in 0:10) {
segments(x0=0.1+i*0.14,y0=1.2,x1=0.1+i*0.14,y1=-10.8)
text(0.1+i*0.14,-11,paste(i*10,"%"))
}


library(plotrix)

text(-0.03,1.6,"Food Category",cex=1.2,pos=2)
text(-0.03,0.9,"Overall",cex=1.2,pos=2)
text(0.1,1.6,"N",cex=1.2,pos=2)
text(0.1,0.9,paste(sum(A[,3])+sum(A[,4])),cex=1.2,pos=2)
text(0.1+1.4*0.2,1.6,"f(A)",cex=1.2)
text(0.1+1.4*0.8,1.6,"f(NA)",cex=1.2)
text(1.6+0.4,1.6,"Fpro",cex=1.2)


rectFill(x1=0.1, y1=0.9-0.2, x2=0.1+sum(A[,4])/(sum(A[,3])+sum(A[,4]))*1.4, y2=0.9+0.2, fg="pink", bg = "pink",pch=0,pch.col="pink")
rectFill(x1=0.1+sum(A[,4])/(sum(A[,3])+sum(A[,4]))*1.4, y1=0.9-0.2, x2=1.5, y2=0.9+0.2, fg="cyan", bg = "cyan",pch=0,pch.col="cyan")
text(0.1+0.2*1.4,0.9,paste(round(100*sum(A[,4])/(sum(A[,3])+sum(A[,4])),1) ) )
text(0.1+0.8*1.4,0.9,paste(round(100*sum(A[,3])/(sum(A[,3])+sum(A[,4])),1) ) )



for (i in 1:nrow(A)) {
text(-0.03,0.9-0.7*i,paste(A[i,1]),cex=1.2,pos=2)
text(0.1,0.9-0.7*i,paste(A[i,3]+A[i,4]),cex=1.2,pos=2)
rectFill(x1=0.1, y1=0.9-0.2-0.7*i, x2=0.1+(A[i,4]/(A[i,3]+A[i,4]))*1.4, y2=0.9+0.2-0.7*i, fg="pink", bg = "pink",pch=0,pch.col="pink")
rectFill(x1=0.1+(A[i,4]/(A[i,3]+A[i,4]))*1.4, y1=0.9-0.2-0.7*i, x2=1.5, y2=0.9+0.2-0.7*i, fg="cyan", bg = "cyan",pch=0,pch.col="cyan")
text(0.1+0.2*1.4,0.9-0.7*i,paste(round(100*A[i,4]/(A[i,3]+A[i,4]),1) ) )
text(0.1+0.8*1.4,0.9-0.7*i,paste(round(100*A[i,3]/(A[i,3]+A[i,4]),1) ) )
}


rectFill(x1=0.95-0.3, y1=-11.7, x2=1.002-0.3, y2=-11.3, fg="pink", bg = "pink",pch=0,pch.col="pink")
rectFill(x1=0.9, y1=-11.7, x2=0.952, y2=-11.3, fg="cyan", bg = "cyan",pch=0,pch.col="cyan")
text(1.075-0.3,-11.53,"A",cex=1.2,pos=2)
text(1.354-0.3,-11.53,"NA",cex=1.2,pos=2)

segments(x0=1.6,y0=1.2,x1=1.6,y1=-10.8,lwd=1)
segments(x0=1.6,y0=-10.72,x1=2.6,y1=-10.72)
for (i in 0:10) {
segments(x0=1.6+i*0.1,y0=1.2,x1=1.6+i*0.1,y1=-10.8)
text(1.6+i*0.1,-11,paste(i/10))
}

rectFill(x1=1.6, y1=0.9, x2=1.6+0.7475079, y2=0.9+0.2, fg="red", bg = "red",pch=0,pch.col="red")
segments(x0=1.6+(0.7475079-0.01383459),y0=0.9+0.1,x1=1.6+(0.7475079+0.01383459),y1=0.9+0.1,col="black")
segments(x0=1.6+(0.7475079-0.01383459),y0=0.9,x1=1.6+(0.7475079-0.01383459),y1=0.9+0.2,col="black")
segments(x0=1.6+(0.7475079+0.01383459),y0=0.9,x1=1.6+(0.7475079+0.01383459),y1=0.9+0.2,col="black")
rectFill(x1=1.6, y1=0.9-0.2, x2=1.6+0.6682661, y2=0.9, fg="darkgreen", bg = "darkgreen",pch=0,pch.col="darkgreen")
segments(x0=1.6+(0.6682661-0.01660669),y0=0.9-0.1,x1=1.6+(0.6682661+0.01660669),y1=0.9-0.1,col="black")
segments(x0=1.6+(0.6682661-0.01660669),y0=0.9,x1=1.6+(0.6682661-0.01660669),y1=0.9-0.2,col="black")
segments(x0=1.6+(0.6682661+0.01660669),y0=0.9,x1=1.6+(0.6682661+0.01660669),y1=0.9-0.2,col="black")


for (i in 1:nrow(A)) {
rectFill(x1=1.6, y1=0.9-0.7*i, x2=1.6+A[i,8], y2=0.9-0.7*i+0.2, fg="red", bg = "red",pch=0,pch.col="red")
segments(x0=1.6+(A[i,8]-A[i,11]),y0=0.9+0.1-0.7*i,x1=1.6+(A[i,8]+A[i,11]),y1=0.9+0.1-0.7*i,col="black")
segments(x0=1.6+(A[i,8]-A[i,11]),y0=0.9-0.7*i,x1=1.6+(A[i,8]-A[i,11]),y1=0.9+0.2-0.7*i,col="black")
segments(x0=1.6+(A[i,8]+A[i,11]),y0=0.9-0.7*i,x1=1.6+(A[i,8]+A[i,11]),y1=0.9+0.2-0.7*i,col="black")
rectFill(x1=1.6, y1=0.9-0.2-0.7*i, x2=1.6+A[i,9], y2=0.9-0.7*i, fg="darkgreen", bg = "darkgreen",pch=0,pch.col="darkgreen")
segments(x0=1.6+(A[i,9]-A[i,12]),y0=0.9-0.1-0.7*i,x1=1.6+(A[i,9]+A[i,12]),y1=0.9-0.1-0.7*i,col="black")
segments(x0=1.6+(A[i,9]-A[i,12]),y0=0.9-0.7*i,x1=1.6+(A[i,9]-A[i,12]),y1=0.9-0.2-0.7*i,col="black")
segments(x0=1.6+(A[i,9]+A[i,12]),y0=0.9-0.7*i,x1=1.6+(A[i,9]+A[i,12]),y1=0.9-0.2-0.7*i,col="black")}

rectFill(x1=2.13, y1=-11.7, x2=2.182, y2=-11.3, fg="red", bg = "red",pch=0,pch.col="red")
rectFill(x1=1.9, y1=-11.7, x2=1.952, y2=-11.3, fg="darkgreen", bg = "darkgreen",pch=0,pch.col="darkgreen")
text(2.054,-11.53,"NA",cex=1.2,pos=2)
text(2.248,-11.53,"A",cex=1.2,pos=2)



segments(x0=1.57,y0=-11.37,x1=1.57,y1=0.5,lty="dashed",col="purple")
segments(x0=1.52,y0=0.5,x1=1.62,y1=0.5,lty="dashed",col="purple")
segments(x0=1.56,y0=-11.37+0.2,x1=1.57,y1=-11.37,lty="dashed",col="purple")
segments(x0=1.58,y0=-11.37+0.2,x1=1.57,y1=-11.37,lty="dashed",col="purple")
text(1.37,-11.58,"Increasing m(Fpro)",cex=1.1,pos=4,col="purple")
segments(x0=2.34,y0=-11.5,x1=2.4,y1=-11.5)
segments(x0=2.34,y0=-11.35,x1=2.34,y1=-11.65)
segments(x0=2.4,y0=-11.35,x1=2.4,y1=-11.65)
text(2.4,-11.53,"95% CI",cex=1.1,pos=4,col="black")






