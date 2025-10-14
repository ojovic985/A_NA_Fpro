#R

#Working directory in R must be '/A_NA_FPro'

source("getting_foodcodes_From_C1C2_to_F15-49_group_9_and_F50+_group_23_p30.R")


A1<-read.csv("Anem_codes_tu3_NonA_test_p30_F1_09.csv",header=TRUE)
A2<-read.csv("Anem_codes_tu3_NonA_test_p30_F5_11.csv",header=TRUE)
NA1<-read.csv("NonAnem_codes_tu3_NonA_test_p30_F1_09.csv",header=TRUE)
NA2<-read.csv("NonAnem_codes_tu3_NonA_test_p30_F5_11.csv",header=TRUE)
A1<-A1[,-1]
A2<-A2[,-1]
NA1<-NA1[,-1]
NA2<-NA2[,-1]

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
NA1C<-Mer2(NA1,C)
NA2C<-Mer2(NA2,C)


write.csv(A1C,"A1C_F1_09_A.csv")
write.csv(A2C,"A2C_F5_11_A.csv")
write.csv(NA1C,"NA1C_F1_09_NonA.csv")
write.csv(NA2C,"NA2C_F5_11_NonA.csv")


A<-read.csv("A1C_F1_09_A.csv",header=TRUE)
A<-A[,-1]
categ<-nrow(A)
br_categ<-rep(0,17)
for (i in 1:nrow(A)) {
if ( (A[i,ncol(A)]>1001)&&(A[i,ncol(A)]<1823) ) {
categ[i]<-1
br_categ[1]<-br_categ[1]+1}

else {
if ( ( (A[i,ncol(A)]>2005)&&(A[i,ncol(A)]<2011) )||( (A[i,ncol(A)]>2601)&&(A[i,ncol(A)]<2609) ) ) {
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
else {
if ( (A[i,ncol(A)]>2001)&&(A[i,ncol(A)]<2005) ) {
categ[i]<-17
br_categ[17]<-br_categ[17]+1}
}}}}}}}}}}}}}}}}}

B<-cbind(A,t(t(categ)))

write.csv(B,"B_A1C_F1_09_A_G0904.csv")
write.csv(br_categ,"br_categ_A1C_F1_09_A_G0904.csv")




A<-read.csv("NA1C_F1_09_NonA.csv",header=TRUE)
A<-A[,-1]
categ<-nrow(A)
br_categ<-rep(0,17)
for (i in 1:nrow(A)) {
if ( (A[i,ncol(A)]>1001)&&(A[i,ncol(A)]<1823) ) {
categ[i]<-1
br_categ[1]<-br_categ[1]+1}

else {
if ( ( (A[i,ncol(A)]>2005)&&(A[i,ncol(A)]<2011) )||( (A[i,ncol(A)]>2601)&&(A[i,ncol(A)]<2609) ) ) {
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
else {
if ( (A[i,ncol(A)]>2001)&&(A[i,ncol(A)]<2005) ) {
categ[i]<-17
br_categ[17]<-br_categ[17]+1}
}}}}}}}}}}}}}}}}}

B<-cbind(A,t(t(categ)))

write.csv(B,"B_NA1C_F1_09_NonA_G0904.csv")
write.csv(br_categ,"br_categ_NA1C_F1_09_NonA_G0904.csv")



A<-read.csv("A2C_F5_11_A.csv",header=TRUE)
A<-A[,-1]
categ<-nrow(A)
br_categ<-rep(0,17)
for (i in 1:nrow(A)) {
if ( (A[i,ncol(A)]>1001)&&(A[i,ncol(A)]<1823) ) {
categ[i]<-1
br_categ[1]<-br_categ[1]+1}

else {
if ( ( (A[i,ncol(A)]>2005)&&(A[i,ncol(A)]<2011) )||( (A[i,ncol(A)]>2601)&&(A[i,ncol(A)]<2609) ) ) {
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
else {
if ( (A[i,ncol(A)]>2001)&&(A[i,ncol(A)]<2005) ) {
categ[i]<-17
br_categ[17]<-br_categ[17]+1}
}}}}}}}}}}}}}}}}}

B<-cbind(A,t(t(categ)))


write.csv(B,"B_A2C_F5_11_A_G0904.csv")
write.csv(br_categ,"br_categ_A2C_F5_11_A_G0904.csv")


A<-read.csv("NA2C_F5_11_NonA.csv",header=TRUE)
A<-A[,-1]
categ<-nrow(A)
br_categ<-rep(0,17)
for (i in 1:nrow(A)) {
if ( (A[i,ncol(A)]>1001)&&(A[i,ncol(A)]<1823) ) {
categ[i]<-1
br_categ[1]<-br_categ[1]+1}

else {
if ( ( (A[i,ncol(A)]>2005)&&(A[i,ncol(A)]<2011) )||( (A[i,ncol(A)]>2601)&&(A[i,ncol(A)]<2609) ) ) {
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
else {
if ( (A[i,ncol(A)]>2001)&&(A[i,ncol(A)]<2005) ) {
categ[i]<-17
br_categ[17]<-br_categ[17]+1}
}}}}}}}}}}}}}}}}}

B<-cbind(A,t(t(categ)))

write.csv(B,"B_NA2C_F5_11_NonA_G0904.csv")
write.csv(br_categ,"br_categ_NA2C_F5_11_NonA_G0904.csv")

D<-read.csv("Food_classes_shorter_G0904.csv",header=TRUE)
A1<-read.csv("br_categ_A1C_F1_09_A_G0904.csv",header=TRUE)
A1<-A1[,-1]
NonA1<-read.csv("br_categ_NA1C_F1_09_NonA_G0904.csv",header=TRUE)
NonA1<-NonA1[,-1]

A2<-read.csv("br_categ_A2C_F5_11_A_G0904.csv",header=TRUE)
A2<-A2[,-1]
NonA2<-read.csv("br_categ_NA2C_F5_11_NonA_G0904.csv",header=TRUE)
NonA2<-NonA2[,-1]


A<-A1+A2
NonA<-NonA1+NonA2

Mat<-cbind(D,t(t(NonA)),t(t(A)))
colnames(Mat)[3]<-"NonA"
colnames(Mat)[4]<-"A"
Mat1<-cbind(Mat,t(t(A/(sum(Mat[,4])/sum(Mat[,3])))))
colnames(Mat1)[5]<-"Norm_A"
NonA_perc<-100*NonA/(NonA+A)
A_perc<-100-NonA_perc
Mat2<-cbind(Mat1,A_perc,NonA_perc)
write.csv(Mat2,"Food_classes_A_perc_G2404.csv")


D<-read.csv("Food_classes_shorter_G0904.csv",header=TRUE)
A1<-read.csv("B_A1C_F1_09_A_G0904.csv",header=TRUE)
A1<-A1[,-1]
NonA1<-read.csv("B_NA1C_F1_09_NonA_G0904.csv",header=TRUE)
NonA1<-NonA1[,-1]

A2<-read.csv("B_A2C_F5_11_A_G0904.csv",header=TRUE)
A2<-A2[,-1]
NonA2<-read.csv("B_NA2C_F5_11_NonA_G0904.csv",header=TRUE)
NonA2<-NonA2[,-1]


NonA<-rbind(NonA1,NonA2)
A<-rbind(A1,A2)

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
write.csv(M_ord,"2404_without_M1_All_food_codes_from_WWEIA_analysis_Incl_dup.csv")
M_ord_minDup<-M_ord[!duplicated(M_ord[,1]), ]
write.csv(M_ord_minDup,"2404_without_M1_All_food_codes_from_WWEIA_analysis_excl_duplicates.csv")


t<-rep(0,17)
sd_t<-rep(0,17)
EM<-rep(0,17)
len<-rep(0,17)
for (i in 1:17) {
t[i]<-mean(A_ord[min(which(match(A_ord[,ncol(A_ord)],i)==1)):max(which(match(A_ord[,ncol(A_ord)],i)==1)),"min_FPro"]) 
sd_t[i]<-sd(A_ord[min(which(match(A_ord[,ncol(A_ord)],i)==1)):max(which(match(A_ord[,ncol(A_ord)],i)==1)),"min_FPro"]) 
len[i]<-length(A_ord[min(which(match(A_ord[,ncol(A_ord)],i)==1)):max(which(match(A_ord[,ncol(A_ord)],i)==1)),"min_FPro"]) 
EM[i]<-1.96*sd_t[i]/sqrt(len[i])
}

A_t<-t
A_EM<-EM

t<-rep(0,17)
sd_t<-rep(0,17)
EM<-rep(0,17)
len<-rep(0,17)
for (i in 1:17) {
t[i]<-mean(NonA_ord[min(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)):max(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)),"min_FPro"]) 
sd_t[i]<-sd(NonA_ord[min(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)):max(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)),"min_FPro"]) 
len[i]<-length(NonA_ord[min(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)):max(which(match(NonA_ord[,ncol(NonA_ord)],i)==1)),"min_FPro"]) 
EM[i]<-1.96*sd_t[i]/sqrt(len[i])
}


NA_t<-t
NA_EM<-EM

t<-rep(0,17)
sd_t<-rep(0,17)
EM<-rep(0,17)
len<-rep(0,17)
for (i in 1:17) {
t[i]<-mean(M_ord[min(which(match(M_ord[,ncol(M_ord)],i)==1)):max(which(match(M_ord[,ncol(M_ord)],i)==1)),"min_FPro"]) 
sd_t[i]<-sd(M_ord[min(which(match(M_ord[,ncol(M_ord)],i)==1)):max(which(match(M_ord[,ncol(M_ord)],i)==1)),"min_FPro"]) 
len[i]<-length(M_ord[min(which(match(M_ord[,ncol(M_ord)],i)==1)):max(which(match(M_ord[,ncol(M_ord)],i)==1)),"min_FPro"]) 
EM[i]<-1.96*sd_t[i]/sqrt(len[i])
}

M_t<-t
M_EM<-EM


Mat<-read.csv("Food_classes_A_perc_G2404.csv",header=TRUE)
Mat2<-cbind(Mat,t(t(A_t)),t(t(NA_t)),t(t(M_t)),t(t(A_EM)),t(t(NA_EM)),t(t(M_EM)))
colnames(Mat2)[9]<-"m(Fpro_A)"
colnames(Mat2)[10]<-"m(Fpro_NA)"
colnames(Mat2)[11]<-"m(Fpro)"
colnames(Mat2)[12]<-"EM(Fpro_A)"
colnames(Mat2)[13]<-"EM(Fpro_NA)"
colnames(Mat2)[14]<-"EM(Fpro)"
Mat3<-Mat2[order(Mat2[,ncol(Mat2)-3]),]
write.csv(Mat3,"Food_classes_A_perc_sorted_by_Fpro_G2404_without_M1.csv") # First KEY file for final input for Figure 1


D<-read.csv("Food_classes_shorter_G0904.csv",header=TRUE)
A1<-read.csv("B_A1C_F1_09_A_G0904.csv",header=TRUE)
A1<-A1[,-1]
NonA1<-read.csv("B_NA1C_F1_09_NonA_G0904.csv",header=TRUE)
NonA1<-NonA1[,-1]

A2<-read.csv("B_A2C_F5_11_A_G0904.csv",header=TRUE)
A2<-A2[,-1]
NonA2<-read.csv("B_NA2C_F5_11_NonA_G0904.csv",header=TRUE)
NonA2<-NonA2[,-1]


NonA<-rbind(NonA1,NonA2)
A<-rbind(A1,A2)

for (i in nrow(A):1) {
if (any(is.na(A[i,]))==TRUE) {
A<-A[-i,]}}

for (i in nrow(NonA):1) {
if (any(is.na(NonA[i,]))==TRUE) {
NonA<-NonA[-i,]}}

A_ord<-A[order(A[,ncol(A)]),]
NonA_ord<-NonA[order(NonA[,ncol(NonA)]),]

NonA_ord<-NonA_ord[,-c(67:69)]

A_ord_2<-cbind(A_ord,t(t(rep(1,dim(A_ord)[1]))))
NonA_ord_2<-cbind(NonA_ord,t(t(rep(0,dim(NonA_ord)[1]))))
colnames(A_ord_2)[ncol(A_ord_2)]<-"A_status"
colnames(NonA_ord_2)[ncol(NonA_ord_2)]<-"A_status"

Food_Compass_2_score<-read.csv("Food_Compass_2_score.csv")

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
A_ord_2_score<-Mer2(A_ord_2,Food_Compass_2_score)
A_ord_2_score<-A_ord_2_score[,-(ncol(A_ord_2_score)-1)]

NonA_ord_2_score<-Mer2(NonA_ord_2,Food_Compass_2_score)
NonA_ord_2_score<-NonA_ord_2_score[,-(ncol(NonA_ord_2_score)-1)]

M2<-rbind(A_ord_2_score,NonA_ord_2_score)
M2_ord<-M2[order(M2[,ncol(M2)-2]),]
M2_ord_minDup<-M2_ord[!duplicated(M2_ord[,1]), ]
write.csv(M2_ord,"Compas_scores_Added_to_2404_without_M1_All_food_codes_from_WWEIA_analysis_Incl_dup.csv") # Second KEY file for final input for Figure 1

Fpro<-rep(0,17)
Score<-rep(0,17)
R_Fpro_vs_Score<-rep(0,17)
for (i in 1:17) {
minFpro<-M2_ord[min(which(match(M2_ord[,ncol(M2_ord)-2],i)==1)):max(which(match(M2_ord[,ncol(M2_ord)-2],i)==1)),"min_FPro"]
A_status<-M2_ord[min(which(match(M2_ord[,ncol(M2_ord)-2],i)==1)):max(which(match(M2_ord[,ncol(M2_ord)-2],i)==1)),"A_status"]
Food_Compass_Score_2<-M2_ord[min(which(match(M2_ord[,ncol(M2_ord)-2],i)==1)):max(which(match(M2_ord[,ncol(M2_ord)-2],i)==1)),"Food_Compass_Score_2"]
R_Fpro_vs_Score[i]<-cor(minFpro,Food_Compass_Score_2)
Mat<-
cbind(minFpro,A_status,Food_Compass_Score_2)
Mat2_A<-Mat[1:(which.min(Mat[,2])-1),]
Mat2_NA<-Mat[which.min(Mat[,2]):nrow(Mat),]
Score[i]<-t.test(Mat2_NA[,3],Mat2_A[,3],alternative="greater")[3]$p.value
Fpro[i]<-t.test(Mat2_NA[,1],Mat2_A[,1],alternative="less")[3]$p.value}

M2_ord2<-M2[order(M2[,ncol(M2)-1]),]
Mat3_NA<-M2_ord2[1:(which.max(M2_ord2[,ncol(M2_ord2)-1])-1),]
Mat3_A<-M2_ord2[which.max(M2_ord2[,ncol(M2_ord2)-1]):nrow(M2_ord2),]
Score_Mat3<-t.test(Mat3_NA[,ncol(Mat3_NA)],Mat3_A[,ncol(Mat3_A)],alternative="greater")[3]$p.value
Fpro_Mat3<-t.test(Mat3_NA[,"min_FPro"],Mat3_A[,"min_FPro"],alternative="less")[3]$p.value


write.csv(t(t(R_Fpro_vs_Score)),"Correlation_Fpro_vs_Score_for_17_cat.csv") # Third KEY file for final input for Figure 1






