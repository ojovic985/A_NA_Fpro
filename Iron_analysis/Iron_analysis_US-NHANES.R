C<-read.csv("Food_code_vs_WWEIA_code_without_duplicates.csv",header=TRUE)
C<-C[,-1]
W<-read.csv("Data_9450_food_codes_g_per_100g.csv")
W<-W[,-1]

Qa<-read.csv("C1C2_all_values_part_1_of_2_F1_07.csv",header=TRUE)
Qb<-read.csv("C1C2_all_values_part_2_of_2_F1_07.csv",header=TRUE)
Q<-rbind(Qa,Qb)
Q<-Q[,-1]

A<-read.csv("DR1IFF_C.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_C.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F15-49_Serie_C.csv",row.names=FALSE)  










A<-read.csv("DR1IFF_D.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_D.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F15-49_Serie_D.csv",row.names=FALSE)  










A<-read.csv("DR1IFF_E.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_E.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F15-49_Serie_E.csv",row.names=FALSE)  










A<-read.csv("DR1IFF_F.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_F.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F15-49_Serie_F.csv",row.names=FALSE)  











A<-read.csv("DR1IFF_G.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_G.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F15-49_Serie_G.csv",row.names=FALSE)  









A<-read.csv("DR1IFF_H.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_H.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F15-49_Serie_H.csv",row.names=FALSE)  






A<-read.csv("DR1IFF_I.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_I.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F15-49_Serie_I.csv",row.names=FALSE)  







A<-read.csv("DR1IFF_J.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_J.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F15-49_Serie_J.csv",row.names=FALSE)  






A<-read.csv("DR1IFF_P.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_P.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F15-49_Serie_P.csv",row.names=FALSE)  













C<-read.csv("Food_code_vs_WWEIA_code_without_duplicates.csv",header=TRUE)
C<-C[,-1]
W<-read.csv("Data_9450_food_codes_g_per_100g.csv")
W<-W[,-1]

Qa<-read.csv("C1C2_all_values_part_1_of_2_F5_07.csv",header=TRUE)
Qb<-read.csv("C1C2_all_values_part_2_of_2_F5_07.csv",header=TRUE)
Q<-rbind(Qa,Qb)
Q<-Q[,-1]



A<-read.csv("DR1IFF_C.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_C.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F50_plus_Serie_C.csv",row.names=FALSE)  









A<-read.csv("DR1IFF_D.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_D.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F50_plus_Serie_D.csv",row.names=FALSE)  









A<-read.csv("DR1IFF_E.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_E.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F50_plus_Serie_E.csv",row.names=FALSE)  










A<-read.csv("DR1IFF_F.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_F.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F50_plus_Serie_F.csv",row.names=FALSE)  









A<-read.csv("DR1IFF_G.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_G.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F50_plus_Serie_G.csv",row.names=FALSE)  








A<-read.csv("DR1IFF_H.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_H.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F50_plus_Serie_H.csv",row.names=FALSE)  










A<-read.csv("DR1IFF_I.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_I.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F50_plus_Serie_I.csv",row.names=FALSE)  















A<-read.csv("DR1IFF_J.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_J.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F50_plus_Serie_J.csv",row.names=FALSE)  









A<-read.csv("DR1IFF_P.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr1.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr1.020"),]
Tot_iron<-sum(G2[,"dr1iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr1ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr1ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr1iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr1iiron"]
NHI<-NHI+0.6*G2[m,"dr1iiron"]
}
else {

if (is.na(G2[m,"dr1iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr1iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat)<-c("seqn","Aval_iron","Total_iron")


A<-read.csv("DR2IFF_P.csv",header=TRUE)
A<-A[,-1]


colnames(Q)[1]<-"seqn"
Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="seqn")

Ser1<-unique(A1[,"seqn"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seqn"),]
G1<-G
Meals<-unique(G1[,"dr2.020"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"dr2.020"),]
Tot_iron<-sum(G2[,"dr2iiron"],na.rm=TRUE)
FE<-sum(G2[,"dr2ivc"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {
F_code<-G2[m,"dr2ifdcd"]

WWEIA_class<-C[match(F_code,C[,1]),2]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"dr2iiron"])==FALSE)&&(((WWEIA_class>2001)&&(WWEIA_class<2403))||((WWEIA_class>2601)&&(WWEIA_class<2609))||((WWEIA_class>3001)&&(WWEIA_class<3005))) ) {
HI<-HI+0.4*G2[m,"dr2iiron"]
NHI<-NHI+0.6*G2[m,"dr2iiron"]
}
else {

if (is.na(G2[m,"dr2iiron"])==FALSE) {
NHI<-NHI+G2[m,"dr2iiron"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 



Mat2<-cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron)))
colnames(Mat2)<-c("seqn","Aval_iron","Total_iron")

merged <- merge(Mat, Mat2, by = "seqn", all = TRUE)

library(dplyr)
A <- as.data.frame(Mat)
B <- as.data.frame(Mat2)
# Elements in A but not in B
A_not_in_B <- anti_join(A, B, by = "seqn")

# Elements in B but not in A
B_not_in_A <- anti_join(B, A, by = "seqn")

# Combine them: all non-matching elements
not_matched <- bind_rows(A_not_in_B, B_not_in_A)

Aval_iron_I<-array(nrow(merged))
Total_iron_I<-array(nrow(merged))

for (i in 1:nrow(merged)) {
Aval_iron_I[i]<-(merged[i,"Aval_iron.x"]+merged[i,"Aval_iron.y"])/2
Total_iron_I[i]<-(merged[i,"Total_iron.x"]+merged[i,"Total_iron.y"])/2}

Mat3<-cbind(merged[,"seqn"],t(t(Aval_iron_I)),t(t(Total_iron_I)))
colnames(Mat3)<-c("seqn","Aval_iron","Total_iron")

for (fj in 1:1) {
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)==0)) {
Mat4<-rbind(Mat3,A_not_in_B) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,B_not_in_A) }
if ((nrow(A_not_in_B)>0)&&(nrow(B_not_in_A)>0)) {
Mat4<-rbind(Mat3,A_not_in_B,B_not_in_A) }
if ((nrow(A_not_in_B)==0)&&(nrow(B_not_in_A)==0)) {
Mat4<-Mat3}
}


write.csv(Mat4,"Aval_and_Tot_Iron_F50_plus_Serie_P.csv",row.names=FALSE)  




##### COMPLEX SURVEY DESIGN STATISTICS #####


#### Goes with C1C2_F5_07.csv or with C1C2_F5_08.csv ####

B1<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_C.csv",header=TRUE)
B2<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_D.csv",header=TRUE)
B3<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_E.csv",header=TRUE)
B4<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_F.csv",header=TRUE)
B5<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_G.csv",header=TRUE)
B6<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_H.csv",header=TRUE)
B7<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_I.csv",header=TRUE)
B8<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_J.csv",header=TRUE)
B9<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_P.csv",header=TRUE)

#### Goes with C1C2_F5_07.csv or with C1C2_F5_08.csv ####





#### Goes with C1C2_F1_07 or with C1C2_F1_08 ####

B1<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_C.csv",header=TRUE) 
B2<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_D.csv",header=TRUE)
B3<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_E.csv",header=TRUE)
B4<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_F.csv",header=TRUE)
B5<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_G.csv",header=TRUE)
B6<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_H.csv",header=TRUE)
B7<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_I.csv",header=TRUE)
B8<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_J.csv",header=TRUE)
B9<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_P.csv",header=TRUE)

#### Goes with C1C2_F1_07.csv or with C1C2_F1_08.csv ####



Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9)

M<-read.csv("Series_C-P_sdmvpsu_stra_Adj_wtmec2yr.csv",header=TRUE)
M<-M[,-1]

M1<-merge(M,Buk,by="seqn")

Qa<-read.csv("C1C2_all_values_part_1_of_2_F1_07.csv",header=TRUE) # This goes with F15-49
Qb<-read.csv("C1C2_all_values_part_2_of_2_F1_07.csv",header=TRUE)
Q<-rbind(Qa,Qb)

Q<-Q[,-1]
colnames(Q)[1]<-"seqn"

Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]

M2<- merge(M1, Q1,by="seqn")

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"B"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2 # Both, i.e. anem + Nonanem


data<-data_1

library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)

est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est) # Anem F15-49 PTFA=1
#SE(est)
#confint(est)
#Aval_iron 
#0.6501852 See First row in table S16, column m(A, Tot/Av-Fe) -second value (after "/") = 0.650
#           Aval_iron
#Aval_iron 0.01314601
#              2.5 %   97.5 %
#Aval_iron 0.6244195 0.675951 # F15-49 Anem, PTFA =1

est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est) # F15-49 PTFA=1
#SE(est)
#confint(est)
#Total_iron 
#  12.23723 #Anem See First row in table S16, column m(A, Tot/Av-Fe) -first value (before "/") = 0.650
#           Total_iron
#Total_iron  0.2263876
#              2.5 %   97.5 %
#Total_iron 11.79352 12.68094 # F15-49 PTFA=1


data<-data_2

library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)

est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

coef(est) #Nonanem F15-49 PTFA =1
SE(est)
confint(est)
#Aval_iron 
#0.7154945 See First row in table S16, column m(Non-A, Tot/Av-Fe) -second value (after "/") = 0.715
#            Aval_iron
#Aval_iron 0.005373949
#              2.5 %    97.5 %
#Aval_iron 0.7049617 0.7260272


est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)  # F15-49 PTFA=1, NonAnem
coef(est)
SE(est)
confint(est)

#coef(est)                          
#SE(est)                    
#confint(est)
#Total_iron 
#  13.39212 #NonAnem #See First row in table S16, column m(Non-A, Tot/Av-Fe) -first value (before "/") = 12.24
#           Total_iron
#Total_iron 0.08853145
#             2.5 %   97.5 %
#Total_iron 13.2186 13.56564 #  F15-49 PTFA=1, NonAnem

svyttest(formula = Total_iron ~ B, design_nhanes)$p.value
#    BNonAnem 
#6.871784e-06  ### F15-49 PTFA=1 # See First row in table S16, column p-value -first value (before "/") = 6.9E-6


data=data_3

library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)


data$B <- relevel(factor(data$B, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")



design_nhanes <- svydesign(
  id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr,
  data = data, nest = TRUE
)
options(survey.lonely.psu="adjust")

model <- svyglm(Aval_iron ~ B, design = design_nhanes)
summary(model)


#Call:  ### F15-49 PTFA=1
#svyglm(formula = Aval_iron ~ B, design = design_nhanes)

#Survey design:
#svydesign(id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr, 
#    data = data, nest = TRUE)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.65019    0.01315  49.459  < 2e-16 ***
#BNonAnem     0.06531    0.01427   4.577  9.9e-06 *** See First row in table S16, column p-value -second value (after "/") = 9.9E-6


confint(model)
#                 2.5 %     97.5 %
#(Intercept) 0.62420712 0.67616337
#BNonAnem    0.03711369 0.09350475

summary(model)$coefficients[2,1]
#[1] 0.06530922 # Last column first row of table S16 value before ± 

summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.02819553 # Last column first row of table S16 value after ± 



Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9)

M<-read.csv("Series_C-P_sdmvpsu_stra_Adj_wtmec2yr.csv",header=TRUE)
M<-M[,-1]

M1<-merge(M,Buk,by="seqn")

Qa<-read.csv("C1C2_all_values_part_1_of_2_F1_08.csv",header=TRUE) # Second row # This goes with F15-49
Qb<-read.csv("C1C2_all_values_part_2_of_2_F1_08.csv",header=TRUE)
Q<-rbind(Qa,Qb)

Q<-Q[,-1]
colnames(Q)[1]<-"seqn"

Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]

M2<- merge(M1, Q1,by="seqn")

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"B"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2 # Both, i.e. anem + Nonanem


data<-data_1

library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)

est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est)  # Anem F15-49 PTFA=2
#SE(est)
#confint(est)
#Aval_iron 
#0.6773969 # Anem
#           Aval_iron
#Aval_iron 0.01350366
#              2.5 %    97.5 %
#Aval_iron 0.6509302 0.7038636 # Anem F15-49 PTFA=2

est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est) # F15-49 PTFA=2, Anem
#SE(est)
#confint(est)
#Total_iron 
#  12.69449 #Anem
#           Total_iron
#Total_iron   0.234135
#              2.5 %   97.5 %
#Total_iron 12.23559 13.15339 # F15-49 PTFA=2, Anem




data<-data_2

library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)

est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est) #Nonanem F15-49 PTFA =2
#SE(est)
#confint(est)
#Aval_iron 
#0.7135632 
#            Aval_iron
#Aval_iron 0.005577008
#              2.5 %   97.5 %
#Aval_iron 0.7026325 0.724494 #Nonanem F15-49 PTFA =2


est <- svymean(Total_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est) 
#SE(est)
#confint(est)
#Total_iron 
#  13.37547 
#           Total_iron
#Total_iron 0.08962048
#              2.5 %   97.5 %
#Total_iron 13.19982 13.55113 # F15-49 PTFA=2, NonAnem



dim(data_1) #Anem F15-49 PTFA =1
#[1] 1123    8
dim(data_2) #Nonanem F15-49 PTFA =1
#[1] 7784    8


data=data_3

library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)



data$B <- relevel(factor(data$B, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")



design_nhanes <- svydesign(
  id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr,
  data = data, nest = TRUE
)


options(survey.lonely.psu="adjust")

model <- svyglm(Aval_iron ~ B, design = design_nhanes)
summary(model)

#Call: ### F15-49 PTFA =2
#svyglm(formula = Aval_iron ~ B, design = design_nhanes)

#Survey design:
#svydesign(id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr, 
#    data = data, nest = TRUE)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.67740    0.01350  50.164   <2e-16 ***
#BNonAnem     0.03617    0.01525   2.371    0.019 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.145948)

#Number of Fisher Scoring iterations: 2

confint(model)
#                  2.5 %     97.5 %
#(Intercept) 0.650712028 0.70408179
#BNonAnem    0.006025128 0.06630752

summary(model)$coefficients[2,1]
#[1] 0.03616633
summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.0301412

svyttest(formula = Total_iron ~ B, design_nhanes)$p.value
#  BNonAnem 
#0.01023173 

dim(data_1)
#[1] 901   8
dim(data_2)
#[1] 7427    8  ### F15-49 PTFA =2




##F50+


B1<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_C.csv",header=TRUE)
B2<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_D.csv",header=TRUE)
B3<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_E.csv",header=TRUE)
B4<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_F.csv",header=TRUE)
B5<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_G.csv",header=TRUE)
B6<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_H.csv",header=TRUE)
B7<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_I.csv",header=TRUE)
B8<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_J.csv",header=TRUE)
B9<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_P.csv",header=TRUE)

Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9)

M<-read.csv("Series_C-P_sdmvpsu_stra_Adj_wtmec2yr.csv",header=TRUE)
M<-M[,-1]

M1<-merge(M,Buk,by="seqn")

Qa<-read.csv("C1C2_all_values_part_1_of_2_F5_07.csv",header=TRUE) #Goes with F50+
Qb<-read.csv("C1C2_all_values_part_2_of_2_F5_07.csv",header=TRUE)
Q<-rbind(Qa,Qb)

Q<-Q[,-1]
colnames(Q)[1]<-"seqn"

Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]

M2<- merge(M1, Q1,by="seqn")

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"B"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2 # Both, i.e. anem + Nonanem


data<-data_1

library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)

est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est) # F50+ Anem PTFA=1
#SE(est)
#confint(est)
#Aval_iron 
#0.6561223 
#           Aval_iron
#Aval_iron 0.01457335
#              2.5 %    97.5 %
#Aval_iron 0.6275591 0.6846856 # F50+ Anem PTFA=1

est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est) # F50+ Anem PTFA=1
#SE(est)                            
#confint(est)
#Total_iron 
#  12.24018 
#           Total_iron
#Total_iron  0.2331061
#             2.5 %   97.5 %
#Total_iron 11.7833 12.69706  # F50+ Anem PTFA=1

dim(data_1) # F50+  PTFA=1
#[1] 1051    8
dim(data_2)
# [1] 8877    8  # F50+  PTFA=1


data<-data_2

library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)

est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#SE(est)
#confint(est)
#Aval_iron 
#0.7093823 
#            Aval_iron
#Aval_iron 0.006401868
#              2.5 %    97.5 %
#Aval_iron 0.6968348 0.7219297

est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est)                                         
#SE(est)
#confint(est)
#Total_iron 
#  12.82373 
#           Total_iron
#Total_iron  0.1036975
#              2.5 %   97.5 %
#Total_iron 12.62048 13.02697  # F50+ NonAnem PTFA=1


data<-data_3

model <- svyglm(Aval_iron ~ B, design = design_nhanes) # F50+ PTFA=1
summary(model)

#Call:
#svyglm(formula = Aval_iron ~ B, design = design_nhanes)

#Survey design:
#svydesign(id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr, 
#    data = data, nest = TRUE)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.65612    0.01457  45.022  < 2e-16 ***
#BNonAnem     0.05326    0.01596   3.337  0.00107 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.1535391)

#Number of Fisher Scoring iterations: 2

confint(model)
#                 2.5 %     97.5 %
#(Intercept) 0.62732363 0.68492107
#BNonAnem    0.02172269 0.08479711


summary(model)$coefficients[2,1]
#[1] 0.0532599

summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.03153721

svyttest(formula = Total_iron ~ B, design_nhanes)$p.value
#  BNonAnem 
#0.02246847 # F50+ PTFA=1



Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9)

M<-read.csv("Series_C-P_sdmvpsu_stra_Adj_wtmec2yr.csv",header=TRUE)
M<-M[,-1]

M1<-merge(M,Buk,by="seqn")

Qa<-read.csv("C1C2_all_values_part_1_of_2_F5_08.csv",header=TRUE) # Fourth row # This goes with F50_plus
Qb<-read.csv("C1C2_all_values_part_2_of_2_F5_08.csv",header=TRUE)
Q<-rbind(Qa,Qb)

Q<-Q[,-1]
colnames(Q)[1]<-"seqn"

Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]

M2<- merge(M1, Q1,by="seqn")

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"B"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2 # Both, i.e. anem + Nonanem


data<-data_1

library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)

est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est) # F50+ Anem PTFA=2
#SE(est)
#confint(est)
#Aval_iron 
#0.6641243 
#           Aval_iron
#Aval_iron 0.01699036
#              2.5 %    97.5 %
#Aval_iron 0.6308238 0.6974248


est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est)
#SE(est)
#confint(est)
#Total_iron 
#  12.31689 
#           Total_iron
#Total_iron  0.2750644
#              2.5 %   97.5 %
#Total_iron 11.77778 12.85601 # F50+ Anem PTFA=2


data<-data_2

library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)

est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est) # F50+ NonAnem PTFA=2
#SE(est)
#confint(est)
#Aval_iron 
#0.7117552 
#            Aval_iron
#Aval_iron 0.006333836
#              2.5 %    97.5 %
#Aval_iron 0.6993411 0.7241693

est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#coef(est)
#SE(est)                            
#confint(est)
#Total_iron 
#  12.83342 
#           Total_iron
#Total_iron  0.1013033
#              2.5 %   97.5 %
#Total_iron 12.63487 13.03197 # F50+ NonAnem PTFA=2

dim(data_1) # F50+ PTFA=2
#[1] 815   8
dim(data_2)
#[1] 8478    8 # F50+ PTFA=2


data<-data_3 # F50+ PTFA=2
library(survey)
options(survey.lonely.psu="adjust")
design_nhanes <- svydesign(
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = data,
  nest = TRUE
)


data$B <- relevel(factor(data$B, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")

design_nhanes <- svydesign(
  id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr,
  data = data, nest = TRUE
)

options(survey.lonely.psu="adjust")
model <- svyglm(Aval_iron ~ B, design = design_nhanes)
summary(model)

#Call:
#svyglm(formula = Aval_iron ~ B, design = design_nhanes)

#Survey design:
#svydesign(id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr, 
#    data = data, nest = TRUE)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.66412    0.01699  39.088   <2e-16 ***
#BNonAnem     0.04763    0.01794   2.655   0.0088 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.1548563)

#Number of Fisher Scoring iterations: 2

confint(model)
#                 2.5 %     97.5 %
#(Intercept) 0.63054922 0.69769931
#BNonAnem    0.01217693 0.08308489

summary(model)$coefficients[2,1]
#[1] 0.04763091
summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.03545398
svyttest(formula = Total_iron ~ B, design_nhanes)$p.value
#  BNonAnem 
#0.07290124 # F50+ PTFA=2











##### COHORT STATISTICS #####

B1<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_C.csv",header=TRUE)
B2<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_D.csv",header=TRUE)
B3<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_E.csv",header=TRUE)
B4<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_F.csv",header=TRUE)
B5<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_G.csv",header=TRUE)
B6<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_H.csv",header=TRUE)
B7<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_I.csv",header=TRUE)
B8<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_J.csv",header=TRUE)
B9<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_P.csv",header=TRUE)

Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9)

Qa<-read.csv("C1C2_all_values_part_1_of_2_F1_07.csv",header=TRUE) # This goes with F15-49
Qb<-read.csv("C1C2_all_values_part_2_of_2_F1_07.csv",header=TRUE)
Q<-rbind(Qa,Qb)

Q<-Q[,-1]
colnames(Q)[1]<-"seqn"

Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]

M2<- merge(Buk, Q1,by="seqn")

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"B"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2

data<-data_1


est <- mean(data_1[,"Aval_iron"], na.rm = TRUE) # F15-49 PTFA=1, Anem
est
#[1] 0.6804031 
est <- mean(data_1[,"Total_iron"], na.rm = TRUE) 
est
#[1] 12.52741 # F15-49 PTFA=1, Anem


est <- mean(data_2[,"Aval_iron"], na.rm = TRUE) # F15-49 PTFA=1, NonAnem
est
#[1] 0.723718
est <- mean(data_2[,"Total_iron"], na.rm = TRUE) 
est
#[1] 13.35015 # F15-49 PTFA=1, NonAnem


data=data_3 # F15-49 PTFA=1

data$B <- relevel(factor(data$B, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")

model <- glm(Aval_iron ~ B, data=data)
summary(model)

#Call:
#glm(formula = Aval_iron ~ B, data = data)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.68040    0.01195  56.932  < 2e-16 ***
#BNonAnem     0.04331    0.01278   3.388 0.000707 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.1603995)

#    Null deviance: 1430.2  on 8906  degrees of freedom
#Residual deviance: 1428.4  on 8905  degrees of freedom
#AIC: 8980.4

#Number of Fisher Scoring iterations: 2

confint(model)
#Waiting for profiling to be done...
#                 2.5 %    97.5 %
#(Intercept) 0.65697913 0.7038270
#BNonAnem    0.01825827 0.0683717

summary(model)$coefficients[2,1]
#[1] 0.04331498

summary(model)$coefficients[2,1]-confint(model)[2,1]
#Waiting for profiling to be done...
#[1] 0.02505671

t.test(formula = Aval_iron ~ B, data=data,var.equal=TRUE)$p.value
#[1] 0.0007067167

t.test(formula = Total_iron ~ B, data=data,var.equal=TRUE)$p.value
#[1] 8.169158e-05 # F15-49 PTFA=1



Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9)

Qa<-read.csv("C1C2_all_values_part_1_of_2_F1_08.csv",header=TRUE) # This goes with F15-49
Qb<-read.csv("C1C2_all_values_part_2_of_2_F1_08.csv",header=TRUE)
Q<-rbind(Qa,Qb)

Q<-Q[,-1]
colnames(Q)[1]<-"seqn"

Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]

M2<- merge(Buk, Q1,by="seqn")

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"B"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2

data<-data_1

est <- mean(data_1[,"Aval_iron"], na.rm = TRUE)  # F15-49 PTFA=2, Anem
est
#[1] 0.6964455

est <- mean(data_1[,"Aval_iron"], na.rm = TRUE) 
est
[1] 12.78068 # F15-49 PTFA=2, Anem



est <- mean(data_2[,"Aval_iron"], na.rm = TRUE) # F15-49 PTFA=2, NonAnem
est
#[1] 0.7218943
est <- mean(data_2[,"Total_iron"], na.rm = TRUE) 
est
#[1] 13.33077 # F15-49 PTFA=2, NonAnem

data=data_3  # F15-49 PTFA=2
data$B <- relevel(factor(data$B, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")
model <- glm(Aval_iron ~ B, data=data)
summary(model)


#Call:
#glm(formula = Aval_iron ~ B, data = data)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.69645    0.01332  52.303   <2e-16 ***
#BNonAnem     0.02545    0.01410   1.805   0.0711 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.1597538)

#    Null deviance: 1330.6  on 8327  degrees of freedom
#Residual deviance: 1330.1  on 8326  degrees of freedom
#AIC: 8363.3

#Number of Fisher Scoring iterations: 2


confint(model)
#Waiting for profiling to be done...
#                   2.5 %     97.5 %
#(Intercept)  0.670347226 0.72254372
#BNonAnem    -0.002187137 0.05308484

summary(model)$coefficients[2,1]
#[1] 0.02544885

summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.02763599

t.test(formula = Aval_iron ~ B, data=data,var.equal=TRUE)$p.value
#[1] 0.07113417

t.test(formula = Total_iron ~ B, data=data,var.equal=TRUE)$p.value
#[1] 0.0168854





B1<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_C.csv",header=TRUE)
B2<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_D.csv",header=TRUE)
B3<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_E.csv",header=TRUE)
B4<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_F.csv",header=TRUE)
B5<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_G.csv",header=TRUE)
B6<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_H.csv",header=TRUE)
B7<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_I.csv",header=TRUE)
B8<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_J.csv",header=TRUE)
B9<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_P.csv",header=TRUE)


Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9)

Qa<-read.csv("C1C2_all_values_part_1_of_2_F5_07.csv",header=TRUE)
Qb<-read.csv("C1C2_all_values_part_2_of_2_F5_07.csv",header=TRUE)
Q<-rbind(Qa,Qb)

Q<-Q[,-1]
colnames(Q)[1]<-"seqn"

Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]

M2<- merge(Buk, Q1,by="seqn")

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"B"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2

data<-data_1


est <- mean(data_1[,"Aval_iron"], na.rm = TRUE) # F50+ PTFA=1, Anem
est
#[1] 0.6381014
est <- mean(data_1[,"Total_iron"], na.rm = TRUE) 
est
#[1] 11.69921  # F50+ PTFA=1, Anem

est <- mean(data_2[,"Aval_iron"], na.rm = TRUE) # F50+ PTFA=1, NonAnem
est
#[1] 0.6922971
est <- mean(data_2[,"Total_iron"], na.rm = TRUE)
est
#[1] 12.43724 # F50+ PTFA=1, NonAnem


data=data_3 # F50+ PTFA=1
data$B <- relevel(factor(data$B, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")
model <- glm(Aval_iron ~ B, data=data)
summary(model)

#Call:
#glm(formula = Aval_iron ~ B, data = data)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.63810    0.01180  54.056  < 2e-16 ***
#BNonAnem     0.05420    0.01248   4.341 1.43e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.1464536)

#    Null deviance: 1456.5  on 9927  degrees of freedom
#Residual deviance: 1453.7  on 9926  degrees of freedom
#AIC: 9106.3

#Number of Fisher Scoring iterations: 2


confint(model)
#Waiting for profiling to be done...
#                 2.5 %     97.5 %
#(Intercept) 0.61496494 0.66123785
#BNonAnem    0.02972797 0.07866353


summary(model)$coefficients[2,1]
#[1] 0.05419575

summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.02446778

t.test(formula = Aval_iron ~ B, data=data,var.equal=TRUE)$p.value
#[1] 1.430535e-05

t.test(formula = Total_iron ~ B, data=data,var.equal=TRUE)$p.value
#[1] 0.0001207392 # F50+ PTFA=1



Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9)

Qa<-read.csv("C1C2_all_values_part_1_of_2_F5_08.csv",header=TRUE)
Qb<-read.csv("C1C2_all_values_part_2_of_2_F5_08.csv",header=TRUE)
Q<-rbind(Qa,Qb)

Q<-Q[,-1]
colnames(Q)[1]<-"seqn"

Q1<-Q[,1:(which(grepl("X",colnames(Q)))[1]-1)]

M2<- merge(Buk, Q1,by="seqn")

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"B"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2

data<-data_3

est <- mean(data_1[,"Aval_iron"], na.rm = TRUE) # F50+ PTFA=2, Anem
est
#[1] 0.6402907

est <- mean(data_1[,"Total_iron"], na.rm = TRUE)
est
#[1] 11.67506 # F50+ PTFA=2, Anem

est <- mean(data_2[,"Aval_iron"], na.rm = TRUE) # F50+ PTFA=2, NonAnem
est
#[1] 0.6938106
est <- mean(data_2[,"Total_iron"], na.rm = TRUE)
est
#[1] 12.45333 # F50+ PTFA=2, NonAnem


data=data_3 # F50+ PTFA=2
data$B <- relevel(factor(data$B, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")
model <- glm(Aval_iron ~ B, data=data)
summary(model)

#Call:
#glm(formula = Aval_iron ~ B, data = data)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.64029    0.01345    47.6  < 2e-16 ***
#BNonAnem     0.05352    0.01408     3.8 0.000145 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.1474693)

#    Null deviance: 1372.3  on 9292  degrees of freedom
#Residual deviance: 1370.1  on 9291  degrees of freedom
#AIC: 8588.3

#Number of Fisher Scoring iterations: 2


confint(model)
#Waiting for profiling to be done...
#                 2.5 %     97.5 %
#(Intercept) 0.61392615 0.66665521
#BNonAnem    0.02591725 0.08112261


summary(model)$coefficients[2,1]
#[1] 0.05351993

summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.02760268

t.test(formula = Aval_iron ~ B, data=data,var.equal=TRUE)$p.value
#[1] 0.0001454725

t.test(formula = Total_iron ~ B, data=data,var.equal=TRUE)$p.value
#[1] 0.0002979123












