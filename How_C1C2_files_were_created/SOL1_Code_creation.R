C<-read.csv("Ser_C_matr3.csv")
C<-C[,-c(1,2)]
D<-read.csv("Ser_D_matr3.csv")
D<-D[,-c(1,2)]
E<-read.csv("Ser_E_matr3.csv")
E<-E[,-c(1,2)]
F<-read.csv("Ser_F_matr3.csv")
F<-F[,-c(1,2)]
G<-read.csv("Ser_G_matr3.csv")
G<-G[,-c(1,2)]
H<-read.csv("Ser_H_matr3.csv")
H<-H[,-c(1,2)]
I<-read.csv("Ser_I_matr3.csv")
I<-I[,-c(1,2)]
J<-read.csv("Ser_J_matr3.csv")
J<-J[,-c(1,2)]
P<-read.csv("Ser_P_matr3.csv")
P<-P[,-c(1,2)]


Merger<-c()
COLnames<-c()
br<-0
for (i in 1:length(colnames(C))) {
if ((is.na(match(colnames(C)[i],colnames(D)))==FALSE)&&(is.na(match(colnames(C)[i],colnames(E)))==FALSE)&&(is.na(match(colnames(C)[i],colnames(F)))==FALSE)&&(is.na(match(colnames(C)[i],colnames(G)))==FALSE)&&(is.na(match(colnames(C)[i],colnames(H)))==FALSE)&&(is.na(match(colnames(C)[i],colnames(I)))==FALSE)&&(is.na(match(colnames(C)[i],colnames(J)))==FALSE)&&(is.na(match(colnames(C)[i],colnames(P)))==FALSE)) {
Merger<-cbind(Merger,C[,i])
COLnames<-cbind(COLnames,colnames(C)[i])
br<-br+1}}

COLnames1<-as.vector(COLnames)
C1<-C[,COLnames1]
D1<-D[,COLnames1]
E1<-E[,COLnames1]
F1<-F[,COLnames1]
G1<-G[,COLnames1]
H1<-H[,COLnames1]
I1<-I[,COLnames1]
J1<-J[,COLnames1]
P1<-P[,COLnames1]

F_mat<-rbind(C1,D1,E1,F1,G1,H1,I1,J1,P1)

F_mat<-F_mat[,-ncol(F_mat)]
F_mat<-F_mat[,-ncol(F_mat)]

C<-read.csv("BPX_C_prep.csv",header=TRUE)
C<-C[,-1]
D<-read.csv("BPX_D_prep.csv",header=TRUE)
D<-D[,-1]
E<-read.csv("BPX_E_prep.csv",header=TRUE)
E<-E[,-1]
F<-read.csv("BPX_F_prep.csv",header=TRUE)
F<-F[,-1]
G<-read.csv("BPX_G_prep.csv",header=TRUE)
G<-G[,-1]
H<-read.csv("BPX_H_prep.csv",header=TRUE)
H<-H[,-1]
I<-read.csv("BPX_I_prep.csv",header=TRUE)
I<-I[,-1]
J<-read.csv("BPX_J_prep.csv",header=TRUE)
J<-J[,-1]
P<-read.csv("BPX_P_cor2.csv",header=TRUE)
P<-P[,-1]

Merge_BPX<-rbind(C,D,E,F,G,H,I,J,I,P)

C2<-read.csv("DSQ1_C.csv")
C2<-C2[,-1]
C3<-read.csv("OHXDEN_C.csv")
C3<-C3[,-1]

D2<-read.csv("DSQ1_D.csv")
D2<-D2[,-1]
D3<-read.csv("OHXDEN_D.csv")
D3<-D3[,-1]

E2<-read.csv("DSQTOT_E.csv")
E2<-E2[,-1]
E3<-read.csv("OHXDEN_E.csv")
E3<-E3[,-1]

F2<-read.csv("DSQTOT_F.csv")
F2<-F2[,-1]
F3<-read.csv("OHXDEN_F.csv")
F3<-F3[,-1]


G2<-read.csv("DSQTOT_G.csv")
G2<-G2[,-1]
G3<-read.csv("OHXDEN_G.csv")
G3<-G3[,-1]

H2<-read.csv("DSQTOT_H.csv")
H2<-H2[,-1]
H3<-read.csv("OHXDEN_H.csv")
H3<-H3[,-1]

I2<-read.csv("DSQTOT_I.csv")
I2<-I2[,-1]
I3<-read.csv("OHXDEN_I.csv")
I3<-I3[,-1]

J2<-read.csv("DSQTOT_J.csv")
J2<-J2[,-1]
J3<-read.csv("OHXDEN_J.csv")
J3<-J3[,-1]

P2<-read.csv("P_DSQTOT.csv")
P2<-P2[,-1]
P3<-read.csv("P_OHXDEN.csv")
P3<-P3[,-1]


Mer2<-function(X1,X2){

X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {
if (is.na(match(X1["seqn"][i,1],X2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(X1["seqn"][i,1],X2["seqn"][,1]))}}
X1X2<-cbind(X1[I,],X2[J,])
return(X1X2)}

CM<-Mer2(C2,C3)
DM<-Mer2(D2,D3)
EM<-Mer2(E2,E3)
FM<-Mer2(F2,F3)
GM<-Mer2(G2,G3)
HM<-Mer2(H2,H3)
IM<-Mer2(I2,I3)
JM<-Mer2(J2,J3)
PM<-Mer2(P2,P3)


Merger<-c()
COLnames<-c()
br<-0
for (i in 1:length(colnames(CM))) {
if ((is.na(match(colnames(CM)[i],colnames(DM)))==FALSE)&&(is.na(match(colnames(CM)[i],colnames(EM)))==FALSE)&&(is.na(match(colnames(CM)[i],colnames(FM)))==FALSE)&&(is.na(match(colnames(CM)[i],colnames(GM)))==FALSE)&&(is.na(match(colnames(CM)[i],colnames(HM)))==FALSE)&&(is.na(match(colnames(CM)[i],colnames(IM)))==FALSE)&&(is.na(match(colnames(CM)[i],colnames(JM)))==FALSE)&&(is.na(match(colnames(CM)[i],colnames(PM)))==FALSE)) {
Merger<-cbind(Merger,CM[,i])
COLnames<-cbind(COLnames,colnames(CM)[i])
br<-br+1}}

COLnames1<-as.vector(COLnames)
C1<-CM[,COLnames1]
D1<-DM[,COLnames1]
E1<-EM[,COLnames1]
F1<-FM[,COLnames1]
G1<-GM[,COLnames1]
H1<-HM[,COLnames1]
I1<-IM[,COLnames1]
J1<-JM[,COLnames1]
P1<-PM[,COLnames1]

S_CPM<-rbind(C1,D1,E1,F1,G1,H1,I1,J1,P1)

CQ<-read.csv("Merged_Ser_C_REL_Questionnaire.csv",header=TRUE)
CQ<-CQ[,-1]
DQ<-read.csv("Merged_Ser_D_REL_Questionnaire.csv",header=TRUE)
DQ<-DQ[,-1]
EQ<-read.csv("Merged_Ser_E_REL_Questionnaire.csv",header=TRUE)
EQ<-EQ[,-1]
FQ<-read.csv("Merged_Ser_F_REL_Questionnaire.csv",header=TRUE)
FQ<-FQ[,-1]
GQ<-read.csv("Merged_Ser_G_REL_Questionnaire.csv",header=TRUE)
GQ<-GQ[,-1]
HQ<-read.csv("Merged_Ser_H_REL_Questionnaire.csv",header=TRUE)
HQ<-HQ[,-1]
IQ<-read.csv("Merged_Ser_I_REL_Questionnaire.csv",header=TRUE)
IQ<-IQ[,-1]
JQ<-read.csv("Merged_Ser_J_REL_Questionnaire.csv",header=TRUE)
JQ<-JQ[,-1]
PQ<-read.csv("Merged_Ser_P_REL_Questionnaire.csv",header=TRUE)
PQ<-PQ[,-1]

Merger<-c()
COLnames<-c()
br<-0
for (i in 1:length(colnames(CQ))) {
if ((is.na(match(colnames(CQ)[i],colnames(DQ)))==FALSE)&&(is.na(match(colnames(CQ)[i],colnames(EQ)))==FALSE)&&(is.na(match(colnames(CQ)[i],colnames(FQ)))==FALSE)&&(is.na(match(colnames(CQ)[i],colnames(GQ)))==FALSE)&&(is.na(match(colnames(CQ)[i],colnames(HQ)))==FALSE)&&(is.na(match(colnames(CQ)[i],colnames(IQ)))==FALSE)&&(is.na(match(colnames(CQ)[i],colnames(JQ)))==FALSE)&&(is.na(match(colnames(CQ)[i],colnames(PQ)))==FALSE)) {
Merger<-cbind(Merger,CQ[,i])
COLnames<-cbind(COLnames,colnames(CQ)[i])
br<-br+1}}

COLnames1<-as.vector(COLnames)
C1<-CQ[,COLnames1]
D1<-DQ[,COLnames1]
E1<-EQ[,COLnames1]
F1<-FQ[,COLnames1]
G1<-GQ[,COLnames1]
H1<-HQ[,COLnames1]
I1<-IQ[,COLnames1]
J1<-JQ[,COLnames1]
P1<-PQ[,COLnames1]

Quest_Mer<-rbind(C1,D1,E1,F1,G1,H1,I1,J1,P1)

Zub_mat<-Mer2(F_mat,S_CPM)

Zub_BPX_mat<-Mer2(Zub_mat,Merge_BPX)

ZBQ_mat<-Mer2(Zub_BPX_mat,Quest_Mer)

matr3<-ZBQ_mat

dim(matr3)
#[1] 61011   371 Comment: SOL1 merged but with some nutritional missing values

X<-array(rep(0),dim(matr3)[2])
for (i in 1:dim(matr3)[2]) {
X[i]<-sum(is.na(matr3[,i]))
}


XX<-matr3
threshold<-0.3*dim(matr3)[1]
zu<-c()
for (i in 1:length(X)) {
if (X[i]>threshold)
zu<-cbind(zu,i)
}

XX<-XX[,-zu]

te<-c()
for (i in 1:length(colnames(XX))) {
if (grepl("ohx",colnames(XX[i]))==TRUE) {
te<-cbind(te,i)}}

ZUBI<-XX[,te]
TUBI<-ZUBI
for (i in 1:nrow(ZUBI)) {
for (j in 1:ncol(ZUBI)) {
if ((is.na(ZUBI[i,j])==FALSE)&&((ZUBI[i,j]==1)||(ZUBI[i,j]==2))) {
TUBI[i,j]<-1}
else {
TUBI[i,j]<-0}
}}
Br_zubi<-array(nrow(TUBI))
for (i in 1:nrow(TUBI)) {
Br_zubi[i]<-sum(TUBI[i,])}
XXY<-XX[,-te]
XXY<-cbind(XXY,Br_zubi)
XX<-XXY

XX1<-XX[,-c(21:44,53:54,131:132,204,209,210,218,221,226)]

FEST<-XX[,"lbxhgb"]
XY<-XX1

XY1<-XY
for (i in 1:dim(XY)[1]) {
if (XY[i,"ridageyr"]>80)
XY1[i,"ridageyr"]<-80}

X<-XY1

num<-array(rep(0),dim(X)[2])
for (i in dim(X)[2]:1) {
num[i]<-sum(is.na(X[,i]))}

num2<-array(rep(0),dim(X)[1])
for (j in 1:dim(X)[1]) {
num2[j]<-sum(is.na(X[j,]))}

X<-XX

anemia<-array(rep(0),dim(X)[1])
for (i in 1:(dim(X)[1])) {
if (X[i,"ridageyr"]<2) {
if (X[i,"lbxhgb"]<10.5)
anemia[i]<-1
}
else {
if (X[i,"ridageyr"]<5) {
if (X[i,"lbxhgb"]<11)
anemia[i]<-1
}
else {
if (X[i,"ridageyr"]<12) {
if (X[i,"lbxhgb"]<11.5)
anemia[i]<-1
}
else {
if (X[i,"ridageyr"]<15) {
if (X[i,"lbxhgb"]<12)
anemia[i]<-1
}
else {
if (X[i,"riagendr"]==1) {
if (X[i,"lbxhgb"]<13)
anemia[i]<-1
}
else {
if (X[i,"lbxhgb"]<12)
anemia[i]<-1
}

}
}
}}
}

X<-XY1
XY1<-X

te<-c()
for (i in 1:length(colnames(XY1))) {
if (grepl("bpx",colnames(XY1[i]))==TRUE) {
te<-cbind(te,i)}}

Tlak<-XY1[,te]
Tlak1<-Tlak[,1:3]

XXY<-XY1[,-te]
XXY<-cbind(XXY,Tlak1)
XY1<-XXY
X<-XY1





Xsel<-XY1[,!colnames(XY1) %in% c("sddsrvyr","ridstatr","sdmvpsu","ridexmon","dmdeduc2","indfmpir","sialang","siaproxy","siaintrp","fialang","fiaproxy","fiaintrp","mialang","miaproxy","miaintrp","bmdstats","bmxleg", "bmxarml", "bmxarmc","bmxht","bmxwaist","dr1exmer","dr1tnumf","dr1drstz","drabf","drdint","dr1day","dr1lang","dbq095z","drqsprep","drqsdiet","dr1.300","drd340","drd360","dr2drstz","dr2exmer","drabf.1","drdint.1","dr2day","dr2lang","dr2tnumf","dr2.300","dsd010","dsd010an","dsdcount","dsdancnt","mcq160a","mcq160b","mcq160c","mcq160d","mcq160e","mcq160f","mcq160l","mcq160m","mcq220","huq030","huq090","mcq010","mcq080")]

X<-array(rep(0),dim(Xsel)[2])
for (i in 1:dim(Xsel)[2]) {
X[i]<-sum(is.na(Xsel[,i]))
}
elim1<-c()
for (i in 1:nrow(Xsel)) {
if (is.na(Xsel[i,"dr1tiron"])==TRUE)
elim1<-cbind(elim1,i)}

Xsel1<-Xsel[-elim1,]

dim(Xsel1)



X<-array(rep(0),dim(Xsel1)[2])
for (i in 1:dim(Xsel1)[2]) {
X[i]<-sum(is.na(Xsel1[,i]))
}

Xsel2<-Xsel1[,!colnames(Xsel1) %in%c("diq050","bpxpls","bpxsy1")]
decis1<-c()
for (i in 1:nrow(Xsel2)) {
if (is.na(Xsel2[i,"dr2tiron"])==TRUE)
decis1<-cbind(decis1,i)}



coln2<-gsub("dr1t","drt",colnames(Xsel2))
coln3<-coln2[-seq(71,133,by=1)]

Xsel3<-cbind(Xsel2[,1:70],Xsel2[,134:ncol(Xsel2)])
colnames(Xsel3)<-coln3
for (i in 1:nrow(Xsel3)) {
if (i%in%decis1==FALSE) {
for (j in 8:70) {
Xsel3[i,j]<-(Xsel2[i,j]+Xsel2[i,j+63])/2}
}}

Xfum<-Xsel3[,colnames(Xsel3) %in% c("ridreth1","diq010","huq010","huq071","mcq053","mcq092")]


Xnum<-Xsel3[,!colnames(Xsel3) %in% c("ridreth1","diq010","huq010","huq071","mcq053","mcq092")]

anemia<-anemia[-elim1]
FEST<-FEST[-elim1]


X<-array(rep(0),dim(Xsel3)[2])
for (i in 1:dim(Xsel3)[2]) {
X[i]<-sum(is.na(Xsel3[,i]))
}


Tnum<-Xnum
for (i in 1:dim(Xnum)[2]) {
if (any(is.na(Xnum[,i]))==TRUE) {

P<-Xnum[,i]
for (j in dim(Xnum)[1]:1) {
if (is.na(Xnum[j,i])==TRUE)
P<-P[-j]
}

for (j in dim(Xnum)[1]:1) {
if (is.na(Xnum[j,i])==TRUE)
Xnum[j,i]<-mean(P)
}}}


num<-array(rep(0),dim(Xnum)[2])
for (i in dim(Xnum)[2]:1) {
num[i]<- sum(is.na(Xnum[,i]))}






library(car)
library(caret)
X<-Xfum
T<-array(rep(0),dim(X)[2])
for (i in dim(X)[2]:1) {
for (j in 1:dim(X)[1]) {
if (is.na(X[j,i])==TRUE)
X[j,i]<-100
}}
df<-data.frame(X)
df2<-df
for (i in 1:ncol(df)) {
df2[,i]<-as.factor(df[,i])}
df<-df2
library(dplyr)
df1=df %>% dummyVars(~.,data=., sep="_") %>% predict(,newdata=df) %>% as.data.frame()
XX<-df1
for (i in dim(df1)[2]:1) {
if (any(is.na(df1[,i]))==TRUE)
XX<-XX[,-i]}
write.csv(XX,"Xfum_1hoten_57838_SOL1.csv")
write.csv(Xnum,"Xnum_1hoten_57838_SOL1.csv")
XNUM<-cbind(Xnum,XX)
for (i in dim(XNUM)[2]:1) {
if (any(is.na(XNUM[,i]))==TRUE)
XNUM<-XNUM[,-i]}
dim(XNUM)
#[1] 57838    98
write.csv(XNUM,"Xnum_and_Xfum_1hoten_57838_SOL1.csv")

write.csv(FEST,"FEST_1hoten_57838_SOL1.csv")
write.csv(anemia,"Anemia_1hoten_57838_SOL1.csv")


FEST<-read.csv("FEST_1hoten_57838_SOL1.csv",header=TRUE)
FEST<-FEST[,-1]
FEST<-as.vector(t(FEST))
Xnum<-read.csv("Xnum_1hoten_57838_SOL1.csv")
Xnum<-Xnum[,-1]


Xnumfum<-read.csv("Xnum_and_Xfum_1hoten_57838_SOL1.csv")
Xnumfum<-Xnumfum[,-1]

anemia<-read.csv("Anemia_1hoten_57838_SOL1.csv",header=TRUE)
anemia<-anemia[,-1]
anemia<-as.vector(t(anemia))

Xfum<-read.csv("Xfum_1hoten_57838_SOL1.csv",header=TRUE)
Xfum<-Xfum[,-1]

Xfum2<-Xfum[,colnames(Xfum) %in% c("ridreth1_1","ridreth1_2","ridreth1_3","ridreth1_4","ridreth1_5","diq010_1","huq010_5",
"huq071_1","mcq053_1","mcq053_2","mcq092_1")]


XNUM<-cbind(Xnum,Xfum2)
write.csv(XNUM, "Xnumfum_1hoten_57838_SOL1.csv")

Xnumfum<-read.csv("Xnumfum_1hoten_57838_SOL1.csv")
Xnumfum<-Xnumfum[,-1]






#Xnum2<-c()
Xnumfum2<-c()
FEST2<-c()
anemia2<-c()
J<-c()
for (i in 1:length(anemia)) {
if ((Xnum[i,"riagendr"]==2)&&(Xnum[i,"ridageyr"]>14)&&(Xnum[i,"ridageyr"]<50)) {
J<-cbind(J,i)}}
#Xnum2<-Xnum[J,]
Xnumfum2<-Xnumfum[J,]
FEST2<-FEST[J]
anemia2<-anemia[J]


write.csv(Xnumfum2,"Xnumfum2_repF_Solution1_57838_to_14170.csv")
write.csv(anemia2,"anemia2_repF_Solution1_57838_to_14170.csv")
write.csv(FEST2,"FEST2_repF_Solution1_57838_to_14170.csv")





#Xnum2<-c()
Xnumfum2<-c()
FEST2<-c()
anemia2<-c()
J<-c()
for (i in 1:length(anemia)) {
if ((Xnum[i,"riagendr"]==1)&&(Xnum[i,"ridageyr"]>14)&&(Xnum[i,"ridageyr"]<66)) {
J<-cbind(J,i)}}
#Xnum2<-Xnum[J,]
Xnumfum2<-Xnumfum[J,]
FEST2<-FEST[J]
anemia2<-anemia[J]


write.csv(Xnumfum2,"Xnumfum2_Male_Solution1_57838_to_19333.csv")
write.csv(anemia2,"anemia2_Male_Solution1_57838_to_19333.csv")
write.csv(FEST2,"FEST2_Male_Solution1_57838_to_19333.csv")




C<-read.csv("UC_C.csv",header=TRUE,sep=",")
C<-C[,-1]
D<-read.csv("UCPREG_D.csv",header=TRUE,sep=",")
D<-D[,-1]
E<-read.csv("UCPREG_E.csv",header=TRUE,sep=",")
E<-E[,-1]
F<-read.csv("UCPREG_F.csv",header=TRUE,sep=",")
F<-F[,-1]
G<-read.csv("UCPREG_G.csv",header=TRUE,sep=",")
G<-G[,-1]
H<-read.csv("UCPREG_H.csv",header=TRUE,sep=",")
H<-H[,-1]
I<-read.csv("UCPREG_I.csv",header=TRUE,sep=",")
I<-I[,-1]
J<-read.csv("UCPREG_J.csv",header=TRUE,sep=",")
J<-J[,-1]
P<-read.csv("P_UCPREG.csv",header=TRUE,sep=",")
P<-P[,-1]
CP<-rbind(C,D,E,F,G,H,I,J,P)
C2<-CP

C1<-read.csv("Xnumfum2_repF_Solution1_57838_to_14170.csv")
C1<-C1[,-1]
anemia<-read.csv("anemia2_repF_Solution1_57838_to_14170.csv")
anemia<-anemia[,-1]
FEST<-read.csv("FEST2_repF_Solution1_57838_to_14170.csv")
FEST<-FEST[,-1]

C1C2<-c()
I<-c()
J<-c()
for (i in 1:dim(C1)[1]) {
if (is.na(match(C1["seqn"][i,1],C2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1["seqn"][i,1],C2["seqn"][,1]))}}
C1C2<-cbind(C1[I,],C2[J,])


anemia2<-anemia[I]
FEST2<-FEST[I]

fe<-c()
for (i in 1:nrow(C1C2)) {
if (C1C2[i,"urxpreg"]==2) {
fe<-cbind(fe,i)}}

Fin_file<-C1C2[fe,]
Fin_file<-Fin_file[,-ncol(Fin_file)]
Fin_file<-Fin_file[,-ncol(Fin_file)]

anemia3<-anemia2[fe]
FEST3<-FEST2[fe]

dim(Fin_file)
#[1] 9516   78 This is how non-pregnant F15-49 is obtained

write.csv(Fin_file,"Xnumfum2_repF_NEG_UrTest_Solution1_14170_to_9516.csv")

write.csv(anemia3,"anemia2_repF_NEG_UrTest_Solution1_14170_to_9516.csv")

write.csv(FEST3,"FEST2_repF_NEG_UrTest_Solution1_14170_to_9516.csv")


