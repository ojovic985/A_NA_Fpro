#Download DEMO, CBC, BMX, DR1TOT, DR2TOT, DR1IFF, DR2IFF for Serie C-P (2003-2020)
#Then download DSQ1_C, DSQ1_D, DSQTOT_E-P, OHXDEN_C-P
#Then download DBQ, DIQ, FSQ, HIQ, HSQ, HUQ, IMQ, PAQ, SMQFAM for C-P
#Finally for urinary test of females F15-49 download UC_C, UCPREG_D-J and P_UCPREG files

Mer2<-function(X1,X2){
X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {   
if (is.na(match(X1["seqn"][i,1],X2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)   
J<-cbind(J,match(X1["seqn"][i,1],X2["seqn"][,1]))}
}
X1X2<-cbind(X1[I,],X2[J,])
return(X1X2)}


C1<-read.csv("DEMO_C.csv")
C2<-read.csv("CBC_C.csv")
C3<-read.csv("BMX_C.csv")
C4<-read.csv("DR1TOT_C.csv")
C5<-read.csv("DR2TOT_C.csv")

C1C2<-Mer2(C1,C2)
C1C2C3<-Mer2(C1C2,C3)
C1C2C3C4<-Mer2(C1C2C3,C4)
C1C2C3C4C5<-Mer2(C1C2C3C4,C5)


ABCE1<-C1C2C3C4C5

matr<-c()
for (i in 1:length(ABCE1[,"ridagemn"])) {
if (is.na(ABCE1[i,"ridagemn"])==FALSE) {
matr<-rbind(matr,ABCE1[i,])}}

matr2<-c()
for (i in 1:length(matr[,"bmxwt"])) {
if (is.na(matr[i,"bmxwt"])==FALSE) {
matr2<-rbind(matr2,matr[i,])}}

matr3<-c()
for (i in 1:length(matr2[,"lbxhgb"])) {
if (is.na(matr2[i,"lbxhgb"])==FALSE) {
matr3<-rbind(matr3,matr2[i,])}}

write.csv(matr3,"Ser_C_matr3_minBPX.csv") #ridagemn bmxwt lbxhgb

C1<-read.csv("DEMO_D.csv")
C2<-read.csv("CBC_D.csv")
C3<-read.csv("BMX_D.csv")
C4<-read.csv("DR1TOT_D.csv")
C5<-read.csv("DR2TOT_D.csv")

C1C2<-Mer2(C1,C2)
C1C2C3<-Mer2(C1C2,C3)
C1C2C3C4<-Mer2(C1C2C3,C4)
C1C2C3C4C5<-Mer2(C1C2C3C4,C5)


ABCE1<-C1C2C3C4C5

matr<-c()
for (i in 1:length(ABCE1[,"ridagemn"])) {
if (is.na(ABCE1[i,"ridagemn"])==FALSE) {
matr<-rbind(matr,ABCE1[i,])}}

matr2<-c()
for (i in 1:length(matr[,"bmxwt"])) {
if (is.na(matr[i,"bmxwt"])==FALSE) {
matr2<-rbind(matr2,matr[i,])}}

matr3<-c()
for (i in 1:length(matr2[,"lbxhgb"])) {
if (is.na(matr2[i,"lbxhgb"])==FALSE) {
matr3<-rbind(matr3,matr2[i,])}}

write.csv(matr3,"Ser_D_matr3_minBPX.csv") #ridagemn bmxwt lbxhgb


C1<-read.csv("DEMO_E.csv")
C2<-read.csv("CBC_E.csv")
C3<-read.csv("BMX_E.csv")
C4<-read.csv("DR1TOT_E.csv")
C5<-read.csv("DR2TOT_E.csv")

C1C2<-Mer2(C1,C2)
C1C2C3<-Mer2(C1C2,C3)
C1C2C3C4<-Mer2(C1C2C3,C4)
C1C2C3C4C5<-Mer2(C1C2C3C4,C5)


ABCE1<-C1C2C3C4C5

matr<-c()
for (i in 1:length(ABCE1[,"ridagemn"])) {
if (is.na(ABCE1[i,"ridagemn"])==FALSE) {
matr<-rbind(matr,ABCE1[i,])}}

matr2<-c()
for (i in 1:length(matr[,"bmxwt"])) {
if (is.na(matr[i,"bmxwt"])==FALSE) {
matr2<-rbind(matr2,matr[i,])}}

matr3<-c()
for (i in 1:length(matr2[,"lbxhgb"])) {
if (is.na(matr2[i,"lbxhgb"])==FALSE) {
matr3<-rbind(matr3,matr2[i,])}}

write.csv(matr3,"Ser_E_matr3_minBPX.csv") #ridagemn bmxwt lbxhgb


C1<-read.csv("DEMO_F.csv")
C2<-read.csv("CBC_F.csv")
C3<-read.csv("BMX_F.csv")
C4<-read.csv("DR1TOT_F.csv")
C5<-read.csv("DR2TOT_F.csv")

C1C2<-Mer2(C1,C2)
C1C2C3<-Mer2(C1C2,C3)
C1C2C3C4<-Mer2(C1C2C3,C4)
C1C2C3C4C5<-Mer2(C1C2C3C4,C5)


ABCE1<-C1C2C3C4C5

matr<-c()
for (i in 1:length(ABCE1[,"ridagemn"])) {
if (is.na(ABCE1[i,"ridagemn"])==FALSE) {
matr<-rbind(matr,ABCE1[i,])}}

matr2<-c()
for (i in 1:length(matr[,"bmxwt"])) {
if (is.na(matr[i,"bmxwt"])==FALSE) {
matr2<-rbind(matr2,matr[i,])}}

matr3<-c()
for (i in 1:length(matr2[,"lbxhgb"])) {
if (is.na(matr2[i,"lbxhgb"])==FALSE) {
matr3<-rbind(matr3,matr2[i,])}}

write.csv(matr3,"Ser_F_matr3_minBPX.csv") #ridagemn bmxwt lbxhgb


C1<-read.csv("DEMO_G.csv")
C2<-read.csv("CBC_G.csv")
C3<-read.csv("BMX_G.csv")
C4<-read.csv("DR1TOT_G.csv")
C5<-read.csv("DR2TOT_G.csv")

C1C2<-Mer2(C1,C2)
C1C2C3<-Mer2(C1C2,C3)
C1C2C3C4<-Mer2(C1C2C3,C4)
C1C2C3C4C5<-Mer2(C1C2C3C4,C5)


ABCE1<-C1C2C3C4C5

matr<-c()
for (i in 1:length(ABCE1[,"lbxhgb"])) {
if (is.na(ABCE1[i,"lbxhgb"])==FALSE) {
matr<-rbind(matr,ABCE1[i,])}}
matr2<-c()
for (i in 1:length(matr[,"bmxwt"])) {
if (is.na(matr[i,"bmxwt"])==FALSE) {
matr2<-rbind(matr2,matr[i,])}}

matr3<-c()
for (i in 1:length(matr2[,"ridageyr"])) {
if (is.na(matr2[i,"ridageyr"])==FALSE) {
matr3<-rbind(matr3,matr2[i,])}}

write.csv(matr3,"Ser_G_matr3_minBPX.csv") #ridageyr bmxwt lbxhgb but NOT ridagemn


C1<-read.csv("DEMO_H.csv")
C2<-read.csv("CBC_H.csv")
C3<-read.csv("BMX_H.csv")
C4<-read.csv("DR1TOT_H.csv")
C5<-read.csv("DR2TOT_H.csv")

C1C2<-Mer2(C1,C2)
C1C2C3<-Mer2(C1C2,C3)
C1C2C3C4<-Mer2(C1C2C3,C4)
C1C2C3C4C5<-Mer2(C1C2C3C4,C5)


ABCE1<-C1C2C3C4C5

matr<-c()
for (i in 1:length(ABCE1[,"lbxhgb"])) {
if (is.na(ABCE1[i,"lbxhgb"])==FALSE) {
matr<-rbind(matr,ABCE1[i,])}}
matr2<-c()
for (i in 1:length(matr[,"bmxwt"])) {
if (is.na(matr[i,"bmxwt"])==FALSE) {
matr2<-rbind(matr2,matr[i,])}}

matr3<-c()
for (i in 1:length(matr2[,"ridageyr"])) {
if (is.na(matr2[i,"ridageyr"])==FALSE) {
matr3<-rbind(matr3,matr2[i,])}}

write.csv(matr3,"Ser_H_matr3_minBPX.csv") #ridageyr bmxwt lbxhgb but NOT ridagemn


C1<-read.csv("DEMO_I.csv")
C2<-read.csv("CBC_I.csv")
C3<-read.csv("BMX_I.csv")
C4<-read.csv("DR1TOT_I.csv")
C5<-read.csv("DR2TOT_I.csv")

C1C2<-Mer2(C1,C2)
C1C2C3<-Mer2(C1C2,C3)
C1C2C3C4<-Mer2(C1C2C3,C4)
C1C2C3C4C5<-Mer2(C1C2C3C4,C5)


ABCE1<-C1C2C3C4C5

matr<-c()
for (i in 1:length(ABCE1[,"lbxhgb"])) {
if (is.na(ABCE1[i,"lbxhgb"])==FALSE) {
matr<-rbind(matr,ABCE1[i,])}}
matr2<-c()
for (i in 1:length(matr[,"bmxwt"])) {
if (is.na(matr[i,"bmxwt"])==FALSE) {
matr2<-rbind(matr2,matr[i,])}}

matr3<-c()
for (i in 1:length(matr2[,"ridageyr"])) {
if (is.na(matr2[i,"ridageyr"])==FALSE) {
matr3<-rbind(matr3,matr2[i,])}}

write.csv(matr3,"Ser_I_matr3_minBPX.csv") #ridageyr bmxwt lbxhgb but NOT ridagemn

C1<-read.csv("DEMO_J.csv")
C2<-read.csv("CBC_J.csv")
C3<-read.csv("BMX_J.csv")
C4<-read.csv("DR1TOT_J.csv")
C5<-read.csv("DR2TOT_J.csv")

C1C2<-Mer2(C1,C2)
C1C2C3<-Mer2(C1C2,C3)
C1C2C3C4<-Mer2(C1C2C3,C4)
C1C2C3C4C5<-Mer2(C1C2C3C4,C5)


ABCE1<-C1C2C3C4C5

matr<-c()
for (i in 1:length(ABCE1[,"lbxhgb"])) {
if (is.na(ABCE1[i,"lbxhgb"])==FALSE) {
matr<-rbind(matr,ABCE1[i,])}}
matr2<-c()
for (i in 1:length(matr[,"bmxwt"])) {
if (is.na(matr[i,"bmxwt"])==FALSE) {
matr2<-rbind(matr2,matr[i,])}}

matr3<-c()
for (i in 1:length(matr2[,"ridageyr"])) {
if (is.na(matr2[i,"ridageyr"])==FALSE) {
matr3<-rbind(matr3,matr2[i,])}}

write.csv(matr3,"Ser_J_matr3_minBPX.csv") #ridageyr bmxwt lbxhgb but NOT ridagemn


C1<-read.csv("DEMO_P.csv")
C2<-read.csv("CBC_P.csv")
C3<-read.csv("BMX_P.csv")
C4<-read.csv("DR1TOT_P.csv")
C5<-read.csv("DR2TOT_P.csv")

C1C2<-Mer2(C1,C2)
C1C2C3<-Mer2(C1C2,C3)
C1C2C3C4<-Mer2(C1C2C3,C4)
C1C2C3C4C5<-Mer2(C1C2C3C4,C5)


ABCE1<-C1C2C3C4C5

matr<-c()
for (i in 1:length(ABCE1[,"lbxhgb"])) {
if (is.na(ABCE1[i,"lbxhgb"])==FALSE) {
matr<-rbind(matr,ABCE1[i,])}}
matr2<-c()
for (i in 1:length(matr[,"bmxwt"])) {
if (is.na(matr[i,"bmxwt"])==FALSE) {
matr2<-rbind(matr2,matr[i,])}}

matr3<-c()
for (i in 1:length(matr2[,"ridageyr"])) {
if (is.na(matr2[i,"ridageyr"])==FALSE) {
matr3<-rbind(matr3,matr2[i,])}}

write.csv(matr3,"Ser_P_matr3_minBPX.csv") #ridageyr bmxwt lbxhgb but NOT ridagemn


C4<-read.csv("DR1IFF_C.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

for (i in 1:nrow(C4)) {
if (i==1) {
fes<-c(i)
fesm<-C4[i,"seqn"]
mark<-C4[i,"seqn"]}
else {
if (C4[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,C4[i,"seqn"])
mark<-C4[i,"seqn"]}
if (i==nrow(C4)) {
fes<-cbind(fes,i+1)}
}

}

C41<-C4[,-grep("dr1.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

C4<-Tur

C4<-C4[,-grep("wtdr2d", colnames(C4))]
C4<-C4[,-grep("dr1iline", colnames(C4))]
C4<-C4[,-grep("dr1drstz", colnames(C4))]
C4<-C4[,-grep("dr1exmer", colnames(C4))]
C4<-C4[,-grep("dr1lang", colnames(C4))]
C4<-C4[,-grep("dr1day", colnames(C4))]
C4<-C4[,-grep("dr1ccmnm", colnames(C4))]
C4<-C4[,-grep("dr1ccmtx", colnames(C4))]
C4<-C4[,-grep("dr1.030z", colnames(C4))]
C4<-C4[,-grep("dr1fs", colnames(C4))]
C4<-C4[,-grep("dr1.040z", colnames(C4))]
C4<-C4[,-grep("dr1ifdcd", colnames(C4))]
C4<-C4[,-grep("dr1mc", colnames(C4))]




C5<-read.csv("DR2IFF_C.csv")
C5<-C5[,-1]
fe<-c()
for (i in 1:nrow(C5)) {
if (is.na(C5[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C5<-C5[-fe,]

for (i in 1:nrow(C5)) {
if (i==1) {
fes<-c(i)
fesm<-C5[i,"seqn"]
mark<-C5[i,"seqn"]}
else {
if (C5[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,C5[i,"seqn"])
mark<-C5[i,"seqn"]}
if (i==nrow(C5)) {
fes<-cbind(fes,i+1)}
}

}

C51<-C5[,-grep("dr2.020", colnames(C5))]

Tur<-matrix(rep(NA, length(unique(C51[,"seqn"]))*ncol(C51)),nrow=length(unique(C51[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C51[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C51)

C5<-Tur





C5<-C5[,-grep("wtdrd1", colnames(C5))]
C5<-C5[,-grep("wtdr2d", colnames(C5))]
C5<-C5[,-grep("dr2iline", colnames(C5))]
C5<-C5[,-grep("dr2drstz", colnames(C5))]
C5<-C5[,-grep("dr2day", colnames(C5))]
C5<-C5[,-grep("dr2exmer", colnames(C5))]
C5<-C5[,-grep("drabf", colnames(C5))]
C5<-C5[,-grep("drdint", colnames(C5))]
C5<-C5[,-grep("dr2lang", colnames(C5))]
C5<-C5[,-grep("dr2ccmnm", colnames(C5))]
C5<-C5[,-grep("dr2ccmtx", colnames(C5))]
C5<-C5[,-grep("dr2.030z", colnames(C5))]
C5<-C5[,-grep("dr2fs", colnames(C5))]
C5<-C5[,-grep("dr2.040z", colnames(C5))]
C5<-C5[,-grep("dr2ifdcd", colnames(C5))]
C5<-C5[,-grep("dr2mc", colnames(C5))]



C1<-read.csv("Ser_C_matr3_minBPX.csv")
C1<-C1[,-c(1,2)]
C2<-read.csv("DSQ1_C.csv")
C2<-C2[,-1]
C3<-read.csv("OHXDEN_C.csv")
C3<-C3[,-1]

> dim(C1)
[1] 8114  345

C1C2<-c()
I<-c()
J<-c()
for (i in 1:dim(C1)[1]) {
if (is.na(match(C1["seqn"][i,1],C2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1["seqn"][i,1],C2["seqn"][,1]))}}
C1C2<-cbind(C1[I,],C2[J,])


C1C2C3<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2)[1]) {
if (is.na(match(C1C2["seqn"][i,1],C3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2["seqn"][i,1],C3["seqn"][,1]))}}
C1C2C3<-cbind(C1C2[I,],C3[J,])


C1C2C3C4<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2C3)[1]) {
if (is.na(match(C1C2C3["seqn"][i,1],C4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2C3["seqn"][i,1],C4[,"seqn"]))}}
C1C2C3C4<-cbind(C1C2C3[I,],C4[J,])

#> dim(C1C2C3C4)
#[1] 7490  556

C1C2C3C4C5<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2C3C4)[1]) {
if (is.na(match(C1C2C3C4["seqn"][i,1],C5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2C3C4["seqn"][i,1],C5[,"seqn"]))}}
C1C2C3C4C5<-cbind(C1C2C3C4[I,],C5[J,])

write.csv(C1C2C3C4C5,"Merged_FullSer_C_minBPX_minQuestion.csv")




D4<-read.csv("DR1IFF_D.csv")
D4<-D4[,-1]

fe<-c()
for (i in 1:nrow(D4)) {
if (is.na(D4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

D4<-D4[-fe,]

for (i in 1:nrow(D4)) {
if (i==1) {
fes<-c(i)
fesm<-D4[i,"seqn"]
mark<-D4[i,"seqn"]}
else {
if (D4[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,D4[i,"seqn"])
mark<-D4[i,"seqn"]}
if (i==nrow(D4)) {
fes<-cbind(fes,i+1)}
}

}

D41<-D4[,-grep("dr1.020", colnames(D4))]

Tur<-matrix(rep(NA, length(unique(D41[,"seqn"]))*ncol(D41)),nrow=length(unique(D41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(D41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(D41)

D4<-Tur

D4<-D4[,-grep("wtdr2d", colnames(D4))]
D4<-D4[,-grep("dr1iline", colnames(D4))]
D4<-D4[,-grep("dr1drstz", colnames(D4))]
D4<-D4[,-grep("dr1exmer", colnames(D4))]
D4<-D4[,-grep("dr1lang", colnames(D4))]
D4<-D4[,-grep("dr1day", colnames(D4))]
D4<-D4[,-grep("dr1ccmnm", colnames(D4))]
D4<-D4[,-grep("dr1ccmtx", colnames(D4))]
D4<-D4[,-grep("dr1.030z", colnames(D4))]
D4<-D4[,-grep("dr1fs", colnames(D4))]
D4<-D4[,-grep("dr1.040z", colnames(D4))]
D4<-D4[,-grep("dr1ifdcd", colnames(D4))]
D4<-D4[,-grep("dr1mc", colnames(D4))]




D5<-read.csv("DR2IFF_D.csv")
D5<-D5[,-1]
fe<-c()
for (i in 1:nrow(D5)) {
if (is.na(D5[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

D5<-D5[-fe,]

for (i in 1:nrow(D5)) {
if (i==1) {
fes<-c(i)
fesm<-D5[i,"seqn"]
mark<-D5[i,"seqn"]}
else {
if (D5[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,D5[i,"seqn"])
mark<-D5[i,"seqn"]}
if (i==nrow(D5)) {
fes<-cbind(fes,i+1)}
}

}

D51<-D5[,-grep("dr2.020", colnames(D5))]

Tur<-matrix(rep(NA, length(unique(D51[,"seqn"]))*ncol(D51)),nrow=length(unique(D51[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(D51[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(D51)

D5<-Tur





D5<-D5[,-grep("wtdrd1", colnames(D5))]
D5<-D5[,-grep("wtdr2d", colnames(D5))]
D5<-D5[,-grep("dr2iline", colnames(D5))]
D5<-D5[,-grep("dr2drstz", colnames(D5))]
D5<-D5[,-grep("dr2day", colnames(D5))]
D5<-D5[,-grep("dr2exmer", colnames(D5))]
D5<-D5[,-grep("drabf", colnames(D5))]
D5<-D5[,-grep("drdint", colnames(D5))]
D5<-D5[,-grep("dr2lang", colnames(D5))]
D5<-D5[,-grep("dr2ccmnm", colnames(D5))]
D5<-D5[,-grep("dr2ccmtx", colnames(D5))]
D5<-D5[,-grep("dr2.030z", colnames(D5))]
D5<-D5[,-grep("dr2fs", colnames(D5))]
D5<-D5[,-grep("dr2.040z", colnames(D5))]
D5<-D5[,-grep("dr2ifdcd", colnames(D5))]
D5<-D5[,-grep("dr2mc", colnames(D5))]



D1<-read.csv("Ser_D_matr3_minBPX.csv")
D1<-D1[,-c(1,2)]
D2<-read.csv("DSQ1_D.csv")
D2<-D2[,-1]
D3<-read.csv("OHXDEN_D.csv")
D3<-D3[,-1]



D1D2<-c()
I<-c()
J<-c()
for (i in 1:dim(D1)[1]) {
if (is.na(match(D1["seqn"][i,1],D2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1["seqn"][i,1],D2["seqn"][,1]))}}
D1D2<-cbind(D1[I,],D2[J,])


D1D2D3<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2)[1]) {
if (is.na(match(D1D2["seqn"][i,1],D3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2["seqn"][i,1],D3["seqn"][,1]))}}
D1D2D3<-cbind(D1D2[I,],D3[J,])

#> dim(D1)
#[1] 8202  336
#> dim(D1D2)
#[1] 8202  341
#> dim(D1D2D3)
#[1] 7404  400

D1D2D3D4<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2D3)[1]) {
if (is.na(match(D1D2D3["seqn"][i,1],D4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2D3["seqn"][i,1],D4[,"seqn"]))}}
D1D2D3D4<-cbind(D1D2D3[I,],D4[J,])



D1D2D3D4D5<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2D3D4)[1]) {
if (is.na(match(D1D2D3D4["seqn"][i,1],D5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2D3D4["seqn"][i,1],D5[,"seqn"]))}}
D1D2D3D4D5<-cbind(D1D2D3D4[I,],D5[J,])

write.csv(D1D2D3D4D5,"Merged_FullSer_D_minBPX_minQuestion.csv")


E4<-read.csv("DR1IFF_E.csv")
E4<-E4[,-1]

fe<-c()
for (i in 1:nrow(E4)) {
if (is.na(E4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

E4<-E4[-fe,]

for (i in 1:nrow(E4)) {
if (i==1) {
fes<-c(i)
fesm<-E4[i,"seqn"]
mark<-E4[i,"seqn"]}
else {
if (E4[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,E4[i,"seqn"])
mark<-E4[i,"seqn"]}
if (i==nrow(E4)) {
fes<-cbind(fes,i+1)}
}

}

E41<-E4[,-grep("dr1.020", colnames(E4))]

Tur<-matrix(rep(NA, length(unique(E41[,"seqn"]))*ncol(E41)),nrow=length(unique(E41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(E41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(E41)

E4<-Tur

E4<-E4[,-grep("dr1dbih", colnames(E4))]
E4<-E4[,-grep("wtdr2d", colnames(E4))]
E4<-E4[,-grep("dr1iline", colnames(E4))]
E4<-E4[,-grep("dr1drstz", colnames(E4))]
E4<-E4[,-grep("dr1exmer", colnames(E4))]
E4<-E4[,-grep("dr1lang", colnames(E4))]
E4<-E4[,-grep("dr1day", colnames(E4))]
E4<-E4[,-grep("dr1ccmnm", colnames(E4))]
E4<-E4[,-grep("dr1ccmtx", colnames(E4))]
E4<-E4[,-grep("dr1.030z", colnames(E4))]
E4<-E4[,-grep("dr1fs", colnames(E4))]
E4<-E4[,-grep("dr1.040z", colnames(E4))]
E4<-E4[,-grep("dr1ifdcd", colnames(E4))]
E4<-E4[,-grep("dr1mc", colnames(E4))]




E5<-read.csv("DR2IFF_E.csv")
E5<-E5[,-1]
fe<-c()
for (i in 1:nrow(E5)) {
if (is.na(E5[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

E5<-E5[-fe,]

for (i in 1:nrow(E5)) {
if (i==1) {
fes<-c(i)
fesm<-E5[i,"seqn"]
mark<-E5[i,"seqn"]}
else {
if (E5[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,E5[i,"seqn"])
mark<-E5[i,"seqn"]}
if (i==nrow(E5)) {
fes<-cbind(fes,i+1)}
}

}

E51<-E5[,-grep("dr2.020", colnames(E5))]

Tur<-matrix(rep(NA, length(unique(E51[,"seqn"]))*ncol(E51)),nrow=length(unique(E51[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(E51[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(E51)

E5<-Tur




E5<-E5[,-grep("dr2dbih", colnames(E5))]
E5<-E5[,-grep("wtdrd1", colnames(E5))]
E5<-E5[,-grep("wtdr2d", colnames(E5))]
E5<-E5[,-grep("dr2iline", colnames(E5))]
E5<-E5[,-grep("dr2drstz", colnames(E5))]
E5<-E5[,-grep("dr2day", colnames(E5))]
E5<-E5[,-grep("dr2exmer", colnames(E5))]
E5<-E5[,-grep("drabf", colnames(E5))]
E5<-E5[,-grep("drdint", colnames(E5))]
E5<-E5[,-grep("dr2lang", colnames(E5))]
E5<-E5[,-grep("dr2ccmnm", colnames(E5))]
E5<-E5[,-grep("dr2ccmtx", colnames(E5))]
E5<-E5[,-grep("dr2.030z", colnames(E5))]
E5<-E5[,-grep("dr2fs", colnames(E5))]
E5<-E5[,-grep("dr2.040z", colnames(E5))]
E5<-E5[,-grep("dr2ifdcd", colnames(E5))]
E5<-E5[,-grep("dr2mc", colnames(E5))]



E1<-read.csv("Ser_E_matr3_minBPX.csv")
E1<-E1[,-c(1,2)]
E2<-read.csv("DSQTOT_E.csv")
E2<-E2[,-1]
E3<-read.csv("OHXDEN_E.csv")
E3<-E3[,-1]



E1E2<-c()
I<-c()
J<-c()
for (i in 1:dim(E1)[1]) {
if (is.na(match(E1["seqn"][i,1],E2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1["seqn"][i,1],E2["seqn"][,1]))}}
E1E2<-cbind(E1[I,],E2[J,])


E1E2E3<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2)[1]) {
if (is.na(match(E1E2["seqn"][i,1],E3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2["seqn"][i,1],E3["seqn"][,1]))}}
E1E2E3<-cbind(E1E2[I,],E3[J,])

#> dim(E1)
#[1] 7820  338
#> dim(E1E2)
#[1] 7820  376
#> dim(E1E2E3)
#[1] 7140  435

E1E2E3E4<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2E3)[1]) {
if (is.na(match(E1E2E3["seqn"][i,1],E4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2E3["seqn"][i,1],E4[,"seqn"]))}}
E1E2E3E4<-cbind(E1E2E3[I,],E4[J,])

#> dim(E1E2E3E4)
#[1] 6856  505

E1E2E3E4E5<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2E3E4)[1]) {
if (is.na(match(E1E2E3E4["seqn"][i,1],E5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2E3E4["seqn"][i,1],E5[,"seqn"]))}}
E1E2E3E4E5<-cbind(E1E2E3E4[I,],E5[J,])

#> dim(E1E2E3E4E5)
#[1] 5880  572

write.csv(E1E2E3E4E5,"Merged_FullSer_E_minBPX_minQuestion.csv")


F4<-read.csv("DR1IFF_F.csv")
F4<-F4[,-1]

fe<-c()
for (i in 1:nrow(F4)) {
if (is.na(F4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

F4<-F4[-fe,]

for (i in 1:nrow(F4)) {
if (i==1) {
fes<-c(i)
fesm<-F4[i,"seqn"]
mark<-F4[i,"seqn"]}
else {
if (F4[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,F4[i,"seqn"])
mark<-F4[i,"seqn"]}
if (i==nrow(F4)) {
fes<-cbind(fes,i+1)}
}

}

F41<-F4[,-grep("dr1.020", colnames(F4))]

Tur<-matrix(rep(NA, length(unique(F41[,"seqn"]))*ncol(F41)),nrow=length(unique(F41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(F41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(F41)

F4<-Tur

F4<-F4[,-grep("dr1dbih", colnames(F4))]
F4<-F4[,-grep("wtdr2d", colnames(F4))]
F4<-F4[,-grep("dr1iline", colnames(F4))]
F4<-F4[,-grep("dr1drstz", colnames(F4))]
F4<-F4[,-grep("dr1exmer", colnames(F4))]
F4<-F4[,-grep("dr1lang", colnames(F4))]
F4<-F4[,-grep("dr1day", colnames(F4))]
F4<-F4[,-grep("dr1ccmnm", colnames(F4))]
F4<-F4[,-grep("dr1ccmtx", colnames(F4))]
F4<-F4[,-grep("dr1.030z", colnames(F4))]
F4<-F4[,-grep("dr1fs", colnames(F4))]
F4<-F4[,-grep("dr1.040z", colnames(F4))]
F4<-F4[,-grep("dr1ifdcd", colnames(F4))]
F4<-F4[,-grep("dr1mc", colnames(F4))]




F5<-read.csv("DR2IFF_F.csv")
F5<-F5[,-1]
fe<-c()
for (i in 1:nrow(F5)) {
if (is.na(F5[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

F5<-F5[-fe,]

for (i in 1:nrow(F5)) {
if (i==1) {
fes<-c(i)
fesm<-F5[i,"seqn"]
mark<-F5[i,"seqn"]}
else {
if (F5[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,F5[i,"seqn"])
mark<-F5[i,"seqn"]}
if (i==nrow(F5)) {
fes<-cbind(fes,i+1)}
}

}

F51<-F5[,-grep("dr2.020", colnames(F5))]

Tur<-matrix(rep(NA, length(unique(F51[,"seqn"]))*ncol(F51)),nrow=length(unique(F51[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(F51[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(F51)

F5<-Tur




F5<-F5[,-grep("dr2dbih", colnames(F5))]
F5<-F5[,-grep("wtdrd1", colnames(F5))]
F5<-F5[,-grep("wtdr2d", colnames(F5))]
F5<-F5[,-grep("dr2iline", colnames(F5))]
F5<-F5[,-grep("dr2drstz", colnames(F5))]
F5<-F5[,-grep("dr2day", colnames(F5))]
F5<-F5[,-grep("dr2exmer", colnames(F5))]
F5<-F5[,-grep("drabf", colnames(F5))]
F5<-F5[,-grep("drdint", colnames(F5))]
F5<-F5[,-grep("dr2lang", colnames(F5))]
F5<-F5[,-grep("dr2ccmnm", colnames(F5))]
F5<-F5[,-grep("dr2ccmtx", colnames(F5))]
F5<-F5[,-grep("dr2.030z", colnames(F5))]
F5<-F5[,-grep("dr2fs", colnames(F5))]
F5<-F5[,-grep("dr2.040z", colnames(F5))]
F5<-F5[,-grep("dr2ifdcd", colnames(F5))]
F5<-F5[,-grep("dr2mc", colnames(F5))]



F1<-read.csv("Ser_F_matr3_minBPX.csv")
F1<-F1[,-c(1,2)]
F2<-read.csv("DSQTOT_F.csv")
F2<-F2[,-1]
F3<-read.csv("OHXDEN_F.csv")
F3<-F3[,-1]



F1F2<-c()
I<-c()
J<-c()
for (i in 1:dim(F1)[1]) {
if (is.na(match(F1["seqn"][i,1],F2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1["seqn"][i,1],F2["seqn"][,1]))}}
F1F2<-cbind(F1[I,],F2[J,])


F1F2F3<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2)[1]) {
if (is.na(match(F1F2["seqn"][i,1],F3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2["seqn"][i,1],F3["seqn"][,1]))}}
F1F2F3<-cbind(F1F2[I,],F3[J,])

#> dim(F1)
#[1] 8362  340
#> dim(F1F2)
#[1] 8362  378
#> dim(F1F2F3)
#[1] 6976  417

F1F2F3F4<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2F3)[1]) {
if (is.na(match(F1F2F3["seqn"][i,1],F4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2F3["seqn"][i,1],F4[,"seqn"]))}}
F1F2F3F4<-cbind(F1F2F3[I,],F4[J,])

#> dim(F1F2F3F4)
#[1] 6722  487

F1F2F3F4F5<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2F3F4)[1]) {
if (is.na(match(F1F2F3F4["seqn"][i,1],F5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2F3F4["seqn"][i,1],F5[,"seqn"]))}}
F1F2F3F4F5<-cbind(F1F2F3F4[I,],F5[J,])

#> dim(F1F2F3F4F5)
#[1] 5859  554

write.csv(F1F2F3F4F5,"Merged_FullSer_F_minBPX_minQuestion.csv")

G4<-read.csv("DR1IFF_G.csv")
G4<-G4[,-1]

fe<-c()
for (i in 1:nrow(G4)) {
if (is.na(G4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

G4<-G4[-fe,]

for (i in 1:nrow(G4)) {
if (i==1) {
fes<-c(i)
fesm<-G4[i,"seqn"]
mark<-G4[i,"seqn"]}
else {
if (G4[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,G4[i,"seqn"])
mark<-G4[i,"seqn"]}
if (i==nrow(G4)) {
fes<-cbind(fes,i+1)}
}

}

G41<-G4[,-grep("dr1.020", colnames(G4))]

Tur<-matrix(rep(NA, length(unique(G41[,"seqn"]))*ncol(G41)),nrow=length(unique(G41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(G41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(G41)

G4<-Tur

G4<-G4[,-grep("dr1dbih", colnames(G4))]
G4<-G4[,-grep("wtdr2d", colnames(G4))]
G4<-G4[,-grep("dr1iline", colnames(G4))]
G4<-G4[,-grep("dr1drstz", colnames(G4))]
G4<-G4[,-grep("dr1exmer", colnames(G4))]
G4<-G4[,-grep("dr1lang", colnames(G4))]
G4<-G4[,-grep("dr1day", colnames(G4))]
G4<-G4[,-grep("dr1ccmnm", colnames(G4))]
G4<-G4[,-grep("dr1ccmtx", colnames(G4))]
G4<-G4[,-grep("dr1.030z", colnames(G4))]
G4<-G4[,-grep("dr1fs", colnames(G4))]
G4<-G4[,-grep("dr1.040z", colnames(G4))]
G4<-G4[,-grep("dr1ifdcd", colnames(G4))]
G4<-G4[,-grep("dr1mc", colnames(G4))]




G5<-read.csv("DR2IFF_G.csv")
G5<-G5[,-1]
fe<-c()
for (i in 1:nrow(G5)) {
if (is.na(G5[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

G5<-G5[-fe,]

for (i in 1:nrow(G5)) {
if (i==1) {
fes<-c(i)
fesm<-G5[i,"seqn"]
mark<-G5[i,"seqn"]}
else {
if (G5[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,G5[i,"seqn"])
mark<-G5[i,"seqn"]}
if (i==nrow(G5)) {
fes<-cbind(fes,i+1)}
}

}

G51<-G5[,-grep("dr2.020", colnames(G5))]

Tur<-matrix(rep(NA, length(unique(G51[,"seqn"]))*ncol(G51)),nrow=length(unique(G51[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(G51[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(G51)

G5<-Tur




G5<-G5[,-grep("dr2dbih", colnames(G5))]
G5<-G5[,-grep("wtdrd1", colnames(G5))]
G5<-G5[,-grep("wtdr2d", colnames(G5))]
G5<-G5[,-grep("dr2iline", colnames(G5))]
G5<-G5[,-grep("dr2drstz", colnames(G5))]
G5<-G5[,-grep("dr2day", colnames(G5))]
G5<-G5[,-grep("dr2exmer", colnames(G5))]
G5<-G5[,-grep("drabf", colnames(G5))]
G5<-G5[,-grep("drdint", colnames(G5))]
G5<-G5[,-grep("dr2lang", colnames(G5))]
G5<-G5[,-grep("dr2ccmnm", colnames(G5))]
G5<-G5[,-grep("dr2ccmtx", colnames(G5))]
G5<-G5[,-grep("dr2.030z", colnames(G5))]
G5<-G5[,-grep("dr2fs", colnames(G5))]
G5<-G5[,-grep("dr2.040z", colnames(G5))]
G5<-G5[,-grep("dr2ifdcd", colnames(G5))]
G5<-G5[,-grep("dr2mc", colnames(G5))]



G1<-read.csv("Ser_G_matr3_minBPX.csv")
G1<-G1[,-c(1,2)]
G2<-read.csv("DSQTOT_G.csv")
G2<-G2[,-1]
G3<-read.csv("OHXDEN_G.csv")
G3<-G3[,-1]



G1G2<-c()
I<-c()
J<-c()
for (i in 1:dim(G1)[1]) {
if (is.na(match(G1["seqn"][i,1],G2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1["seqn"][i,1],G2["seqn"][,1]))}}
G1G2<-cbind(G1[I,],G2[J,])


G1G2G3<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2)[1]) {
if (is.na(match(G1G2["seqn"][i,1],G3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2["seqn"][i,1],G3["seqn"][,1]))}}
G1G2G3<-cbind(G1G2[I,],G3[J,])

#> dim(G1)
#[1] 7871  348
#> dim(G1G2)
#[1] 7871  387
#> dim(G1G2G3)
#[1] 7871  497

G1G2G3G4<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2G3)[1]) {
if (is.na(match(G1G2G3["seqn"][i,1],G4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2G3["seqn"][i,1],G4[,"seqn"]))}}
G1G2G3G4<-cbind(G1G2G3[I,],G4[J,])

#> dim(G1G2G3G4)
#[1] 7276  567

G1G2G3G4G5<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2G3G4)[1]) {
if (is.na(match(G1G2G3G4["seqn"][i,1],G5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2G3G4["seqn"][i,1],G5[,"seqn"]))}}
G1G2G3G4G5<-cbind(G1G2G3G4[I,],G5[J,])

#> dim(G1G2G3G4G5)
#[1] 6546  634

write.csv(G1G2G3G4G5,"Merged_FullSer_G_minBPX_minQuestion.csv")

H4<-read.csv("DR1IFF_H.csv")
H4<-H4[,-1]

fe<-c()
for (i in 1:nrow(H4)) {
if (is.na(H4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

H4<-H4[-fe,]

for (i in 1:nrow(H4)) {
if (i==1) {
fes<-c(i)
fesm<-H4[i,"seqn"]
mark<-H4[i,"seqn"]}
else {
if (H4[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,H4[i,"seqn"])
mark<-H4[i,"seqn"]}
if (i==nrow(H4)) {
fes<-cbind(fes,i+1)}
}

}

H41<-H4[,-grep("dr1.020", colnames(H4))]

Tur<-matrix(rep(NA, length(unique(H41[,"seqn"]))*ncol(H41)),nrow=length(unique(H41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(H41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(H41)

H4<-Tur

H4<-H4[,-grep("dr1dbih", colnames(H4))]
H4<-H4[,-grep("wtdr2d", colnames(H4))]
H4<-H4[,-grep("dr1iline", colnames(H4))]
H4<-H4[,-grep("dr1drstz", colnames(H4))]
H4<-H4[,-grep("dr1exmer", colnames(H4))]
H4<-H4[,-grep("dr1lang", colnames(H4))]
H4<-H4[,-grep("dr1day", colnames(H4))]
H4<-H4[,-grep("dr1ccmnm", colnames(H4))]
H4<-H4[,-grep("dr1ccmtx", colnames(H4))]
H4<-H4[,-grep("dr1.030z", colnames(H4))]
H4<-H4[,-grep("dr1fs", colnames(H4))]
H4<-H4[,-grep("dr1.040z", colnames(H4))]
H4<-H4[,-grep("dr1ifdcd", colnames(H4))]




H5<-read.csv("DR2IFF_H.csv")
H5<-H5[,-1]
fe<-c()
for (i in 1:nrow(H5)) {
if (is.na(H5[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

H5<-H5[-fe,]

for (i in 1:nrow(H5)) {
if (i==1) {
fes<-c(i)
fesm<-H5[i,"seqn"]
mark<-H5[i,"seqn"]}
else {
if (H5[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,H5[i,"seqn"])
mark<-H5[i,"seqn"]}
if (i==nrow(H5)) {
fes<-cbind(fes,i+1)}
}

}

H51<-H5[,-grep("dr2.020", colnames(H5))]

Tur<-matrix(rep(NA, length(unique(H51[,"seqn"]))*ncol(H51)),nrow=length(unique(H51[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(H51[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(H51)

H5<-Tur




H5<-H5[,-grep("dr2dbih", colnames(H5))]
H5<-H5[,-grep("wtdrd1", colnames(H5))]
H5<-H5[,-grep("wtdr2d", colnames(H5))]
H5<-H5[,-grep("dr2iline", colnames(H5))]
H5<-H5[,-grep("dr2drstz", colnames(H5))]
H5<-H5[,-grep("dr2day", colnames(H5))]
H5<-H5[,-grep("dr2exmer", colnames(H5))]
H5<-H5[,-grep("drabf", colnames(H5))]
H5<-H5[,-grep("drdint", colnames(H5))]
H5<-H5[,-grep("dr2lang", colnames(H5))]
H5<-H5[,-grep("dr2ccmnm", colnames(H5))]
H5<-H5[,-grep("dr2ccmtx", colnames(H5))]
H5<-H5[,-grep("dr2.030z", colnames(H5))]
H5<-H5[,-grep("dr2fs", colnames(H5))]
H5<-H5[,-grep("dr2.040z", colnames(H5))]
H5<-H5[,-grep("dr2ifdcd", colnames(H5))]



H1<-read.csv("Ser_H_matr3_minBPX.csv")
H1<-H1[,-c(1,2)]
H2<-read.csv("DSQTOT_H.csv")
H2<-H2[,-1]
H3<-read.csv("OHXDEN_H.csv")
H3<-H3[,-1]



H1H2<-c()
I<-c()
J<-c()
for (i in 1:dim(H1)[1]) {
if (is.na(match(H1["seqn"][i,1],H2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1["seqn"][i,1],H2["seqn"][,1]))}}
H1H2<-cbind(H1[I,],H2[J,])


H1H2H3<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2)[1]) {
if (is.na(match(H1H2["seqn"][i,1],H3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2["seqn"][i,1],H3["seqn"][,1]))}}
H1H2H3<-cbind(H1H2[I,],H3[J,])

#> dim(H1)
#[1] 8470  351
#> dim(H1H2)
#[1] 8470  390
#> dim(H1H2H3)
#[1] 8470  500

H1H2H3H4<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2H3)[1]) {
if (is.na(match(H1H2H3["seqn"][i,1],H4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2H3["seqn"][i,1],H4[,"seqn"]))}}
H1H2H3H4<-cbind(H1H2H3[I,],H4[J,])

#> dim(H1H2H3H4)
#[1] 7607  570

H1H2H3H4H5<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2H3H4)[1]) {
if (is.na(match(H1H2H3H4["seqn"][i,1],H5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2H3H4["seqn"][i,1],H5[,"seqn"]))}}
H1H2H3H4H5<-cbind(H1H2H3H4[I,],H5[J,])

#> dim(H1H2H3H4H5)
#[1] 6691  637

write.csv(H1H2H3H4H5,"Merged_FullSer_H_minBPX_minQuestion.csv")

I4<-read.csv("DR1IFF_I.csv")
I4<-I4[,-1]

fe<-c()
for (i in 1:nrow(I4)) {
if (is.na(I4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

I4<-I4[-fe,]

for (i in 1:nrow(I4)) {
if (i==1) {
fes<-c(i)
fesm<-I4[i,"seqn"]
mark<-I4[i,"seqn"]}
else {
if (I4[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,I4[i,"seqn"])
mark<-I4[i,"seqn"]}
if (i==nrow(I4)) {
fes<-cbind(fes,i+1)}
}

}

I41<-I4[,-grep("dr1.020", colnames(I4))]

Tur<-matrix(rep(NA, length(unique(I41[,"seqn"]))*ncol(I41)),nrow=length(unique(I41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(I41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(I41)

I4<-Tur

I4<-I4[,-grep("dr1dbih", colnames(I4))]
I4<-I4[,-grep("wtdr2d", colnames(I4))]
I4<-I4[,-grep("dr1iline", colnames(I4))]
I4<-I4[,-grep("dr1drstz", colnames(I4))]
I4<-I4[,-grep("dr1exmer", colnames(I4))]
I4<-I4[,-grep("dr1lang", colnames(I4))]
I4<-I4[,-grep("dr1day", colnames(I4))]
I4<-I4[,-grep("dr1ccmnm", colnames(I4))]
I4<-I4[,-grep("dr1ccmtx", colnames(I4))]
I4<-I4[,-grep("dr1.030z", colnames(I4))]
I4<-I4[,-grep("dr1fs", colnames(I4))]
I4<-I4[,-grep("dr1.040z", colnames(I4))]
I4<-I4[,-grep("dr1ifdcd", colnames(I4))]




I5<-read.csv("DR2IFF_I.csv")
I5<-I5[,-1]
fe<-c()
for (i in 1:nrow(I5)) {
if (is.na(I5[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

I5<-I5[-fe,]

for (i in 1:nrow(I5)) {
if (i==1) {
fes<-c(i)
fesm<-I5[i,"seqn"]
mark<-I5[i,"seqn"]}
else {
if (I5[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,I5[i,"seqn"])
mark<-I5[i,"seqn"]}
if (i==nrow(I5)) {
fes<-cbind(fes,i+1)}
}

}

I51<-I5[,-grep("dr2.020", colnames(I5))]

Tur<-matrix(rep(NA, length(unique(I51[,"seqn"]))*ncol(I51)),nrow=length(unique(I51[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(I51[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(I51)

I5<-Tur




I5<-I5[,-grep("dr2dbih", colnames(I5))]
I5<-I5[,-grep("wtdrd1", colnames(I5))]
I5<-I5[,-grep("wtdr2d", colnames(I5))]
I5<-I5[,-grep("dr2iline", colnames(I5))]
I5<-I5[,-grep("dr2drstz", colnames(I5))]
I5<-I5[,-grep("dr2day", colnames(I5))]
I5<-I5[,-grep("dr2exmer", colnames(I5))]
I5<-I5[,-grep("drabf", colnames(I5))]
I5<-I5[,-grep("drdint", colnames(I5))]
I5<-I5[,-grep("dr2lang", colnames(I5))]
I5<-I5[,-grep("dr2ccmnm", colnames(I5))]
I5<-I5[,-grep("dr2ccmtx", colnames(I5))]
I5<-I5[,-grep("dr2.030z", colnames(I5))]
I5<-I5[,-grep("dr2fs", colnames(I5))]
I5<-I5[,-grep("dr2.040z", colnames(I5))]
I5<-I5[,-grep("dr2ifdcd", colnames(I5))]



I1<-read.csv("Ser_I_matr3_minBPX.csv")
I1<-I1[,-c(1,2)]
I2<-read.csv("DSQTOT_I.csv")
I2<-I2[,-1]
I3<-read.csv("OHXDEN_I.csv")
I3<-I3[,-1]



I1I2<-c()
I<-c()
J<-c()
for (i in 1:dim(I1)[1]) {
if (is.na(match(I1["seqn"][i,1],I2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1["seqn"][i,1],I2["seqn"][,1]))}}
I1I2<-cbind(I1[I,],I2[J,])


I1I2I3<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2)[1]) {
if (is.na(match(I1I2["seqn"][i,1],I3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2["seqn"][i,1],I3["seqn"][,1]))}}
I1I2I3<-cbind(I1I2[I,],I3[J,])

#> dim(I1)
#[1] 8041  351
#> dim(I1I2)
#[1] 8041  390
#> dim(I1I2I3)
#[1] 8041  560

I1I2I3I4<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2I3)[1]) {
if (is.na(match(I1I2I3["seqn"][i,1],I4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2I3["seqn"][i,1],I4[,"seqn"]))}}
I1I2I3I4<-cbind(I1I2I3[I,],I4[J,])

#> dim(I1I2I3I4)
#[1] 7335  630

I1I2I3I4I5<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2I3I4)[1]) {
if (is.na(match(I1I2I3I4["seqn"][i,1],I5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2I3I4["seqn"][i,1],I5[,"seqn"]))}}
I1I2I3I4I5<-cbind(I1I2I3I4[I,],I5[J,])

#> dim(I1I2I3I4I5)
#[1] 6083  697

write.csv(I1I2I3I4I5,"Merged_FullSer_I_minBPX_minQuestion.csv")


J4<-read.csv("DR1IFF_J.csv")
J4<-J4[,-1]

fe<-c()
for (i in 1:nrow(J4)) {
if (is.na(J4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

J4<-J4[-fe,]

for (i in 1:nrow(J4)) {
if (i==1) {
fes<-c(i)
fesm<-J4[i,"seqn"]
mark<-J4[i,"seqn"]}
else {
if (J4[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,J4[i,"seqn"])
mark<-J4[i,"seqn"]}
if (i==nrow(J4)) {
fes<-cbind(fes,i+1)}
}

}

J41<-J4[,-grep("dr1.020", colnames(J4))]

Tur<-matrix(rep(NA, length(unique(J41[,"seqn"]))*ncol(J41)),nrow=length(unique(J41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(J41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(J41)

J4<-Tur

J4<-J4[,-grep("dr1dbih", colnames(J4))]
J4<-J4[,-grep("wtdr2d", colnames(J4))]
J4<-J4[,-grep("dr1iline", colnames(J4))]
J4<-J4[,-grep("dr1drstz", colnames(J4))]
J4<-J4[,-grep("dr1exmer", colnames(J4))]
J4<-J4[,-grep("dr1lang", colnames(J4))]
J4<-J4[,-grep("dr1day", colnames(J4))]
J4<-J4[,-grep("dr1ccmnm", colnames(J4))]
J4<-J4[,-grep("dr1ccmtx", colnames(J4))]
J4<-J4[,-grep("dr1.030z", colnames(J4))]
J4<-J4[,-grep("dr1fs", colnames(J4))]
J4<-J4[,-grep("dr1.040z", colnames(J4))]
J4<-J4[,-grep("dr1ifdcd", colnames(J4))]




J5<-read.csv("DR2IFF_J.csv")
J5<-J5[,-1]
fe<-c()
for (i in 1:nrow(J5)) {
if (is.na(J5[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

J5<-J5[-fe,]

for (i in 1:nrow(J5)) {
if (i==1) {
fes<-c(i)
fesm<-J5[i,"seqn"]
mark<-J5[i,"seqn"]}
else {
if (J5[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,J5[i,"seqn"])
mark<-J5[i,"seqn"]}
if (i==nrow(J5)) {
fes<-cbind(fes,i+1)}
}

}

J51<-J5[,-grep("dr2.020", colnames(J5))]

Tur<-matrix(rep(NA, length(unique(J51[,"seqn"]))*ncol(J51)),nrow=length(unique(J51[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(J51[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(J51)

J5<-Tur




J5<-J5[,-grep("dr2dbih", colnames(J5))]
J5<-J5[,-grep("wtdrd1", colnames(J5))]
J5<-J5[,-grep("wtdr2d", colnames(J5))]
J5<-J5[,-grep("dr2iline", colnames(J5))]
J5<-J5[,-grep("dr2drstz", colnames(J5))]
J5<-J5[,-grep("dr2day", colnames(J5))]
J5<-J5[,-grep("dr2exmer", colnames(J5))]
J5<-J5[,-grep("drabf", colnames(J5))]
J5<-J5[,-grep("drdint", colnames(J5))]
J5<-J5[,-grep("dr2lang", colnames(J5))]
J5<-J5[,-grep("dr2ccmnm", colnames(J5))]
J5<-J5[,-grep("dr2ccmtx", colnames(J5))]
J5<-J5[,-grep("dr2.030z", colnames(J5))]
J5<-J5[,-grep("dr2fs", colnames(J5))]
J5<-J5[,-grep("dr2.040z", colnames(J5))]
J5<-J5[,-grep("dr2ifdcd", colnames(J5))]



J1<-read.csv("Ser_J_matr3_minBPX.csv")
J1<-J1[,-c(1,2)]
J2<-read.csv("DSQTOT_J.csv")
J2<-J2[,-1]
J3<-read.csv("OHXDEN_J.csv")
J3<-J3[,-1]



J1J2<-c()
I<-c()
J<-c()
for (i in 1:dim(J1)[1]) {
if (is.na(match(J1["seqn"][i,1],J2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1["seqn"][i,1],J2["seqn"][,1]))}}
J1J2<-cbind(J1[I,],J2[J,])


J1J2J3<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2)[1]) {
if (is.na(match(J1J2["seqn"][i,1],J3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2["seqn"][i,1],J3["seqn"][,1]))}}
J1J2J3<-cbind(J1J2[I,],J3[J,])

> dim(J1)
[1] 7426  346
> dim(J1J2)
[1] 7426  385
> dim(J1J2J3)
[1] 7426  555

J1J2J3J4<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2J3)[1]) {
if (is.na(match(J1J2J3["seqn"][i,1],J4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2J3["seqn"][i,1],J4[,"seqn"]))}}
J1J2J3J4<-cbind(J1J2J3[I,],J4[J,])

#> dim(J1J2J3J4)
#[1] 6676  625

J1J2J3J4J5<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2J3J4)[1]) {
if (is.na(match(J1J2J3J4["seqn"][i,1],J5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2J3J4["seqn"][i,1],J5[,"seqn"]))}}
J1J2J3J4J5<-cbind(J1J2J3J4[I,],J5[J,])

#> dim(J1J2J3J4J5)
#[1] 5818  692

write.csv(J1J2J3J4J5,"Merged_FullSer_J_minBPX_minQuestion.csv")


P4<-read.csv("DR1IFF_P.csv")
P4<-P4[,-1]

fe<-c()
for (i in 1:nrow(P4)) {
if (is.na(P4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

P4<-P4[-fe,]

for (i in 1:nrow(P4)) {
if (i==1) {
fes<-c(i)
fesm<-P4[i,"seqn"]
mark<-P4[i,"seqn"]}
else {
if (P4[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,P4[i,"seqn"])
mark<-P4[i,"seqn"]}
if (i==nrow(P4)) {
fes<-cbind(fes,i+1)}
}

}

P41<-P4[,-grep("dr1.020", colnames(P4))]

Tur<-matrix(rep(NA, length(unique(P41[,"seqn"]))*ncol(P41)),nrow=length(unique(P41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(P41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(P41)

P4<-Tur

P4<-P4[,-grep("dr1dbih", colnames(P4))]
P4<-P4[,-grep("wtdr2d", colnames(P4))]
P4<-P4[,-grep("dr1iline", colnames(P4))]
P4<-P4[,-grep("dr1drstz", colnames(P4))]
P4<-P4[,-grep("dr1exmer", colnames(P4))]
P4<-P4[,-grep("dr1lang", colnames(P4))]
P4<-P4[,-grep("dr1day", colnames(P4))]
P4<-P4[,-grep("dr1ccmnm", colnames(P4))]
P4<-P4[,-grep("dr1ccmtx", colnames(P4))]
P4<-P4[,-grep("dr1.030z", colnames(P4))]
P4<-P4[,-grep("dr1fs", colnames(P4))]
P4<-P4[,-grep("dr1.040z", colnames(P4))]
P4<-P4[,-grep("dr1ifdcd", colnames(P4))]




P5<-read.csv("DR2IFF_P.csv")
P5<-P5[,-1]
fe<-c()
for (i in 1:nrow(P5)) {
if (is.na(P5[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

P5<-P5[-fe,]

for (i in 1:nrow(P5)) {
if (i==1) {
fes<-c(i)
fesm<-P5[i,"seqn"]
mark<-P5[i,"seqn"]}
else {
if (P5[i,"seqn"]!=mark) {
fes<-cbind(fes,c(i))
fesm<-cbind(fesm,P5[i,"seqn"])
mark<-P5[i,"seqn"]}
if (i==nrow(P5)) {
fes<-cbind(fes,i+1)}
}

}

P51<-P5[,-grep("dr2.020", colnames(P5))]

Tur<-matrix(rep(NA, length(unique(P51[,"seqn"]))*ncol(P51)),nrow=length(unique(P51[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(P51[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(P51)

P5<-Tur




P5<-P5[,-grep("dr2dbih", colnames(P5))]
P5<-P5[,-grep("wtdrd1", colnames(P5))]
P5<-P5[,-grep("wtdr2d", colnames(P5))]
P5<-P5[,-grep("dr2iline", colnames(P5))]
P5<-P5[,-grep("dr2drstz", colnames(P5))]
P5<-P5[,-grep("dr2day", colnames(P5))]
P5<-P5[,-grep("dr2exmer", colnames(P5))]
P5<-P5[,-grep("drabf", colnames(P5))]
P5<-P5[,-grep("drdint", colnames(P5))]
P5<-P5[,-grep("dr2lang", colnames(P5))]
P5<-P5[,-grep("dr2ccmnm", colnames(P5))]
P5<-P5[,-grep("dr2ccmtx", colnames(P5))]
P5<-P5[,-grep("dr2.030z", colnames(P5))]
P5<-P5[,-grep("dr2fs", colnames(P5))]
P5<-P5[,-grep("dr2.040z", colnames(P5))]
P5<-P5[,-grep("dr2ifdcd", colnames(P5))]



P1<-read.csv("Ser_P_matr3_II.csv") # Ser_P_matr3_minBPX.csv
P1<-P1[,-c(1,2)]
P2<-read.csv("P_DSQTOT.csv")
P2<-P2[,-1]
P3<-read.csv("P_OHXDEN.csv")
P3<-P3[,-1]



P1P2<-c()
I<-c()
J<-c()
for (i in 1:dim(P1)[1]) {
if (is.na(match(P1["seqn"][i,1],P2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1["seqn"][i,1],P2["seqn"][,1]))}}
P1P2<-cbind(P1[I,],P2[J,])


P1P2P3<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2)[1]) {
if (is.na(match(P1P2["seqn"][i,1],P3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2["seqn"][i,1],P3["seqn"][,1]))}}
P1P2P3<-cbind(P1P2[I,],P3[J,])


P1P2P3P4<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2P3)[1]) {
if (is.na(match(P1P2P3["seqn"][i,1],P4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2P3["seqn"][i,1],P4[,"seqn"]))}}
P1P2P3P4<-cbind(P1P2P3[I,],P4[J,])

P1P2P3P4P5<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2P3P4)[1]) {
if (is.na(match(P1P2P3P4["seqn"][i,1],P5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2P3P4["seqn"][i,1],P5[,"seqn"]))}}
P1P2P3P4P5<-cbind(P1P2P3P4[I,],P5[J,])

write.csv(P1P2P3P4P5,"Merged_FullSer_P_minBPX_minQuestion.csv")



C1<-read.csv("DBQ_C.csv")
C1<-C1[,-1]
C2<-read.csv("DIQ_C.csv")
C2<-C2[,-1]
C3<-read.csv("FSQ_C.csv")
C3<-C3[,-1]
C4<-read.csv("HIQ_C.csv")
C4<-C4[,-1]
C5<-read.csv("HSQ_C.csv")
C5<-C5[,-1]
C6<-read.csv("HUQ_C.csv")
C6<-C6[,-1]
C7<-read.csv("IMQ_C.csv")
C7<-C7[,-1]
C8<-read.csv("MCQ_C.csv")
C8<-C8[,-1]
C9<-read.csv("PAQ_C.csv")
C9<-C9[,-1]
C10<-read.csv("SMQFAM_C.csv")
C10<-C10[,-1]

C1C2<-c()
I<-c()
J<-c()
for (i in 1:dim(C1)[1]) {
if (is.na(match(C1["seqn"][i,1],C2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1["seqn"][i,1],C2["seqn"][,1]))}}
C1C2<-cbind(C1[I,],C2[J,])


C1C2C3<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2)[1]) {
if (is.na(match(C1C2["seqn"][i,1],C3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2["seqn"][i,1],C3["seqn"][,1]))}}
C1C2C3<-cbind(C1C2[I,],C3[J,])


C1C2C3C4<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2C3)[1]) {
if (is.na(match(C1C2C3["seqn"][i,1],C4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2C3["seqn"][i,1],C4[,"seqn"]))}}
C1C2C3C4<-cbind(C1C2C3[I,],C4[J,])

C1C2C3C4C5<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2C3C4)[1]) {
if (is.na(match(C1C2C3C4["seqn"][i,1],C5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2C3C4["seqn"][i,1],C5[,"seqn"]))}}
C1C2C3C4C5<-cbind(C1C2C3C4[I,],C5[J,])

C1C2C3C4C5C6<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2C3C4C5)[1]) {
if (is.na(match(C1C2C3C4C5["seqn"][i,1],C6[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2C3C4C5["seqn"][i,1],C6[,"seqn"]))}}
C1C2C3C4C5C6<-cbind(C1C2C3C4C5[I,],C6[J,])


C1C2C3C4C5C6C7<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2C3C4C5C6)[1]) {
if (is.na(match(C1C2C3C4C5C6["seqn"][i,1],C7[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2C3C4C5C6["seqn"][i,1],C7[,"seqn"]))}}
C1C2C3C4C5C6C7<-cbind(C1C2C3C4C5C6[I,],C7[J,])


C1C2C3C4C5C6C7C8<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2C3C4C5C6C7)[1]) {
if (is.na(match(C1C2C3C4C5C6C7["seqn"][i,1],C8[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2C3C4C5C6C7["seqn"][i,1],C8[,"seqn"]))}}
C1C2C3C4C5C6C7C8<-cbind(C1C2C3C4C5C6C7[I,],C8[J,])


C1C2C3C4C5C6C7C8C9<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2C3C4C5C6C7C8)[1]) {
if (is.na(match(C1C2C3C4C5C6C7C8["seqn"][i,1],C9[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2C3C4C5C6C7C8["seqn"][i,1],C9[,"seqn"]))}}
C1C2C3C4C5C6C7C8C9<-cbind(C1C2C3C4C5C6C7C8[I,],C9[J,])


C1C2C3C4C5C6C7C8C9C10<-c()
I<-c()
J<-c()
for (i in 1:dim(C1C2C3C4C5C6C7C8C9)[1]) {
if (is.na(match(C1C2C3C4C5C6C7C8C9["seqn"][i,1],C10[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1C2C3C4C5C6C7C8C9["seqn"][i,1],C10[,"seqn"]))}}
C1C2C3C4C5C6C7C8C9C10<-cbind(C1C2C3C4C5C6C7C8C9[I,],C10[J,])

write.csv(C1C2C3C4C5C6C7C8C9C10,"Merged_Ser_C_Questionnaire.csv")




D1<-read.csv("DBQ_D.csv")
D1<-D1[,-1]
D2<-read.csv("DIQ_D.csv")
D2<-D2[,-1]
D3<-read.csv("FSQ_D.csv")
D3<-D3[,-1]
D4<-read.csv("HIQ_D.csv")
D4<-D4[,-1]
D5<-read.csv("HSQ_D.csv")
D5<-D5[,-1]
D6<-read.csv("HUQ_D.csv")
D6<-D6[,-1]
D7<-read.csv("IMQ_D.csv")
D7<-D7[,-1]
D8<-read.csv("MCQ_D.csv")
D8<-D8[,-1]
D9<-read.csv("PAQ_D.csv")
D9<-D9[,-1]
D10<-read.csv("SMQFAM_D.csv")
D10<-D10[,-1]

D1D2<-c()
I<-c()
J<-c()
for (i in 1:dim(D1)[1]) {
if (is.na(match(D1["seqn"][i,1],D2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1["seqn"][i,1],D2["seqn"][,1]))}}
D1D2<-cbind(D1[I,],D2[J,])


D1D2D3<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2)[1]) {
if (is.na(match(D1D2["seqn"][i,1],D3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2["seqn"][i,1],D3["seqn"][,1]))}}
D1D2D3<-cbind(D1D2[I,],D3[J,])


D1D2D3D4<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2D3)[1]) {
if (is.na(match(D1D2D3["seqn"][i,1],D4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2D3["seqn"][i,1],D4[,"seqn"]))}}
D1D2D3D4<-cbind(D1D2D3[I,],D4[J,])


D1D2D3D4D5<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2D3D4)[1]) {
if (is.na(match(D1D2D3D4["seqn"][i,1],D5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2D3D4["seqn"][i,1],D5[,"seqn"]))}}
D1D2D3D4D5<-cbind(D1D2D3D4[I,],D5[J,])

D1D2D3D4D5D6<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2D3D4D5)[1]) {
if (is.na(match(D1D2D3D4D5["seqn"][i,1],D6[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2D3D4D5["seqn"][i,1],D6[,"seqn"]))}}
D1D2D3D4D5D6<-cbind(D1D2D3D4D5[I,],D6[J,])


D1D2D3D4D5D6D7<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2D3D4D5D6)[1]) {
if (is.na(match(D1D2D3D4D5D6["seqn"][i,1],D7[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2D3D4D5D6["seqn"][i,1],D7[,"seqn"]))}}
D1D2D3D4D5D6D7<-cbind(D1D2D3D4D5D6[I,],D7[J,])


D1D2D3D4D5D6D7D8<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2D3D4D5D6D7)[1]) {
if (is.na(match(D1D2D3D4D5D6D7["seqn"][i,1],D8[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2D3D4D5D6D7["seqn"][i,1],D8[,"seqn"]))}}
D1D2D3D4D5D6D7D8<-cbind(D1D2D3D4D5D6D7[I,],D8[J,])


D1D2D3D4D5D6D7D8D9<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2D3D4D5D6D7D8)[1]) {
if (is.na(match(D1D2D3D4D5D6D7D8["seqn"][i,1],D9[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2D3D4D5D6D7D8["seqn"][i,1],D9[,"seqn"]))}}
D1D2D3D4D5D6D7D8D9<-cbind(D1D2D3D4D5D6D7D8[I,],D9[J,])


D1D2D3D4D5D6D7D8D9D10<-c()
I<-c()
J<-c()
for (i in 1:dim(D1D2D3D4D5D6D7D8D9)[1]) {
if (is.na(match(D1D2D3D4D5D6D7D8D9["seqn"][i,1],D10[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(D1D2D3D4D5D6D7D8D9["seqn"][i,1],D10[,"seqn"]))}}
D1D2D3D4D5D6D7D8D9D10<-cbind(D1D2D3D4D5D6D7D8D9[I,],D10[J,])


write.csv(D1D2D3D4D5D6D7D8D9D10,"Merged_Ser_D_Questionnaire.csv")

E1<-read.csv("DBQ_E.csv")
E1<-E1[,-1]
E2<-read.csv("DIQ_E.csv")
E2<-E2[,-1]
E3<-read.csv("FSQ_E.csv")
E3<-E3[,-1]
E4<-read.csv("HIQ_E.csv")
E4<-E4[,-1]
E5<-read.csv("HSQ_E.csv")
E5<-E5[,-1]
E6<-read.csv("HUQ_E.csv")
E6<-E6[,-1]
E7<-read.csv("IMQ_E.csv")
E7<-E7[,-1]
E8<-read.csv("MCQ_E.csv")
E8<-E8[,-1]
E9<-read.csv("PAQ_E.csv")
E9<-E9[,-1]
E10<-read.csv("SMQFAM_E.csv")
E10<-E10[,-1]

E1E2<-c()
I<-c()
J<-c()
for (i in 1:dim(E1)[1]) {
if (is.na(match(E1["seqn"][i,1],E2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1["seqn"][i,1],E2["seqn"][,1]))}}
E1E2<-cbind(E1[I,],E2[J,])


E1E2E3<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2)[1]) {
if (is.na(match(E1E2["seqn"][i,1],E3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2["seqn"][i,1],E3["seqn"][,1]))}}
E1E2E3<-cbind(E1E2[I,],E3[J,])


E1E2E3E4<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2E3)[1]) {
if (is.na(match(E1E2E3["seqn"][i,1],E4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2E3["seqn"][i,1],E4[,"seqn"]))}}
E1E2E3E4<-cbind(E1E2E3[I,],E4[J,])

E1E2E3E4E5<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2E3E4)[1]) {
if (is.na(match(E1E2E3E4["seqn"][i,1],E5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2E3E4["seqn"][i,1],E5[,"seqn"]))}}
E1E2E3E4E5<-cbind(E1E2E3E4[I,],E5[J,])

E1E2E3E4E5E6<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2E3E4E5)[1]) {
if (is.na(match(E1E2E3E4E5["seqn"][i,1],E6[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2E3E4E5["seqn"][i,1],E6[,"seqn"]))}}
E1E2E3E4E5E6<-cbind(E1E2E3E4E5[I,],E6[J,])


E1E2E3E4E5E6E7<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2E3E4E5E6)[1]) {
if (is.na(match(E1E2E3E4E5E6["seqn"][i,1],E7[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2E3E4E5E6["seqn"][i,1],E7[,"seqn"]))}}
E1E2E3E4E5E6E7<-cbind(E1E2E3E4E5E6[I,],E7[J,])


E1E2E3E4E5E6E7E8<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2E3E4E5E6E7)[1]) {
if (is.na(match(E1E2E3E4E5E6E7["seqn"][i,1],E8[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2E3E4E5E6E7["seqn"][i,1],E8[,"seqn"]))}}
E1E2E3E4E5E6E7E8<-cbind(E1E2E3E4E5E6E7[I,],E8[J,])


E1E2E3E4E5E6E7E8E9<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2E3E4E5E6E7E8)[1]) {
if (is.na(match(E1E2E3E4E5E6E7E8["seqn"][i,1],E9[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2E3E4E5E6E7E8["seqn"][i,1],E9[,"seqn"]))}}
E1E2E3E4E5E6E7E8E9<-cbind(E1E2E3E4E5E6E7E8[I,],E9[J,])


E1E2E3E4E5E6E7E8E9E10<-c()
I<-c()
J<-c()
for (i in 1:dim(E1E2E3E4E5E6E7E8E9)[1]) {
if (is.na(match(E1E2E3E4E5E6E7E8E9["seqn"][i,1],E10[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(E1E2E3E4E5E6E7E8E9["seqn"][i,1],E10[,"seqn"]))}}
E1E2E3E4E5E6E7E8E9E10<-cbind(E1E2E3E4E5E6E7E8E9[I,],E10[J,])

write.csv(E1E2E3E4E5E6E7E8E9E10,"Merged_Ser_E_Questionnaire.csv")

F1<-read.csv("DBQ_F.csv")
F1<-F1[,-1]
F2<-read.csv("DIQ_F.csv")
F2<-F2[,-1]
F3<-read.csv("FSQ_F.csv")
F3<-F3[,-1]
F4<-read.csv("HIQ_F.csv")
F4<-F4[,-1]
F5<-read.csv("HSQ_F.csv")
F5<-F5[,-1]
F6<-read.csv("HUQ_F.csv")
F6<-F6[,-1]
F7<-read.csv("IMQ_F.csv")
F7<-F7[,-1]
F8<-read.csv("MCQ_F.csv")
F8<-F8[,-1]
F9<-read.csv("PAQ_F.csv")
F9<-F9[,-1]
F10<-read.csv("SMQFAM_F.csv")
F10<-F10[,-1]

F1F2<-c()
I<-c()
J<-c()
for (i in 1:dim(F1)[1]) {
if (is.na(match(F1["seqn"][i,1],F2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1["seqn"][i,1],F2["seqn"][,1]))}}
F1F2<-cbind(F1[I,],F2[J,])


F1F2F3<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2)[1]) {
if (is.na(match(F1F2["seqn"][i,1],F3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2["seqn"][i,1],F3["seqn"][,1]))}}
F1F2F3<-cbind(F1F2[I,],F3[J,])


F1F2F3F4<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2F3)[1]) {
if (is.na(match(F1F2F3["seqn"][i,1],F4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2F3["seqn"][i,1],F4[,"seqn"]))}}
F1F2F3F4<-cbind(F1F2F3[I,],F4[J,])


F1F2F3F4F5<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2F3F4)[1]) {
if (is.na(match(F1F2F3F4["seqn"][i,1],F5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2F3F4["seqn"][i,1],F5[,"seqn"]))}}
F1F2F3F4F5<-cbind(F1F2F3F4[I,],F5[J,])

F1F2F3F4F5F6<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2F3F4F5)[1]) {
if (is.na(match(F1F2F3F4F5["seqn"][i,1],F6[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2F3F4F5["seqn"][i,1],F6[,"seqn"]))}}
F1F2F3F4F5F6<-cbind(F1F2F3F4F5[I,],F6[J,])


F1F2F3F4F5F6F7<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2F3F4F5F6)[1]) {
if (is.na(match(F1F2F3F4F5F6["seqn"][i,1],F7[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2F3F4F5F6["seqn"][i,1],F7[,"seqn"]))}}
F1F2F3F4F5F6F7<-cbind(F1F2F3F4F5F6[I,],F7[J,])


F1F2F3F4F5F6F7F8<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2F3F4F5F6F7)[1]) {
if (is.na(match(F1F2F3F4F5F6F7["seqn"][i,1],F8[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2F3F4F5F6F7["seqn"][i,1],F8[,"seqn"]))}}
F1F2F3F4F5F6F7F8<-cbind(F1F2F3F4F5F6F7[I,],F8[J,])


F1F2F3F4F5F6F7F8F9<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2F3F4F5F6F7F8)[1]) {
if (is.na(match(F1F2F3F4F5F6F7F8["seqn"][i,1],F9[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2F3F4F5F6F7F8["seqn"][i,1],F9[,"seqn"]))}}
F1F2F3F4F5F6F7F8F9<-cbind(F1F2F3F4F5F6F7F8[I,],F9[J,])


F1F2F3F4F5F6F7F8F9F10<-c()
I<-c()
J<-c()
for (i in 1:dim(F1F2F3F4F5F6F7F8F9)[1]) {
if (is.na(match(F1F2F3F4F5F6F7F8F9["seqn"][i,1],F10[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(F1F2F3F4F5F6F7F8F9["seqn"][i,1],F10[,"seqn"]))}}
F1F2F3F4F5F6F7F8F9F10<-cbind(F1F2F3F4F5F6F7F8F9[I,],F10[J,])

write.csv(F1F2F3F4F5F6F7F8F9F10,"Merged_Ser_F_Questionnaire.csv")

G1<-read.csv("DBQ_G.csv")
G1<-G1[,-1]
G2<-read.csv("DIQ_G.csv")
G2<-G2[,-1]
G3<-read.csv("FSQ_G.csv")
G3<-G3[,-1]
G4<-read.csv("HIQ_G.csv")
G4<-G4[,-1]
G5<-read.csv("HSQ_G.csv")
G5<-G5[,-1]
G6<-read.csv("HUQ_G.csv")
G6<-G6[,-1]
G7<-read.csv("IMQ_G.csv")
G7<-G7[,-1]
G8<-read.csv("MCQ_G.csv")
G8<-G8[,-1]
G9<-read.csv("PAQ_G.csv")
G9<-G9[,-1]
G10<-read.csv("SMQFAM_G.csv")
G10<-G10[,-1]

G1G2<-c()
I<-c()
J<-c()
for (i in 1:dim(G1)[1]) {
if (is.na(match(G1["seqn"][i,1],G2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1["seqn"][i,1],G2["seqn"][,1]))}}
G1G2<-cbind(G1[I,],G2[J,])


G1G2G3<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2)[1]) {
if (is.na(match(G1G2["seqn"][i,1],G3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2["seqn"][i,1],G3["seqn"][,1]))}}
G1G2G3<-cbind(G1G2[I,],G3[J,])


G1G2G3G4<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2G3)[1]) {
if (is.na(match(G1G2G3["seqn"][i,1],G4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2G3["seqn"][i,1],G4[,"seqn"]))}}
G1G2G3G4<-cbind(G1G2G3[I,],G4[J,])


G1G2G3G4G5<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2G3G4)[1]) {
if (is.na(match(G1G2G3G4["seqn"][i,1],G5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2G3G4["seqn"][i,1],G5[,"seqn"]))}}
G1G2G3G4G5<-cbind(G1G2G3G4[I,],G5[J,])

G1G2G3G4G5G6<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2G3G4G5)[1]) {
if (is.na(match(G1G2G3G4G5["seqn"][i,1],G6[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2G3G4G5["seqn"][i,1],G6[,"seqn"]))}}
G1G2G3G4G5G6<-cbind(G1G2G3G4G5[I,],G6[J,])


G1G2G3G4G5G6G7<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2G3G4G5G6)[1]) {
if (is.na(match(G1G2G3G4G5G6["seqn"][i,1],G7[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2G3G4G5G6["seqn"][i,1],G7[,"seqn"]))}}
G1G2G3G4G5G6G7<-cbind(G1G2G3G4G5G6[I,],G7[J,])


G1G2G3G4G5G6G7G8<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2G3G4G5G6G7)[1]) {
if (is.na(match(G1G2G3G4G5G6G7["seqn"][i,1],G8[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2G3G4G5G6G7["seqn"][i,1],G8[,"seqn"]))}}
G1G2G3G4G5G6G7G8<-cbind(G1G2G3G4G5G6G7[I,],G8[J,])


G1G2G3G4G5G6G7G8G9<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2G3G4G5G6G7G8)[1]) {
if (is.na(match(G1G2G3G4G5G6G7G8["seqn"][i,1],G9[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2G3G4G5G6G7G8["seqn"][i,1],G9[,"seqn"]))}}
G1G2G3G4G5G6G7G8G9<-cbind(G1G2G3G4G5G6G7G8[I,],G9[J,])


G1G2G3G4G5G6G7G8G9G10<-c()
I<-c()
J<-c()
for (i in 1:dim(G1G2G3G4G5G6G7G8G9)[1]) {
if (is.na(match(G1G2G3G4G5G6G7G8G9["seqn"][i,1],G10[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(G1G2G3G4G5G6G7G8G9["seqn"][i,1],G10[,"seqn"]))}}
G1G2G3G4G5G6G7G8G9G10<-cbind(G1G2G3G4G5G6G7G8G9[I,],G10[J,])

write.csv(G1G2G3G4G5G6G7G8G9G10,"Merged_Ser_G_Questionnaire.csv")

H1<-read.csv("DBQ_H.csv")
H1<-H1[,-1]
H2<-read.csv("DIQ_H.csv")
H2<-H2[,-1]
H3<-read.csv("FSQ_H.csv")
H3<-H3[,-1]
H4<-read.csv("HIQ_H.csv")
H4<-H4[,-1]
H5<-read.csv("HSQ_H.csv")
H5<-H5[,-1]
H6<-read.csv("HUQ_H.csv")
H6<-H6[,-1]
H7<-read.csv("IMQ_H.csv")
H7<-H7[,-1]
H8<-read.csv("MCQ_H.csv")
H8<-H8[,-1]
H9<-read.csv("PAQ_H.csv")
H9<-H9[,-1]
H10<-read.csv("SMQFAM_H.csv")
H10<-H10[,-1]

H1H2<-c()
I<-c()
J<-c()
for (i in 1:dim(H1)[1]) {
if (is.na(match(H1["seqn"][i,1],H2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1["seqn"][i,1],H2["seqn"][,1]))}}
H1H2<-cbind(H1[I,],H2[J,])


H1H2H3<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2)[1]) {
if (is.na(match(H1H2["seqn"][i,1],H3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2["seqn"][i,1],H3["seqn"][,1]))}}
H1H2H3<-cbind(H1H2[I,],H3[J,])


H1H2H3H4<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2H3)[1]) {
if (is.na(match(H1H2H3["seqn"][i,1],H4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2H3["seqn"][i,1],H4[,"seqn"]))}}
H1H2H3H4<-cbind(H1H2H3[I,],H4[J,])


H1H2H3H4H5<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2H3H4)[1]) {
if (is.na(match(H1H2H3H4["seqn"][i,1],H5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2H3H4["seqn"][i,1],H5[,"seqn"]))}}
H1H2H3H4H5<-cbind(H1H2H3H4[I,],H5[J,])

H1H2H3H4H5H6<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2H3H4H5)[1]) {
if (is.na(match(H1H2H3H4H5["seqn"][i,1],H6[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2H3H4H5["seqn"][i,1],H6[,"seqn"]))}}
H1H2H3H4H5H6<-cbind(H1H2H3H4H5[I,],H6[J,])


H1H2H3H4H5H6H7<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2H3H4H5H6)[1]) {
if (is.na(match(H1H2H3H4H5H6["seqn"][i,1],H7[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2H3H4H5H6["seqn"][i,1],H7[,"seqn"]))}}
H1H2H3H4H5H6H7<-cbind(H1H2H3H4H5H6[I,],H7[J,])


H1H2H3H4H5H6H7H8<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2H3H4H5H6H7)[1]) {
if (is.na(match(H1H2H3H4H5H6H7["seqn"][i,1],H8[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2H3H4H5H6H7["seqn"][i,1],H8[,"seqn"]))}}
H1H2H3H4H5H6H7H8<-cbind(H1H2H3H4H5H6H7[I,],H8[J,])


H1H2H3H4H5H6H7H8H9<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2H3H4H5H6H7H8)[1]) {
if (is.na(match(H1H2H3H4H5H6H7H8["seqn"][i,1],H9[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2H3H4H5H6H7H8["seqn"][i,1],H9[,"seqn"]))}}
H1H2H3H4H5H6H7H8H9<-cbind(H1H2H3H4H5H6H7H8[I,],H9[J,])


H1H2H3H4H5H6H7H8H9H10<-c()
I<-c()
J<-c()
for (i in 1:dim(H1H2H3H4H5H6H7H8H9)[1]) {
if (is.na(match(H1H2H3H4H5H6H7H8H9["seqn"][i,1],H10[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(H1H2H3H4H5H6H7H8H9["seqn"][i,1],H10[,"seqn"]))}}
H1H2H3H4H5H6H7H8H9H10<-cbind(H1H2H3H4H5H6H7H8H9[I,],H10[J,])

write.csv(H1H2H3H4H5H6H7H8H9H10,"Merged_Ser_H_Questionnaire.csv")

I1<-read.csv("DBQ_I.csv")
I1<-I1[,-1]
I2<-read.csv("DIQ_I.csv")
I2<-I2[,-1]
I3<-read.csv("FSQ_I.csv")
I3<-I3[,-1]
I4<-read.csv("HIQ_I.csv")
I4<-I4[,-1]
I5<-read.csv("HSQ_I.csv")
I5<-I5[,-1]
I6<-read.csv("HUQ_I.csv")
I6<-I6[,-1]
I7<-read.csv("IMQ_I.csv")
I7<-I7[,-1]
I8<-read.csv("MCQ_I.csv")
I8<-I8[,-1]
I9<-read.csv("PAQ_I.csv")
I9<-I9[,-1]
I10<-read.csv("SMQFAM_I.csv")
I10<-I10[,-1]

I1I2<-c()
I<-c()
J<-c()
for (i in 1:dim(I1)[1]) {
if (is.na(match(I1["seqn"][i,1],I2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1["seqn"][i,1],I2["seqn"][,1]))}}
I1I2<-cbind(I1[I,],I2[J,])


I1I2I3<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2)[1]) {
if (is.na(match(I1I2["seqn"][i,1],I3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2["seqn"][i,1],I3["seqn"][,1]))}}
I1I2I3<-cbind(I1I2[I,],I3[J,])


I1I2I3I4<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2I3)[1]) {
if (is.na(match(I1I2I3["seqn"][i,1],I4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2I3["seqn"][i,1],I4[,"seqn"]))}}
I1I2I3I4<-cbind(I1I2I3[I,],I4[J,])


I1I2I3I4I5<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2I3I4)[1]) {
if (is.na(match(I1I2I3I4["seqn"][i,1],I5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2I3I4["seqn"][i,1],I5[,"seqn"]))}}
I1I2I3I4I5<-cbind(I1I2I3I4[I,],I5[J,])

I1I2I3I4I5I6<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2I3I4I5)[1]) {
if (is.na(match(I1I2I3I4I5["seqn"][i,1],I6[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2I3I4I5["seqn"][i,1],I6[,"seqn"]))}}
I1I2I3I4I5I6<-cbind(I1I2I3I4I5[I,],I6[J,])


I1I2I3I4I5I6I7<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2I3I4I5I6)[1]) {
if (is.na(match(I1I2I3I4I5I6["seqn"][i,1],I7[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2I3I4I5I6["seqn"][i,1],I7[,"seqn"]))}}
I1I2I3I4I5I6I7<-cbind(I1I2I3I4I5I6[I,],I7[J,])


I1I2I3I4I5I6I7I8<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2I3I4I5I6I7)[1]) {
if (is.na(match(I1I2I3I4I5I6I7["seqn"][i,1],I8[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2I3I4I5I6I7["seqn"][i,1],I8[,"seqn"]))}}
I1I2I3I4I5I6I7I8<-cbind(I1I2I3I4I5I6I7[I,],I8[J,])


I1I2I3I4I5I6I7I8I9<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2I3I4I5I6I7I8)[1]) {
if (is.na(match(I1I2I3I4I5I6I7I8["seqn"][i,1],I9[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2I3I4I5I6I7I8["seqn"][i,1],I9[,"seqn"]))}}
I1I2I3I4I5I6I7I8I9<-cbind(I1I2I3I4I5I6I7I8[I,],I9[J,])


I1I2I3I4I5I6I7I8I9I10<-c()
I<-c()
J<-c()
for (i in 1:dim(I1I2I3I4I5I6I7I8I9)[1]) {
if (is.na(match(I1I2I3I4I5I6I7I8I9["seqn"][i,1],I10[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(I1I2I3I4I5I6I7I8I9["seqn"][i,1],I10[,"seqn"]))}}
I1I2I3I4I5I6I7I8I9I10<-cbind(I1I2I3I4I5I6I7I8I9[I,],I10[J,])

write.csv(I1I2I3I4I5I6I7I8I9I10,"Merged_Ser_I_Questionnaire.csv")

J1<-read.csv("DBQ_J.csv")
J1<-J1[,-1]
J2<-read.csv("DIQ_J.csv")
J2<-J2[,-1]
J3<-read.csv("FSQ_J.csv")
J3<-J3[,-1]
J4<-read.csv("HIQ_J.csv")
J4<-J4[,-1]
J5<-read.csv("HSQ_J.csv")
J5<-J5[,-1]
J6<-read.csv("HUQ_J.csv")
J6<-J6[,-1]
J7<-read.csv("IMQ_J.csv")
J7<-J7[,-1]
J8<-read.csv("MCQ_J.csv")
J8<-J8[,-1]
J9<-read.csv("PAQ_J.csv")
J9<-J9[,-1]
J10<-read.csv("SMQFAM_J.csv")
J10<-J10[,-1]

J1J2<-c()
I<-c()
J<-c()
for (i in 1:dim(J1)[1]) {
if (is.na(match(J1["seqn"][i,1],J2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1["seqn"][i,1],J2["seqn"][,1]))}}
J1J2<-cbind(J1[I,],J2[J,])


J1J2J3<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2)[1]) {
if (is.na(match(J1J2["seqn"][i,1],J3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2["seqn"][i,1],J3["seqn"][,1]))}}
J1J2J3<-cbind(J1J2[I,],J3[J,])


J1J2J3J4<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2J3)[1]) {
if (is.na(match(J1J2J3["seqn"][i,1],J4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2J3["seqn"][i,1],J4[,"seqn"]))}}
J1J2J3J4<-cbind(J1J2J3[I,],J4[J,])


J1J2J3J4J5<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2J3J4)[1]) {
if (is.na(match(J1J2J3J4["seqn"][i,1],J5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2J3J4["seqn"][i,1],J5[,"seqn"]))}}
J1J2J3J4J5<-cbind(J1J2J3J4[I,],J5[J,])

J1J2J3J4J5J6<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2J3J4J5)[1]) {
if (is.na(match(J1J2J3J4J5["seqn"][i,1],J6[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2J3J4J5["seqn"][i,1],J6[,"seqn"]))}}
J1J2J3J4J5J6<-cbind(J1J2J3J4J5[I,],J6[J,])


J1J2J3J4J5J6J7<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2J3J4J5J6)[1]) {
if (is.na(match(J1J2J3J4J5J6["seqn"][i,1],J7[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2J3J4J5J6["seqn"][i,1],J7[,"seqn"]))}}
J1J2J3J4J5J6J7<-cbind(J1J2J3J4J5J6[I,],J7[J,])


J1J2J3J4J5J6J7J8<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2J3J4J5J6J7)[1]) {
if (is.na(match(J1J2J3J4J5J6J7["seqn"][i,1],J8[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2J3J4J5J6J7["seqn"][i,1],J8[,"seqn"]))}}
J1J2J3J4J5J6J7J8<-cbind(J1J2J3J4J5J6J7[I,],J8[J,])


J1J2J3J4J5J6J7J8J9<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2J3J4J5J6J7J8)[1]) {
if (is.na(match(J1J2J3J4J5J6J7J8["seqn"][i,1],J9[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2J3J4J5J6J7J8["seqn"][i,1],J9[,"seqn"]))}}
J1J2J3J4J5J6J7J8J9<-cbind(J1J2J3J4J5J6J7J8[I,],J9[J,])


J1J2J3J4J5J6J7J8J9J10<-c()
I<-c()
J<-c()
for (i in 1:dim(J1J2J3J4J5J6J7J8J9)[1]) {
if (is.na(match(J1J2J3J4J5J6J7J8J9["seqn"][i,1],J10[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(J1J2J3J4J5J6J7J8J9["seqn"][i,1],J10[,"seqn"]))}}
J1J2J3J4J5J6J7J8J9J10<-cbind(J1J2J3J4J5J6J7J8J9[I,],J10[J,])

write.csv(J1J2J3J4J5J6J7J8J9J10,"Merged_Ser_J_Questionnaire.csv")

P1<-read.csv("P_DBQ.csv")
P1<-P1[,-1]
P2<-read.csv("P_DIQ.csv")
P2<-P2[,-1]
P3<-read.csv("P_FSQ.csv")
P3<-P3[,-1]
P4<-read.csv("P_HIQ.csv")
P4<-P4[,-1]
P5<-read.csv("P_HSQ.csv")
P5<-P5[,-1]
P6<-read.csv("P_HUQ.csv")
P6<-P6[,-1]
P7<-read.csv("P_IMQ.csv")
P7<-P7[,-1]
P8<-read.csv("P_MCQ.csv")
P8<-P8[,-1]
P9<-read.csv("P_PAQ.csv")
P9<-P9[,-1]
P10<-read.csv("P_SMQFAM.csv")
P10<-P10[,-1]

P1P2<-c()
I<-c()
J<-c()
for (i in 1:dim(P1)[1]) {
if (is.na(match(P1["seqn"][i,1],P2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1["seqn"][i,1],P2["seqn"][,1]))}}
P1P2<-cbind(P1[I,],P2[J,])


P1P2P3<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2)[1]) {
if (is.na(match(P1P2["seqn"][i,1],P3["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2["seqn"][i,1],P3["seqn"][,1]))}}
P1P2P3<-cbind(P1P2[I,],P3[J,])


P1P2P3P4<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2P3)[1]) {
if (is.na(match(P1P2P3["seqn"][i,1],P4[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2P3["seqn"][i,1],P4[,"seqn"]))}}
P1P2P3P4<-cbind(P1P2P3[I,],P4[J,])


P1P2P3P4P5<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2P3P4)[1]) {
if (is.na(match(P1P2P3P4["seqn"][i,1],P5[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2P3P4["seqn"][i,1],P5[,"seqn"]))}}
P1P2P3P4P5<-cbind(P1P2P3P4[I,],P5[J,])

P1P2P3P4P5P6<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2P3P4P5)[1]) {
if (is.na(match(P1P2P3P4P5["seqn"][i,1],P6[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2P3P4P5["seqn"][i,1],P6[,"seqn"]))}}
P1P2P3P4P5P6<-cbind(P1P2P3P4P5[I,],P6[J,])


P1P2P3P4P5P6P7<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2P3P4P5P6)[1]) {
if (is.na(match(P1P2P3P4P5P6["seqn"][i,1],P7[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2P3P4P5P6["seqn"][i,1],P7[,"seqn"]))}}
P1P2P3P4P5P6P7<-cbind(P1P2P3P4P5P6[I,],P7[J,])


P1P2P3P4P5P6P7P8<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2P3P4P5P6P7)[1]) {
if (is.na(match(P1P2P3P4P5P6P7["seqn"][i,1],P8[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2P3P4P5P6P7["seqn"][i,1],P8[,"seqn"]))}}
P1P2P3P4P5P6P7P8<-cbind(P1P2P3P4P5P6P7[I,],P8[J,])


P1P2P3P4P5P6P7P8P9<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2P3P4P5P6P7P8)[1]) {
if (is.na(match(P1P2P3P4P5P6P7P8["seqn"][i,1],P9[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2P3P4P5P6P7P8["seqn"][i,1],P9[,"seqn"]))}}
P1P2P3P4P5P6P7P8P9<-cbind(P1P2P3P4P5P6P7P8[I,],P9[J,])


P1P2P3P4P5P6P7P8P9P10<-c()
I<-c()
J<-c()
for (i in 1:dim(P1P2P3P4P5P6P7P8P9)[1]) {
if (is.na(match(P1P2P3P4P5P6P7P8P9["seqn"][i,1],P10[,"seqn"]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(P1P2P3P4P5P6P7P8P9["seqn"][i,1],P10[,"seqn"]))}}
P1P2P3P4P5P6P7P8P9P10<-cbind(P1P2P3P4P5P6P7P8P9[I,],P10[J,])

write.csv(P1P2P3P4P5P6P7P8P9P10,"Merged_Ser_P_Questionnaire.csv")







Mer2<-function(X1,X2){
X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {   
if (is.na(match(X1["seqn"][i,1],X2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)   
J<-cbind(J,match(X1["seqn"][i,1],X2["seqn"][,1]))}
}
X1X2<-cbind(X1[I,],X2[J,])
return(X1X2)}

C1<-read.csv("Merged_FullSer_C_minBPX_minQuestion.csv")
C1<-C1[,-1]
C2<-read.csv("Merged_Ser_C_Questionnaire.csv")
C2<-C2[,-1]
C1C2<-Mer2(C1,C2)
write.csv(C1C2,"Merged_FullSer_C_minBPX.csv")

D1<-read.csv("Merged_FullSer_D_minBPX_minQuestion.csv")
D1<-D1[,-1]
D2<-read.csv("Merged_Ser_D_Questionnaire.csv")
D2<-D2[,-1]
D1D2<-Mer2(D1,D2)
write.csv(D1D2,"Merged_FullSer_D_minBPX.csv")

E1<-read.csv("Merged_FullSer_E_minBPX_minQuestion.csv")
E1<-E1[,-1]
E2<-read.csv("Merged_Ser_E_Questionnaire.csv")
E2<-E2[,-1]
E1E2<-Mer2(D1,D2)
write.csv(E1E2,"Merged_FullSer_E_minBPX.csv")

F1<-read.csv("Merged_FullSer_F_minBPX_minQuestion.csv")
F1<-F1[,-1]
F2<-read.csv("Merged_Ser_F_Questionnaire.csv")
F2<-F2[,-1]
F1F2<-Mer2(F1,F2)
write.csv(F1F2,"Merged_FullSer_F_minBPX.csv")

G1<-read.csv("Merged_FullSer_G_minBPX_minQuestion.csv")
G1<-G1[,-1]
G2<-read.csv("Merged_Ser_G_Questionnaire.csv")
G2<-G2[,-1]
G1G2<-Mer2(G1,G2)
write.csv(G1G2,"Merged_FullSer_G_minBPX.csv")

H1<-read.csv("Merged_FullSer_H_minBPX_minQuestion.csv")
H1<-H1[,-1]
H2<-read.csv("Merged_Ser_H_Questionnaire.csv")
H2<-H2[,-1]
H1H2<-Mer2(H1,H2)
write.csv(H1H2,"Merged_FullSer_H_minBPX.csv")

I1<-read.csv("Merged_FullSer_I_minBPX_minQuestion.csv")
I1<-I1[,-1]
I2<-read.csv("Merged_Ser_I_Questionnaire.csv")
I2<-I2[,-1]
I1I2<-Mer2(I1,I2)
write.csv(I1I2,"Merged_FullSer_I_minBPX.csv")

J1<-read.csv("Merged_FullSer_J_minBPX_minQuestion.csv")
J1<-J1[,-1]
J2<-read.csv("Merged_Ser_J_Questionnaire.csv")
J2<-J2[,-1]
J1J2<-Mer2(J1,J2)
write.csv(J1J2,"Merged_FullSer_J_minBPX.csv")

P1<-read.csv("Merged_FullSer_P_minBPX_minQuestion.csv")
P1<-P1[,-1]
P2<-read.csv("Merged_Ser_P_Questionnaire.csv")
P2<-P2[,-1]
P1P2<-Mer2(P1,P2)
write.csv(P1P2,"Merged_FullSer_P_minBPX.csv")

C<-read.csv("Merged_FullSer_C_minBPX.csv")
C<-C[,-c(1)]
D<-read.csv("Merged_FullSer_D_minBPX.csv")
D<-D[,-c(1)]
E<-read.csv("Merged_FullSer_E_minBPX.csv")
E<-E[,-c(1)]
F<-read.csv("Merged_FullSer_F_minBPX.csv")
F<-F[,-c(1)]
G<-read.csv("Merged_FullSer_G_minBPX.csv")
G<-G[,-c(1)]
H<-read.csv("Merged_FullSer_H_minBPX.csv")
H<-H[,-c(1)]
I<-read.csv("Merged_FullSer_I_minBPX.csv")
I<-I[,-c(1)]
J<-read.csv("Merged_FullSer_J_minBPX.csv")
J<-J[,-c(1)]
P<-read.csv("Merged_FullSer_P_minBPX.csv")
P<-P[,-c(1)]

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

dim(F_mat)
#[1] 54893   525

matr3<-F_mat
X<-array(rep(0),dim(matr3)[2])
for (i in 1:dim(matr3)[2]) {
X[i]<-sum(is.na(matr3[,i]))
}


XX<-matr3
threshold<-0.3*dim(matr3)[1]
for (i in length(X):1) {
if (X[i]>threshold)
XX<-XX[,-i]
}


XX1<-XX[,-c(20:33,35:43,52:53,130:131,134:135,203,208,241:243,308,373:374,377,381,383,385,390,392,397,398)]

FEST<-XX1[,"lbxhgb"]

XY<-XX1[,-grep("lbxhgb", colnames(XX1))]

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

X<-XX1

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

threshold2<-0.3*dim(matr3)[2]
for (i in length(num2):1) {
if (num2[i]>threshold2) {
XY1<-XY1[-i,]
anemia<-anemia[-i]
FEST<-FEST[-i]}}

dim(XY1)
#[1] 54892   352
length(FEST)
#[1] 54892   So this explains sentence: "The merge of such individual datasets resulted in 54,892 cases."

Xnum<-XY1[,!colnames(X) %in% c("sddsrvyr","ridexmon","ridreth1","sialang","siaproxy","siaintrp","fialang","fiaproxy","fiaintrp","mialang","miaproxy","miaintrp","bmdstats","dr1drstz","drabf","drdint","dr1day","dr1lang","dbq095z","drqsprep","drqsdiet","dr1.300","drd340","drd360","dr2drstz","dr2day","dr2lang","dr2.300","dsd010","dsd010an","dsdcount","dsdancnt","ohx08tc","ohx07tc","ohx06tc","ohx05tc","ohx04tc","ohx03tc","ohx02tc","ohx01tc","ohx09tc","ohx10tc","ohx11tc","ohx12tc","ohx13tc","ohx14tc","ohx15tc","ohx16tc","ohx24tc","ohx23tc","ohx22tc","ohx21tc","ohx20tc","ohx19tc","ohx18tc","ohx17tc","ohx25tc","ohx26tc","ohx27tc","ohx28tc","ohx29tc","ohx30tc","ohx31tc","ohx32tc","diq010","diq050","fsdhh","fsdad","fsd151","hiq210","hsq590","huq010","huq030","huq071","huq090","imq020","mcq010","mcq053","mcq080","mcq092")]

Xfum<-XY1[,colnames(X) %in% c("sddsrvyr","ridexmon","ridreth1","sialang","siaproxy","siaintrp","fialang","fiaproxy","fiaintrp","mialang","miaproxy","miaintrp","bmdstats","dr1drstz","drabf","drdint","dr1day","dr1lang","dbq095z","drqsprep","drqsdiet","dr1.300","drd340","drd360","dr2drstz","dr2day","dr2lang","dr2.300","dsd010","dsd010an","dsdcount","dsdancnt","ohx08tc","ohx07tc","ohx06tc","ohx05tc","ohx04tc","ohx03tc","ohx02tc","ohx01tc","ohx09tc","ohx10tc","ohx11tc","ohx12tc","ohx13tc","ohx14tc","ohx15tc","ohx16tc","ohx24tc","ohx23tc","ohx22tc","ohx21tc","ohx20tc","ohx19tc","ohx18tc","ohx17tc","ohx25tc","ohx26tc","ohx27tc","ohx28tc","ohx29tc","ohx30tc","ohx31tc","ohx32tc","diq010","diq050","fsdhh","fsdad","fsd151","hiq210","hsq590","huq010","huq030","huq071","huq090","imq020","mcq010","mcq053","mcq080","mcq092")]

ff<-array(ncol(Xnum))
ff1<-rep(0,ncol(Xnum))
for (i in 1:ncol(Xnum)) {
ff[i]<-length(unique(Xnum[,i]))
if (ff[i]<2)
ff1[i]<-1}

for (i in ncol(Xnum):1) {
if (ff1[i]==1)
Xnum<-Xnum[,-i]}


ee<-array(ncol(Xfum))
ee1<-rep(0,ncol(Xfum))
for (i in 1:ncol(Xfum)) {
ee[i]<-length(unique(Xfum[,i]))
if (ee[i]<2)
ee1[i]<-1}
for (i in ncol(Xfum):1) {
if (ee1[i]==1)
Xfum<-Xfum[,-i]}

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

dim(Xnum)
#[1] 54892   271

num<-array(rep(0),dim(Xnum)[2])
for (i in dim(Xnum)[2]:1) {
num[i]<- sum(is.na(Xnum[,i]))}

B2<-FEST
#B2<- anemia
F<-Xnum


write.csv(Xnum,"Xnum_Merged_FullSer_C-P_minBPX.csv")
write.csv(FEST,"FEST_Merged_FullSer_C-P_minBPX.csv")
write.csv(anemia,"anemia_Merged_FullSer_C-P_minBPX.csv")

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
for (i in dim(df1)[2]:1) {
if (any(is.na(df1[,i]))==TRUE)
XX<-XX[,-i]}
write.csv(XX,"Xfum_1hotencoded_Merged_FullSer_C-P_minBPX.csv")
XNUM<-as.matrix(XX)
write.csv(XNUM,"Xfum_1hotencoded_Merged_FullSer_C-P_minBPX.csv")
XNUM<-cbind(Xnum,XNUM)
for (i in dim(XNUM)[2]:1) {
if (any(is.na(XNUM[,i]))==TRUE)
XX<-XX[,-i]}
dim(XX)
#[1] 54892   717
write.csv(XX,"Xnum_and_Xfum_1hotencoded_Merged_FullSer_C-P_minBPX.csv")

FEST<-read.csv("FEST_Merged_FullSer_C-P_minBPX.csv",header=TRUE)
FEST<-FEST[,-1]
FEST<-as.vector(t(FEST))
Xnum<-read.csv("Xnum_Merged_FullSer_C-P_minBPX.csv")
Xnum<-Xnum[,-1]
Xnum<-as.matrix(Xnum)

Xnumfum<-read.csv("Xnum_and_Xfum_1hotencoded_Merged_FullSer_C-P_minBPX.csv")
Xnumfum<-Xnumfum[,-1]
A<-Xnumfum[,1:3]
B<-read.csv("FEST_Merged_FullSer_C-P_minBPX.csv")
C<-Xnumfum[,"mcq053_2"]
dim(B)
Fe<-cbind(A,B,C)
Fe<-Fe[,-4]
Fe[1,]
#   seqn riagendr ridageyr    x

#Children 6 - 59 months of age110 or higher
#Children 5 - 11 years of age115 or higher
#Children 12 - 14 years of age120 or higher

anemia2<-c()
J<-c()
for (i in 1:dim(Fe)[1]) {
if ((Fe[i,"ridageyr"]>4)&&(Fe[i,"ridageyr"]<12)) {
J<-cbind(J,i)
if (Fe[i,4]<11.5) {
anemia2<-cbind(anemia2,1)}
else {
anemia2<-cbind(anemia2,0)}}}

length(anemia2)
#[1] 6249
sum(anemia2)
#[1] 208

Xnumfum2<-cbind(A[J,],t(anemia2),t(t(C[J])))

write.csv(Xnumfum2,"Children_aged_5-11.csv")

Ce<-Xnumfum2

for (i in dim(Xnumfum2)[1]:1) {
if (Ce[i,5]==0) {
Ce<-Ce[-i,]}}

Ce<-Ce[,-5]
write.csv(Ce,"Children_aged_5-11_mcq053_2.csv")

A<-Xnumfum[,1:3]
B<-read.csv("FEST_Merged_FullSer_C-P_minBPX.csv")
C<-Xnumfum[,"mcq053_2"]
dim(B)
#[1] 54892     2
Fe<-cbind(A,B,C)
Fe<-Fe[,-4]
anemia2<-c()
J<-c()
for (i in 1:dim(Fe)[1]) {
if ((Fe[i,"ridageyr"]>11)&&(Fe[i,"ridageyr"]<15)) {
J<-cbind(J,i)
if (Fe[i,4]<12) {
anemia2<-cbind(anemia2,1)}
else {
anemia2<-cbind(anemia2,0)}}}

Xnumfum2<-cbind(A[J,],t(anemia2),t(t(C[J])))

dim(Xnumfum2)
#[1] 3210    4
sum(Xnumfum2[,4])
#[1] 160
Xnumfum2[1,]
#   seqn riagendr ridageyr t(anemia2)
#3 21007        2       14          0
write.csv(Xnumfum2,"Children_aged_12-14.csv")

Ce<-Xnumfum2

for (i in dim(Xnumfum2)[1]:1) {
if (Ce[i,5]==0) {
Ce<-Ce[-i,]}}

Ce<-Ce[,-5]
write.csv(Ce,"Children_aged_12-14_mcq053_2.csv")

A<-Xnumfum[,1:3]
B<-read.csv("FEST_Merged_FullSer_C-P_minBPX.csv")
C<-Xnumfum[,"mcq053_2"]
dim(B)
#[1] 54892     2
Fe<-cbind(A,B,C)
Fe<-Fe[,-4]
anemia2<-c()
J<-c()
for (i in 1:dim(Fe)[1]) {
if ((Fe[i,"ridageyr"]>49)&&(Fe[i,"riagendr"]==2)) {
J<-cbind(J,i)
if (Fe[i,4]<12) {
anemia2<-cbind(anemia2,1)}
else {
anemia2<-cbind(anemia2,0)}}}

Xnumfum2<-cbind(A[J,],t(anemia2),t(t(C[J])))

Ce<-Xnumfum2

for (i in dim(Xnumfum2)[1]:1) {
if (Ce[i,5]==0) {
Ce<-Ce[-i,]}}

Ce<-Ce[,-5]
write.csv(Ce,"Female_aged_50_and_above_mcq053_2.csv")

write.csv(Xnumfum2,"Female_aged_50_and_above.csv")

A<-Xnumfum[,1:3]
B<-read.csv("FEST_Merged_FullSer_C-P_minBPX.csv")
C<-Xnumfum[,"mcq053_2"]

Fe<-cbind(A,B,C)
Fe<-Fe[,-4]
anemia2<-c()
J<-c()
for (i in 1:dim(Fe)[1]) {
if ((Fe[i,"ridageyr"]>65)&&(Fe[i,"riagendr"]==1)) {
J<-cbind(J,i)
if (Fe[i,4]<13) {
anemia2<-cbind(anemia2,1)}
else {
anemia2<-cbind(anemia2,0)}}}

Xnumfum2<-cbind(A[J,],t(anemia2),t(t(C[J])))
Ce<-Xnumfum2

for (i in dim(Xnumfum2)[1]:1) {
if (Ce[i,5]==0) {
Ce<-Ce[-i,]}}

Ce<-Ce[,-5]
write.csv(Ce,"Males_aged_66_and_above_mcq053_2.csv")

write.csv(Xnumfum2,"Males_aged_66_and_above.csv")


A<-Xnumfum[,1:3]
B<-read.csv("FEST_Merged_FullSer_C-P_minBPX.csv")
C<-Xnumfum[,"mcq053_2"]

Fe<-cbind(A,B,C)
Fe<-Fe[,-4]
anemia2<-c()
J<-c()
for (i in 1:dim(Fe)[1]) {
if ((Fe[i,"ridageyr"]>14)&&(Fe[i,"ridageyr"]<66)&&(Fe[i,"riagendr"]==1)) {
J<-cbind(J,i)
if (Fe[i,4]<13) {
anemia2<-cbind(anemia2,1)}
else {
anemia2<-cbind(anemia2,0)}}}

Xnumfum2<-cbind(A[J,],t(anemia2),t(t(C[J])))
Ce<-Xnumfum2

for (i in dim(Xnumfum2)[1]:1) {
if (Ce[i,5]==0) {
Ce<-Ce[-i,]}}

Ce<-Ce[,-5]
write.csv(Ce,"Males_aged_15-65_mcq053_2.csv")

write.csv(Xnumfum2,"Males_aged_15-65.csv")


A<-Xnumfum[,1:3]
B<-read.csv("FEST_Merged_FullSer_C-P_minBPX.csv")
C<-Xnumfum[,"mcq053_2"]
dim(B)
#[1] 54892     2
Fe<-cbind(A,B,C)
Fe<-Fe[,-4]
anemia2<-c()
J<-c()
for (i in 1:dim(Fe)[1]) {
if ((Fe[i,"ridageyr"]>14)&&(Fe[i,"ridageyr"]<50)&&(Fe[i,"riagendr"]==2)) {
J<-cbind(J,i)
if (Fe[i,4]<12) {
anemia2<-cbind(anemia2,1)}
else {
anemia2<-cbind(anemia2,0)}}}

Xnumfum2<-cbind(A[J,],t(anemia2),t(t(C[J])))
Ce<-Xnumfum2

for (i in dim(Xnumfum2)[1]:1) {
if (Ce[i,5]==0) {
Ce<-Ce[-i,]}}

Ce<-Ce[,-5]
write.csv(Ce,"Females_aged_15-49_mcq053_2.csv")

write.csv(Xnumfum2,"Females_aged_15-49.csv")

C1<-read.csv("Females_aged_15-49_mcq053_2.csv")
C1<-C1[,-1]

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

C1C2<-c()
I<-c()
J<-c()
for (i in 1:dim(C1)[1]) {
if (is.na(match(C1["seqn"][i,1],C2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1["seqn"][i,1],C2["seqn"][,1]))}}
C1C2<-cbind(C1[I,],C2[J,])
fe<-c()
for (i in 1:nrow(C1C2)) {
if ((is.na(C1C2[i,"urxpreg"])==FALSE)&&(C1C2[i,"urxpreg"]==2)) {
fe<-cbind(fe,i)}}
Fin_file<-C1C2[fe,]
dim(Fin_file)
#[1] 8328    6
Fin_file<-Fin_file[,-ncol(Fin_file)]
Fin_file<-Fin_file[,-ncol(Fin_file)]
Fin_file<-Fin_file[,!colnames(Fin_file) %in% c("riagendr")]
write.csv(Fin_file,"Female_aged_15-49_with_NEG_UrTest_8907_mcq053_2.csv")

C1<-read.csv("Females_aged_15-49.csv")
C1<-C1[,-1]

C1C2<-c()
I<-c()
J<-c()
for (i in 1:dim(C1)[1]) {
if (is.na(match(C1["seqn"][i,1],C2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(C1["seqn"][i,1],C2["seqn"][,1]))}}
C1C2<-cbind(C1[I,],C2[J,])
fe<-c()
for (i in 1:nrow(C1C2)) {
if ((is.na(C1C2[i,"urxpreg"])==FALSE)&&(C1C2[i,"urxpreg"]==2)) {
fe<-cbind(fe,i)}}
Fin_file<-C1C2[fe,]
Fin_file<-Fin_file[,-ncol(Fin_file)]
Fin_file<-Fin_file[,-ncol(Fin_file)]
Fin_file<-Fin_file[,!colnames(Fin_file) %in% c("riagendr")]
dim(Fin_file)
#[1] 8907    3

write.csv(Fin_file,"Female_aged_15-49_with_NEG_UrTest_8907.csv")

