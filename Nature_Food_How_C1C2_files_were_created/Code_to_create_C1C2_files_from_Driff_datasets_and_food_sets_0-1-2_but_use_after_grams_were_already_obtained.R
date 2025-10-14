Mer2<-function(X1,X2){
X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {   
if (is.na(match(X1["Food code"][i,1],X2["Food code"][,1]))==FALSE) {
I<-cbind(I,i)   
J<-cbind(J,match(X1["Food code"][i,1],X2["Food code"][,1]))}
}
X1X2<-cbind(X1[I,],X2[J,])
return(X1X2)}


Teh<-function(X){
df<-X[,2:ncol(X)]
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
X1<-cbind(X[,1],as.matrix(df))
return(X1)} 

C1<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_1_C_mat.csv")
C1<-C1[,-1]
C1<-Teh(C1)
sum(C1[,2:ncol(C1)])
colnames(C1)[1]<-"Food code"

C2<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_2_C_mat.csv")
C2<-C2[,-1]
C2<-Teh(C2)
colnames(C2)[1]<-"Food code"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"Food code"

C1A<-read.csv("GRAM_3099-FSS_Driff_1_C_mat.csv")
C1A<-C1A[,-1]
C1A<-Teh(C1A)
sum(C1A[,2:ncol(C1A)])
colnames(C1A)[1]<-"Food code"

C2A<-read.csv("GRAM_3099-FSS_Driff_2_C_mat.csv")
C2A<-C2A[,-1]
C2A<-Teh(C2A)
colnames(C2A)[1]<-"Food code"

C1C2A<-Mer2(C1A,C2A)

C3A<-cbind(C1C2A[,1],(C1C2A[,2:ncol(C1A)]+C1C2A[,(2+ncol(C1A)):ncol(C1C2A)]))
colnames(C3A)[1]<-"Food code"

write.csv(C3A,"0-1-2_3099-FSS_Driff_both_days_C_mat.csv")

C3_3outof9_plus_all9<-cbind(C3,C3A[,2:ncol(C3A)])

write.csv(C3_3outof9_plus_all9,"0-1-2_9450-FSS_Driff_both_days_C_mat.csv")


D1<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_1_D_mat.csv")
D1<-D1[,-1]
D1<-Teh(D1)
sum(D1[,2:ncol(D1)])
colnames(D1)[1]<-"Food code"

D2<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_2_D_mat.csv")
D2<-D2[,-1]
D2<-Teh(D2)
colnames(D2)[1]<-"Food code"

D1D2<-Mer2(D1,D2)

D3<-cbind(D1D2[,1],(D1D2[,2:ncol(D1)]+D1D2[,(2+ncol(D1)):ncol(D1D2)]))
colnames(D3)[1]<-"Food code"

D1A<-read.csv("GRAM_3099-FSS_Driff_1_D_mat.csv")
D1A<-D1A[,-1]
D1A<-Teh(D1A)
sum(D1A[,2:ncol(D1A)])
colnames(D1A)[1]<-"Food code"

D2A<-read.csv("GRAM_3099-FSS_Driff_2_D_mat.csv")
D2A<-D2A[,-1]
D2A<-Teh(D2A)
colnames(D2A)[1]<-"Food code"

D1D2A<-Mer2(D1A,D2A)

D3A<-cbind(D1D2A[,1],(D1D2A[,2:ncol(D1A)]+D1D2A[,(2+ncol(D1A)):ncol(D1D2A)]))
colnames(D3A)[1]<-"Food code"

write.csv(D3A,"0-1-2_3099-FSS_Driff_both_days_D_mat.csv")

D3_3outof9_plus_all9<-cbind(D3,D3A[,2:ncol(D3A)])

write.csv(D3_3outof9_plus_all9,"0-1-2_9450-FSS_Driff_both_days_D_mat.csv"



E1<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_1_E_mat.csv")
E1<-E1[,-1]
E1<-Teh(E1)
sum(E1[,2:ncol(E1)])
colnames(E1)[1]<-"Food code"

E2<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_2_E_mat.csv")
E2<-E2[,-1]
E2<-Teh(E2)
colnames(E2)[1]<-"Food code"

E1E2<-Mer2(E1,E2)

E3<-cbind(E1E2[,1],(E1E2[,2:ncol(E1)]+E1E2[,(2+ncol(E1)):ncol(E1E2)]))
colnames(E3)[1]<-"Food code"

E1A<-read.csv("GRAM_3099-FSS_Driff_1_E_mat.csv")
E1A<-E1A[,-1]
E1A<-Teh(E1A)
sum(E1A[,2:ncol(E1A)])
colnames(E1A)[1]<-"Food code"

E2A<-read.csv("GRAM_3099-FSS_Driff_2_E_mat.csv")
E2A<-E2A[,-1]
E2A<-Teh(E2A)
colnames(E2A)[1]<-"Food code"

E1E2A<-Mer2(E1A,E2A)

E3A<-cbind(E1E2A[,1],(E1E2A[,2:ncol(E1A)]+E1E2A[,(2+ncol(E1A)):ncol(E1E2A)]))
colnames(E3A)[1]<-"Food code"

write.csv(E3A,"0-1-2_3099-FSS_Driff_both_days_E_mat.csv")

E3_3outof9_plus_all9<-cbind(E3,E3A[,2:ncol(E3A)])

write.csv(E3_3outof9_plus_all9,"0-1-2_9450-FSS_Driff_both_days_E_mat.csv"



F1<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_1_F_mat.csv")
F1<-F1[,-1]
F1<-Teh(F1)
sum(F1[,2:ncol(F1)])
colnames(F1)[1]<-"Food code"

F2<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_2_F_mat.csv")
F2<-F2[,-1]
F2<-Teh(F2)
colnames(F2)[1]<-"Food code"

F1F2<-Mer2(F1,F2)

F3<-cbind(F1F2[,1],(F1F2[,2:ncol(F1)]+F1F2[,(2+ncol(F1)):ncol(F1F2)]))
colnames(F3)[1]<-"Food code"

F1A<-read.csv("GRAM_3099-FSS_Driff_1_F_mat.csv")
F1A<-F1A[,-1]
F1A<-Teh(F1A)
sum(F1A[,2:ncol(F1A)])
colnames(F1A)[1]<-"Food code"

F2A<-read.csv("GRAM_3099-FSS_Driff_2_F_mat.csv")
F2A<-F2A[,-1]
F2A<-Teh(F2A)
colnames(F2A)[1]<-"Food code"

F1F2A<-Mer2(F1A,F2A)

F3A<-cbind(F1F2A[,1],(F1F2A[,2:ncol(F1A)]+F1F2A[,(2+ncol(F1A)):ncol(F1F2A)]))
colnames(F3A)[1]<-"Food code"

write.csv(F3A,"0-1-2_3099-FSS_Driff_both_days_F_mat.csv")

F3_3outof9_plus_all9<-cbind(F3,F3A[,2:ncol(F3A)])

write.csv(F3_3outof9_plus_all9,"0-1-2_9450-FSS_Driff_both_days_F_mat.csv"



G1<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_1_G_mat.csv")
G1<-G1[,-1]
G1<-Teh(G1)
sum(G1[,2:ncol(G1)])
colnames(G1)[1]<-"Food code"

G2<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_2_G_mat.csv")
G2<-G2[,-1]
G2<-Teh(G2)
colnames(G2)[1]<-"Food code"

G1G2<-Mer2(G1,G2)

G3<-cbind(G1G2[,1],(G1G2[,2:ncol(G1)]+G1G2[,(2+ncol(G1)):ncol(G1G2)]))
colnames(G3)[1]<-"Food code"

G1A<-read.csv("GRAM_3099-FSS_Driff_1_G_mat.csv")
G1A<-G1A[,-1]
G1A<-Teh(G1A)
sum(G1A[,2:ncol(G1A)])
colnames(G1A)[1]<-"Food code"

G2A<-read.csv("GRAM_3099-FSS_Driff_2_G_mat.csv")
G2A<-G2A[,-1]
G2A<-Teh(G2A)
colnames(G2A)[1]<-"Food code"

G1G2A<-Mer2(G1A,G2A)

G3A<-cbind(G1G2A[,1],(G1G2A[,2:ncol(G1A)]+G1G2A[,(2+ncol(G1A)):ncol(G1G2A)]))
colnames(G3A)[1]<-"Food code"

write.csv(G3A,"0-1-2_3099-FSS_Driff_both_days_G_mat.csv")

G3_3outof9_plus_all9<-cbind(G3,G3A[,2:ncol(G3A)])

write.csv(G3_3outof9_plus_all9,"0-1-2_9450-FSS_Driff_both_days_G_mat.csv"


H1<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_1_H_mat.csv")
H1<-H1[,-1]
H1<-Teh(H1)
sum(H1[,2:ncol(H1)])
colnames(H1)[1]<-"Food code"

H2<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_2_H_mat.csv")
H2<-H2[,-1]
H2<-Teh(H2)
colnames(H2)[1]<-"Food code"

H1H2<-Mer2(H1,H2)

H3<-cbind(H1H2[,1],(H1H2[,2:ncol(H1)]+H1H2[,(2+ncol(H1)):ncol(H1H2)]))
colnames(H3)[1]<-"Food code"

H1A<-read.csv("GRAM_3099-FSS_Driff_1_H_mat.csv")
H1A<-H1A[,-1]
H1A<-Teh(H1A)
sum(H1A[,2:ncol(H1A)])
colnames(H1A)[1]<-"Food code"

H2A<-read.csv("GRAM_3099-FSS_Driff_2_H_mat.csv")
H2A<-H2A[,-1]
H2A<-Teh(H2A)
colnames(H2A)[1]<-"Food code"

H1H2A<-Mer2(H1A,H2A)

H3A<-cbind(H1H2A[,1],(H1H2A[,2:ncol(H1A)]+H1H2A[,(2+ncol(H1A)):ncol(H1H2A)]))
colnames(H3A)[1]<-"Food code"

write.csv(H3A,"0-1-2_3099-FSS_Driff_both_days_H_mat.csv")

H3_3outof9_plus_all9<-cbind(H3,H3A[,2:ncol(H3A)])

write.csv(H3_3outof9_plus_all9,"0-1-2_9450-FSS_Driff_both_days_H_mat.csv"








I1<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_1_I_mat.csv")
I1<-I1[,-1]
I1<-Teh(I1)
sum(I1[,2:ncol(I1)])
colnames(I1)[1]<-"Food code"

I2<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_2_I_mat.csv")
I2<-I2[,-1]
I2<-Teh(I2)
colnames(I2)[1]<-"Food code"

I1I2<-Mer2(I1,I2)

I3<-cbind(I1I2[,1],(I1I2[,2:ncol(I1)]+I1I2[,(2+ncol(I1)):ncol(I1I2)]))
colnames(I3)[1]<-"Food code"

I1A<-read.csv("GRAM_3099-FSS_Driff_1_I_mat.csv")
I1A<-I1A[,-1]
I1A<-Teh(I1A)
sum(I1A[,2:ncol(I1A)])
colnames(I1A)[1]<-"Food code"

I2A<-read.csv("GRAM_3099-FSS_Driff_2_I_mat.csv")
I2A<-I2A[,-1]
I2A<-Teh(I2A)
colnames(I2A)[1]<-"Food code"

I1I2A<-Mer2(I1A,I2A)

I3A<-cbind(I1I2A[,1],(I1I2A[,2:ncol(I1A)]+I1I2A[,(2+ncol(I1A)):ncol(I1I2A)]))
colnames(I3A)[1]<-"Food code"

write.csv(I3A,"0-1-2_3099-FSS_Driff_both_days_I_mat.csv")

I3_3outof9_plus_all9<-cbind(I3,I3A[,2:ncol(I3A)])

write.csv(I3_3outof9_plus_all9,"0-1-2_9450-FSS_Driff_both_days_I_mat.csv"





J1<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_1_J_mat.csv")
J1<-J1[,-1]
J1<-Teh(J1)
sum(J1[,2:ncol(J1)])
colnames(J1)[1]<-"Food code"

J2<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_2_J_mat.csv")
J2<-J2[,-1]
J2<-Teh(J2)
colnames(J2)[1]<-"Food code"

J1J2<-Mer2(J1,J2)

J3<-cbind(J1J2[,1],(J1J2[,2:ncol(J1)]+J1J2[,(2+ncol(J1)):ncol(J1J2)]))
colnames(J3)[1]<-"Food code"

J1A<-read.csv("GRAM_3099-FSS_Driff_1_J_mat.csv")
J1A<-J1A[,-1]
J1A<-Teh(J1A)
sum(J1A[,2:ncol(J1A)])
colnames(J1A)[1]<-"Food code"

J2A<-read.csv("GRAM_3099-FSS_Driff_2_J_mat.csv")
J2A<-J2A[,-1]
J2A<-Teh(J2A)
colnames(J2A)[1]<-"Food code"

J1J2A<-Mer2(J1A,J2A)

J3A<-cbind(J1J2A[,1],(J1J2A[,2:ncol(J1A)]+J1J2A[,(2+ncol(J1A)):ncol(J1J2A)]))
colnames(J3A)[1]<-"Food code"

write.csv(J3A,"0-1-2_3099-FSS_Driff_both_days_J_mat.csv")

J3_3outof9_plus_all9<-cbind(J3,J3A[,2:ncol(J3A)])

write.csv(J3_3outof9_plus_all9,"0-1-2_9450-FSS_Driff_both_days_J_mat.csv"






P1<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_1_P_mat.csv")
P1<-P1[,-1]
P1<-Teh(P1)
sum(P1[,2:ncol(P1)])
colnames(P1)[1]<-"Food code"

P2<-read.csv("GRAM_Added_3_outof_9_NHACONS_Driff_2_P_mat.csv")
P2<-P2[,-1]
P2<-Teh(P2)
colnames(P2)[1]<-"Food code"

P1P2<-Mer2(P1,P2)

P3<-cbind(P1P2[,1],(P1P2[,2:ncol(P1)]+P1P2[,(2+ncol(P1)):ncol(P1P2)]))
colnames(P3)[1]<-"Food code"

P1A<-read.csv("GRAM_3099-FSS_Driff_1_P_mat.csv")
P1A<-P1A[,-1]
P1A<-Teh(P1A)
sum(P1A[,2:ncol(P1A)])
colnames(P1A)[1]<-"Food code"

P2A<-read.csv("GRAM_3099-FSS_Driff_2_P_mat.csv")
P2A<-P2A[,-1]
P2A<-Teh(P2A)
colnames(P2A)[1]<-"Food code"

P1P2A<-Mer2(P1A,P2A)

P3A<-cbind(P1P2A[,1],(P1P2A[,2:ncol(P1A)]+P1P2A[,(2+ncol(P1A)):ncol(P1P2A)]))
colnames(P3A)[1]<-"Food code"

write.csv(P3A,"0-1-2_3099-FSS_Driff_both_days_P_mat.csv")

P3_3outof9_plus_all9<-cbind(P3,P3A[,2:ncol(P3A)])

write.csv(P3_3outof9_plus_all9,"0-1-2_9450-FSS_Driff_both_days_P_mat.csv"


Merge<-rbind(C3A,D3A,E3A,F3A,G3A,H3A,I3A,J3A,P3A)

write.csv(Merge,"Merged_0-1-2_3099-FSS_Driff_both_days_C-P.csv")

Merge<-rbind(C3_3outof9_plus_all9,D3_3outof9_plus_all9,E3_3outof9_plus_all9,F3_3outof9_plus_all9,G3_3outof9_plus_all9,H3_3outof9_plus_all9,I3_3outof9_plus_all9,J3_3outof9_plus_all9,P3_3outof9_plus_all9)

write.csv(Merge,"Merged_0-1-2_9450-FSS_Driff_both_days_C-P.csv") # This creates merged Data-9450 in 0-1-2 for both interview days for all series C-P. 








### Finally how C1C2 files are created for 0-1-2###


A<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907.csv") #From Datasets!

A<-A[,-1]

B<-read.csv("Merged_0-1-2_3099-FSS_Driff_both_days_C-P.csv")

B<-B[,-1]

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

C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F1_03.csv") # The same should be done with PTFA=2 (only non-anemia cases), males, g/kg and No. days. And the similar procedure was repeated for F50+!


A<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907_mcq053_2.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F1_04.csv")


A<-read.csv("Female_aged_50_and_above.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F5_03.csv")



A<-read.csv("Female_aged_50_and_above_mcq053_2.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F5_04.csv")


















A<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907.csv") #From Datasets!

A<-A[,-1]

B<-read.csv("Merged_0-1-2_9450-FSS_Driff_both_days_C-P.csv")

B<-B[,-1]

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

C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F1_09.csv") # The same should be done with PTFA=2 (only non-anemia cases), males, g/kg and No. days. And the similar procedure was repeated for F50+!


A<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907_mcq053_2.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F1_10.csv")


A<-read.csv("Female_aged_50_and_above.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F5_09.csv")



A<-read.csv("Female_aged_50_and_above_mcq053_2.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F5_10.csv")




