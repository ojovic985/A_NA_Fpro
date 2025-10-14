C1C2<-read.csv("C1C2_of_Merge9451_with_8907F.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F1_gram_8907.csv")

B<-read.csv("ML_on_F1_gram_8907.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_07_1098_cases.csv") # This produces data ready for ML runs containing food code, 62 nutrient levels (as independent variables and score as dependent (target) variable) for F15-49. The same was done for grams of food/kg weight of participant, No of days, and then for PTFA=2 (instead PTFA=1), and for F50+.

Disjunkcija2<-function(X1,X2){

X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {
if (is.na(match(X1["Food code"][i,1],X2["Food code"][,1]))==TRUE) {
I<-cbind(I,i)}}
X1X2<-cbind(X1[I,])
return(X1X2)}

G<-Disjunkcija2(Xe,B)
dim(G)
#[1] 8150   66

G1<-G[,-c(64:66)]

write.csv(G1,"G1_9450v_ML_on_F1_07_8352_cases.csv") # This produces extrapolation set for later food prediction

F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_07_NO_0_val_on_No.Anem_1086_cases.csv")

G1<-Disjunkcija2(Xe,F1)
dim(G1)

write.csv(G1,"G1_9450v_ML_on_F1_07_NO_0_val_on_No.Anem_8364_cases.csv")















C1C2<-read.csv("C1C2_of_Merge9451_with_8328F.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F1_gram_8328.csv")

B<-read.csv("ML_on_F1_gram_8328.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_08_1041_cases.csv") # This produces data ready for ML runs containing food code, 62 nutrient levels (as independent variables and score as dependent (target) variable) for F15-49. The same was done for grams of food/kg weight of participant, No of days, and then for PTFA=2 (instead PTFA=1), and for F50+.

Disjunkcija2<-function(X1,X2){

X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {
if (is.na(match(X1["Food code"][i,1],X2["Food code"][,1]))==TRUE) {
I<-cbind(I,i)}}
X1X2<-cbind(X1[I,])
return(X1X2)}

G<-Disjunkcija2(Xe,B)
dim(G)
#[1] 8150   66

G1<-G[,-c(64:66)]

write.csv(G1,"G1_9450v_ML_on_F1_08_8409_cases.csv") # This produces extrapolation set for later food prediction


F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_08_NO_0_val_on_No.Anem_1022_cases.csv")

G1<-Disjunkcija2(Xe,F1)
dim(G1)

write.csv(G1,"G1_9450v_ML_on_F1_08_NO_0_val_on_No.Anem_8428_cases.csv")











C1C2<-read.csv("C1C2_Xnumfum2_repF_with_NEG_UrTest_seqn_age_Anemia_aged_15-49_8907_plus_Merged_NHACONS_3outof9_plus_all9_9451_foods_Driffs_C-P_0-1-2.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F1_0-1-2_8907.csv")

B<-read.csv("ML_on_F1_0-1-2_8907.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_09_1098_cases.csv") # This produces data ready for ML runs containing food code, 62 nutrient levels (as independent variables and score as dependent (target) variable) for F15-49. The same was done for grams of food/kg weight of participant, No of days, and then for PTFA=2 (instead PTFA=1), and for F50+.



F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_09_NO_0_val_on_No.Anem_1086_cases.csv")

















C1C2<-read.csv("C1C2_Xnumfum2_repF_with_NEG_UrTest_seqn_age_NEG_mcq053_Anemia_aged_15-49_plus_Merged_NHACONS_3outof9_plus_all9_9451_foods_Driffs_C-P_0-1-2.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F1_0-1-2_8352.csv")

B<-read.csv("ML_on_F1_0-1-2_8352.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_10_1041_cases.csv") 



F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_10_NO_0_val_on_No.Anem_1022_cases.csv")
















C1C2<-read.csv("C1C2_of_Merge9450_grams_per_kg_with_8907F.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F1_grams_per_kg_8907.csv")

B<-read.csv("ML_on_F1_grams_per_kg_8907.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_11_1098_cases.csv") 



F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_11_NO_0_val_on_No.Anem_1086_cases.csv")














C1C2<-read.csv("C1C2_of_Merge9450_grams_per_kg_with_8328F.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F1_grams_per_kg_8328.csv")

B<-read.csv("ML_on_F1_grams_per_kg_8328.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_12_1041_cases.csv") 



F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F1_12_NO_0_val_on_No.Anem_1022_cases.csv")















C1C2<-read.csv("C1C2_Female_aged_50_and_above_plus_Merged_NHACONS_3outof9_plus_all9_9451_foods_Driffs_C-P_simple_GRAMS_sums.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F5_gram_9928.csv")

B<-read.csv("ML_on_F5_gram_9928.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_07_1300_cases.csv") # This produces data ready for ML runs containing food code, 62 nutrient levels (as independent variables and score as dependent (target) variable) for F15-49. The same was done for grams of food/kg weight of participant, No of days, and then for PTFA=2 (instead PTFA=1), and for F50+.

Disjunkcija2<-function(X1,X2){

X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {
if (is.na(match(X1["Food code"][i,1],X2["Food code"][,1]))==TRUE) {
I<-cbind(I,i)}}
X1X2<-cbind(X1[I,])
return(X1X2)}

G<-Disjunkcija2(Xe,B)
dim(G)
#[1] 8150   66

G1<-G[,-c(64:66)]

write.csv(G1,"G1_9450v_ML_on_F5_07_8150_cases.csv") # This produces extrapolation set for later food prediction


F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_07_NO_0_val_on_No.Anem_1277_cases.csv")

G1<-Disjunkcija2(Xe,F1)
dim(G1)

write.csv(G1,"G1_9450v_ML_on_F5_07_NO_0_val_on_No.Anem_8173_cases.csv")
















C1C2<-read.csv("C1C2_Female_aged_50_and_above_mcq053_2_plus_Merged_NHACONS_3outof9_plus_all9_9451_foods_Driffs_C-P_simple_GRAMS_sums.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F5_gram_9293.csv")

B<-read.csv("ML_on_F5_gram_9293.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_08_1240_cases.csv") # This produces data ready for ML runs containing food code, 62 nutrient levels (as independent variables and score as dependent (target) variable) for F15-49. The same was done for grams of food/kg weight of participant, No of days, and then for PTFA=2 (instead PTFA=1), and for F50+.

Disjunkcija2<-function(X1,X2){

X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {
if (is.na(match(X1["Food code"][i,1],X2["Food code"][,1]))==TRUE) {
I<-cbind(I,i)}}
X1X2<-cbind(X1[I,])
return(X1X2)}

G<-Disjunkcija2(Xe,B)
dim(G)
#[1] 8150   66

G1<-G[,-c(64:66)]

write.csv(G1,"G1_9450v_ML_on_F5_08_8210_cases.csv") # This produces extrapolation set for later food prediction


F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_08_NO_0_val_on_No.Anem_1205_cases.csv")

G1<-Disjunkcija2(Xe,F1)
dim(G1)

write.csv(G1,"G1_9450v_ML_on_F5_08_NO_0_val_on_No.Anem_8245_cases.csv")
















C1C2<-read.csv("C1C2_Female_aged_50_and_above_plus_Merged_NHACONS_3outof9_plus_all9_9451_foods_Driffs_C-P_0-1-2.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F5_0-1-2_9928.csv")

B<-read.csv("ML_on_F5_0-1-2_9928.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_09_1300_cases.csv") # This produces data ready for ML runs containing food code, 62 nutrient levels (as independent variables and score as dependent (target) variable) for F15-49. The same was done for grams of food/kg weight of participant, No of days, and then for PTFA=2 (instead PTFA=1), and for F50+.


F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_09_NO_0_val_on_No.Anem_1277_cases.csv")















C1C2<-read.csv("C1C2_Female_aged_50_and_above_mcq053_2_plus_Merged_NHACONS_3outof9_plus_all9_9451_foods_Driffs_C-P_0-1-2.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F5_0-1-2_9293.csv")

B<-read.csv("ML_on_F5_0-1-2_9293.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_10_1240_cases.csv") # This produces data ready for ML runs containing food code, 62 nutrient levels (as independent variables and score as dependent (target) variable) for F15-49. The same was done for grams of food/kg weight of participant, No of days, and then for PTFA=2 (instead PTFA=1), and for F50+.


F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_10_NO_0_val_on_No.Anem_1205_cases.csv")




















C1C2<-read.csv("C1C2_Female_aged_50_and_above_plus_ALLE_GRAMS_per_KG_Merged_NHACONS_Driffs_C-P_9450_foods.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F5_grams_per_kg_9928.csv")

B<-read.csv("ML_on_F5_grams_per_kg_9928.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_11_1300_cases.csv") # This produces data ready for ML runs containing food code, 62 nutrient levels (as independent variables and score as dependent (target) variable) for F15-49. The same was done for grams of food/kg weight of participant, No of days, and then for PTFA=2 (instead PTFA=1), and for F50+.


F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_11_NO_0_val_on_No.Anem_1277_cases.csv")


















C1C2<-read.csv("C1C2_Female_aged_50_and_above_mcq053_2_plus_ALLE_GRAMS_per_KG_Merged_NHACONS_Driffs_C-P_9450_foods.csv",header=TRUE)
C1C2<-C1C2[,-1]

zu<-c()
Th<-C1C2[,4:ncol(C1C2)]
He<-colSums(Th)
df<-Th
isnum <- sapply(df, is.numeric)
df[,isnum] <- as.data.frame(ifelse(df[,isnum] > 0 | df[,isnum] < 0, 1, 0))
He2<-colSums(df)
for (i in 1:ncol(Th)) {
if (He2[i]<30)
zu<-cbind(zu,i)
}
Mat<-Th[,-zu]
Mat1<-cbind(C1C2[,1:3],Mat)
Mat2<-Mat1[order(Mat1[,3]),]

NonAnem<-Mat2[1:(which.max(Mat2[,3])-1),]
Anem<-Mat2[which.max(Mat2[,3]):nrow(Mat2),]

p_ter<-c()
count_Anem<-c()
count_NonAnem<-c()
brA<-0
brNA<-0
for (i in 4:ncol(Anem)) {
if (mean(NonAnem[,i])>mean(Anem[,i])) {
p_ter<-cbind(p_ter,-log((t.test(NonAnem[,i],Anem[,i],alternative="greater")[3]$p.value))+log(0.5))
brNA<-brNA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
else {
p_ter<-cbind(p_ter,log((t.test(NonAnem[,i],Anem[,i],alternative="less")[3]$p.value))-log(0.5))
brA<-brA+1
count_NonAnem<-cbind(count_NonAnem,sum(NonAnem[,i]))
count_Anem<-cbind(count_Anem,sum(Anem[,i]))}
}

Rez<-cbind(t(count_Anem),t(count_NonAnem),t(count_Anem)/dim(Anem)[1],t(count_NonAnem)/dim(NonAnem)[1],(t(count_NonAnem)/dim(NonAnem)[1])/(t(count_Anem)/dim(Anem)[1]),(t(p_ter)))
colnames(Rez)=c("No.Anem","No.NonAnem","Mean.Anem","Mean.NonAnem","Mean(NonAnem/Anem)","score") 

ter_Fcod_2<-gsub("X","",colnames(Anem[4:ncol(Anem)]))
rownames(Rez)<-t(ter_Fcod_2)

E<-as.numeric(rownames(Rez))
for (j in 1:1) {
if (any(is.na(E))==TRUE) {

E[match(NA,E)]<-94000000}}

rownames(Rez)<-E

write.csv(Rez,"ML_on_F5_grams_per_kg_9293.csv")

B<-read.csv("ML_on_F5_grams_per_kg_9293.csv",header=TRUE)
colnames(B)[1]<-"Food code"

A<-read.csv("Data_9450_food_codes_g_per_100g.csv")
A<-A[,-1]
Xe<-cbind(A[,ncol(A)],A[,-ncol(A)])
colnames(Xe)[1]<-"Food code"

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

F<-Mer2(Xe,B)
F1<-F[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_12_1240_cases.csv") # This produces data ready for ML runs containing food code, 62 nutrient levels (as independent variables and score as dependent (target) variable) for F15-49. The same was done for grams of food/kg weight of participant, No of days, and then for PTFA=2 (instead PTFA=1), and for F50+.


F<-Mer2(Xe,B)
te<-c()
for (i in 1:nrow(F)) {
if (F[i,match("No.Anem",colnames(F))]==0)
te<-cbind(te,i)
}
F1<-F[-te,]

F1<-F1[,-c(64:72)]

write.csv(F1,"9450v_ML_on_F5_12_NO_0_val_on_No.Anem_1205_cases.csv")





