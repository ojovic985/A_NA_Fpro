

## First interview day of 3099 data##

C4<-read.csv("DR1IFF_C.csv") #the same must be repeated with "DR1IFF_D.csv", "DR1IFF_E.csv", "DR1IFF_F.csv", "DR1IFF_G.csv","DR1IFF_H.csv","DR1IFF_I.csv","DR1IFF_J.csv" "DR1IFF_P.csv"
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_1_C_mat.csv")  # This creates participant vs food codes for serie C, first interview day of 3099 food size set. The same must be repeated with D-P



## Second interview day of 3099 data##

C4<-read.csv("DR2IFF_C.csv") #the same must be repeated with "DR2IFF_D.csv", "DR2IFF_E.csv", "DR2IFF_F.csv", "DR2IFF_G.csv","DR2IFF_H.csv","DR2IFF_I.csv","DR2IFF_J.csv" "DR2IFF_P.csv"
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_2_C_mat.csv")  # This creates participant vs food codes for serie C, second interview day of 3099 food size set. The same must be repeated with D-P




##First interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR1IFF_C.csv") #the same must be repeated with "DR1IFF_D.csv", "DR1IFF_E.csv", "DR1IFF_F.csv", "DR1IFF_G.csv","DR1IFF_H.csv","DR1IFF_I.csv","DR1IFF_J.csv" "DR1IFF_P.csv"
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

#Alta<-read.csv("3099_NOVA1-4_NHANES_2003-2020_consistent_food_codes.csv",header=TRUE) # for creating 3099-data foods
#Alta<-Alta[,-1]

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_C_mat.csv") # This creates participant vs food codes for serie C, first interview day of 6351 foods repeated 3 out of 9 times. The same must be repeated with D-P



##Second interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR2IFF_C.csv") #the same must be repeated with "DR2IFF_D.csv", "DR2IFF_E.csv", "DR2IFF_F.csv", "DR2IFF_G.csv","DR2IFF_H.csv","DR2IFF_I.csv","DR2IFF_J.csv" "DR2IFF_P.csv"

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}}

colnames(Fr)[1]<-"seqn"
Fr<-cbind(Tur[,1],Fr)

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_C_mat.csv") # This creates participant vs food codes for serie C, second interview day of 6351 foods repeated 3 out of 9 times. The same must be repeated with D-P


##Summing first and second interview days##

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


C1<-read.csv("GRAM_per_kg_3099-FSS_Driff_1_C_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_3099-FSS_Driff_2_C_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

write.csv("GRAM_per_kg_3099-FSS_Driff_both_days_C_mat.csv") # This creates Data-3099 in GRAM_per_kgs for both interview days, but only for Serie C, this must be repeated for D-P


C1<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_C_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_C_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

C3A<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_C_mat.csv")
C3A<-C3A[,-1]

C3_3outof9_plus_all9<-cbind(C3,C3A[,2:ncol(C3A)])
write.csv(C3_3outof9_plus_all9,"GRAM_per_kg_9450-FSS_Driff_both_days_C_mat.csv") # This creates Data-9450 in grams for both interview days, but only for Serie C, this must be repeated for D-P






























C4<-read.csv("DR1IFF_D.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_1_D_mat.csv")



## Second interview day of 3099 data##

C4<-read.csv("DR2IFF_D.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_2_D_mat.csv")


##First interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR1IFF_D.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

#Alta<-read.csv("3099_NOVA1-4_NHANES_2003-2020_consistent_food_codes.csv",header=TRUE) # for creating 3099-data foods
#Alta<-Alta[,-1]

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_D_mat.csv")



##Second interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR2IFF_D.csv")

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}}

colnames(Fr)[1]<-"seqn"
Fr<-cbind(Tur[,1],Fr)

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_D_mat.csv")


##Summing first and second interview days##

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


C1<-read.csv("GRAM_per_kg_3099-FSS_Driff_1_D_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_3099-FSS_Driff_2_D_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

write.csv("GRAM_per_kg_3099-FSS_Driff_both_days_D_mat.csv")


C1<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_D_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_D_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

C3A<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_D_mat.csv")
C3A<-C3A[,-1]

C3_3outof9_plus_all9<-cbind(C3,C3A[,2:ncol(C3A)])
write.csv(C3_3outof9_plus_all9,"GRAM_per_kg_9450-FSS_Driff_both_days_D_mat.csv")































C4<-read.csv("DR1IFF_E.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_1_E_mat.csv")  



## Second interview day of 3099 data##

C4<-read.csv("DR2IFF_E.csv") 
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_2_E_mat.csv")  # This creates participant vs food codes for serie E, second interview day of 3099 food size set.



##First interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR1IFF_E.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

#Alta<-read.csv("3099_NOVA1-4_NHANES_2003-2020_consistent_food_codes.csv",header=TRUE) # for creating 3099-data foods
#Alta<-Alta[,-1]

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_E_mat.csv") # This creates participant vs food codes for serie C, first interview day of 6351 foods repeated 3 out of 9 times.


##Second interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR2IFF_E.csv")

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}}

colnames(Fr)[1]<-"seqn"
Fr<-cbind(Tur[,1],Fr)

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_E_mat.csv") # This creates participant vs food codes for serie C, second interview day of 6351 foods repeated 3 out of 9 times.


##Summing first and second interview days##

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


C1<-read.csv("GRAM_per_kg_3099-FSS_Driff_1_E_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_3099-FSS_Driff_2_E_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

write.csv("GRAM_per_kg_3099-FSS_Driff_both_days_E_mat.csv") # This creates Data-3099 in grams for both interview days, but only for Serie E


C1<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_E_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_E_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

C3A<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_E_mat.csv")
C3A<-C3A[,-1]

C3_3outof9_plus_all9<-cbind(C3,C3A[,2:ncol(C3A)])
write.csv(C3_3outof9_plus_all9,"GRAM_per_kg_9450-FSS_Driff_both_days_E_mat.csv") # This creates Data-9450 in GRAM_per_kgs for both interview days, but only for Serie E





































C4<-read.csv("DR1IFF_F.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_1_F_mat.csv")


## Second interview day of 3099 data##

C4<-read.csv("DR2IFF_F.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_2_F_mat.csv")




##First interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR1IFF_F.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

#Alta<-read.csv("3099_NOVA1-4_NHANES_2003-2020_consistent_food_codes.csv",header=TRUE) # for creating 3099-data foods
#Alta<-Alta[,-1]

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_F_mat.csv")


##Second interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR2IFF_F.csv")

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}}

colnames(Fr)[1]<-"seqn"
Fr<-cbind(Tur[,1],Fr)

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_F_mat.csv") # This creates participant vs food codes for serie F


##Summing first and second interview days##

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


C1<-read.csv("GRAM_per_kg_3099-FSS_Driff_1_F_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_3099-FSS_Driff_2_F_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

write.csv("GRAM_per_kg_3099-FSS_Driff_both_days_F_mat.csv") # This creates Data-3099 in grams for both interview days, but only for Serie F


C1<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_F_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_F_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

C3A<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_F_mat.csv")
C3A<-C3A[,-1]

C3_3outof9_plus_all9<-cbind(C3,C3A[,2:ncol(C3A)])
write.csv(C3_3outof9_plus_all9,"GRAM_per_kg_9450-FSS_Driff_both_days_F_mat.csv") # This creates Data-9450 in grams for both interview days, but only for Serie F










































C4<-read.csv("DR1IFF_G.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_1_G_mat.csv")  # This creates participant vs food codes for serie G, first interview day of 3099 food size set.



## Second interview day of 3099 data##

C4<-read.csv("DR2IFF_G.csv") #the same must be repeated with "DR2IFF_D.csv", "DR2IFF_E.csv", "DR2IFF_F.csv", "DR2IFF_G.csv","DR2IFF_H.csv","DR2IFF_I.csv","DR2IFF_J.csv" "DR2IFF_P.csv"
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_2_G_mat.csv")  # This creates participant vs food codes for serie G, second interview day of 3099 food size set.




##First interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR1IFF_G.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

#Alta<-read.csv("3099_NOVA1-4_NHANES_2003-2020_consistent_food_codes.csv",header=TRUE) # for creating 3099-data foods
#Alta<-Alta[,-1]

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_G_mat.csv") # This creates participant vs food codes for serie G, first interview day of 6351 foods repeated 3 out of 9 times. 


##Second interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR2IFF_G.csv")

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}}

colnames(Fr)[1]<-"seqn"
Fr<-cbind(Tur[,1],Fr)

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_G_mat.csv") # This creates participant vs food codes for serie G, second interview day of 6351 foods repeated 3 out of 9 times. 


##Summing first and second interview days##

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


C1<-read.csv("GRAM_per_kg_3099-FSS_Driff_1_G_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_3099-FSS_Driff_2_G_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

write.csv("GRAM_per_kg_3099-FSS_Driff_both_days_G_mat.csv") # This creates Data-3099 in GRAM_per_kgs for both interview days, but only for Serie G


C1<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_G_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_G_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

C3A<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_G_mat.csv")
C3A<-C3A[,-1]

C3_3outof9_plus_all9<-cbind(C3,C3A[,2:ncol(C3A)])
write.csv(C3_3outof9_plus_all9,"GRAM_per_kg_9450-FSS_Driff_both_days_G_mat.csv") # This creates Data-9450 in GRAM_per_kgs for both interview days, but only for Serie G

























C4<-read.csv("DR1IFF_H.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_1_H_mat.csv")  # This creates participant vs food codes for serie G, first interview day of 3099 food size set.



## Second interview day of 3099 data##

C4<-read.csv("DR2IFF_H.csv") #the same must be repeated with "DR2IFF_D.csv", "DR2IFF_E.csv", "DR2IFF_F.csv", "DR2IFF_G.csv","DR2IFF_H.csv","DR2IFF_I.csv","DR2IFF_J.csv" "DR2IFF_P.csv"
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_2_H_mat.csv")  # This creates participant vs food codes for serie G, second interview day of 3099 food size set.




##First interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR1IFF_H.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

#Alta<-read.csv("3099_NOVA1-4_NHANES_2003-2020_consistent_food_codes.csv",header=TRUE) # for creating 3099-data foods
#Alta<-Alta[,-1]

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_H_mat.csv") # This creates participant vs food codes for serie G, first interview day of 6351 foods repeated 3 out of 9 times. 


##Second interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR2IFF_H.csv")

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}}

colnames(Fr)[1]<-"seqn"
Fr<-cbind(Tur[,1],Fr)

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_H_mat.csv") # This creates participant vs food codes for serie G, second interview day of 6351 foods repeated 3 out of 9 times. 


##Summing first and second interview days##

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


C1<-read.csv("GRAM_per_kg_3099-FSS_Driff_1_H_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_3099-FSS_Driff_2_H_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

write.csv("GRAM_per_kg_3099-FSS_Driff_both_days_H_mat.csv") # This creates Data-3099 in GRAM_per_kgs for both interview days, but only for Serie G


C1<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_H_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_H_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

C3A<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_H_mat.csv")
C3A<-C3A[,-1]

C3_3outof9_plus_all9<-cbind(C3,C3A[,2:ncol(C3A)])
write.csv(C3_3outof9_plus_all9,"GRAM_per_kg_9450-FSS_Driff_both_days_H_mat.csv") # This creates Data-9450 in GRAM_per_kgs for both interview days, but only for Serie G




































C4<-read.csv("DR1IFF_I.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_1_I_mat.csv")  # This creates participant vs food codes for serie G, first interview day of 3099 food size set.



## Second interview day of 3099 data##

C4<-read.csv("DR2IFF_I.csv") #the same must be repeated with "DR2IFF_D.csv", "DR2IFF_E.csv", "DR2IFF_F.csv", "DR2IFF_G.csv","DR2IFF_H.csv","DR2IFF_I.csv","DR2IFF_J.csv" "DR2IFF_P.csv"
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_2_I_mat.csv")  # This creates participant vs food codes for serie G, second interview day of 3099 food size set.




##First interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR1IFF_I.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

#Alta<-read.csv("3099_NOVA1-4_NHANES_2003-2020_consistent_food_codes.csv",header=TRUE) # for creating 3099-data foods
#Alta<-Alta[,-1]

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_I_mat.csv") # This creates participant vs food codes for serie G, first interview day of 6351 foods repeated 3 out of 9 times. 


##Second interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR2IFF_I.csv")

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}}

colnames(Fr)[1]<-"seqn"
Fr<-cbind(Tur[,1],Fr)

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_I_mat.csv") # This creates participant vs food codes for serie G, second interview day of 6351 foods repeated 3 out of 9 times. 


##Summing first and second interview days##

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


C1<-read.csv("GRAM_per_kg_3099-FSS_Driff_1_I_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_3099-FSS_Driff_2_I_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

write.csv("GRAM_per_kg_3099-FSS_Driff_both_days_I_mat.csv") # This creates Data-3099 in GRAM_per_kgs for both interview days, but only for Serie G


C1<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_I_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_I_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

C3A<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_I_mat.csv")
C3A<-C3A[,-1]

C3_3outof9_plus_all9<-cbind(C3,C3A[,2:ncol(C3A)])
write.csv(C3_3outof9_plus_all9,"GRAM_per_kg_9450-FSS_Driff_both_days_I_mat.csv") # This creates Data-9450 in GRAM_per_kgs for both interview days, but only for Serie G













































C4<-read.csv("DR1IFF_J.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_1_J_mat.csv")  # This creates participant vs food codes for serie G, first interview day of 3099 food size set.



## Second interview day of 3099 data##

C4<-read.csv("DR2IFF_J.csv") #the same must be repeated with "DR2IFF_D.csv", "DR2IFF_E.csv", "DR2IFF_F.csv", "DR2IFF_G.csv","DR2IFF_H.csv","DR2IFF_I.csv","DR2IFF_J.csv" "DR2IFF_P.csv"
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_2_J_mat.csv")  # This creates participant vs food codes for serie G, second interview day of 3099 food size set.




##First interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR1IFF_J.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

#Alta<-read.csv("3099_NOVA1-4_NHANES_2003-2020_consistent_food_codes.csv",header=TRUE) # for creating 3099-data foods
#Alta<-Alta[,-1]

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_J_mat.csv") # This creates participant vs food codes for serie G, first interview day of 6351 foods repeated 3 out of 9 times. 


##Second interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR2IFF_J.csv")

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}}

colnames(Fr)[1]<-"seqn"
Fr<-cbind(Tur[,1],Fr)

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_J_mat.csv") # This creates participant vs food codes for serie G, second interview day of 6351 foods repeated 3 out of 9 times. 


##Summing first and second interview days##

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


C1<-read.csv("GRAM_per_kg_3099-FSS_Driff_1_J_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_3099-FSS_Driff_2_J_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

write.csv("GRAM_per_kg_3099-FSS_Driff_both_days_J_mat.csv") # This creates Data-3099 in GRAM_per_kgs for both interview days, but only for Serie G


C1<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_J_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_J_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

C3A<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_J_mat.csv")
C3A<-C3A[,-1]

C3_3outof9_plus_all9<-cbind(C3,C3A[,2:ncol(C3A)])
write.csv(C3_3outof9_plus_all9,"GRAM_per_kg_9450-FSS_Driff_both_days_J_mat.csv") # This creates Data-9450 in GRAM_per_kgs for both interview days, but only for Serie G
































C4<-read.csv("DR1IFF_P.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_1_P_mat.csv")  # This creates participant vs food codes for serie G, first interview day of 3099 food size set.



## Second interview day of 3099 data##

C4<-read.csv("DR2IFF_P.csv") #the same must be repeated with "DR2IFF_D.csv", "DR2IFF_E.csv", "DR2IFF_F.csv", "DR2IFF_G.csv","DR2IFF_H.csv","DR2IFF_I.csv","DR2IFF_J.csv" "DR2IFF_P.csv"
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta<-read.csv("Data_3099_food_codes_g_per_100g.csv",header=TRUE)
Alta<-Alta[,-1]

I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_3099-FSS_Driff_2_P_mat.csv")  # This creates participant vs food codes for serie G, second interview day of 3099 food size set.




##First interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR1IFF_P.csv")
C4<-C4[,-1]

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr1ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1


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

#Alta<-read.csv("3099_NOVA1-4_NHANES_2003-2020_consistent_food_codes.csv",header=TRUE) # for creating 3099-data foods
#Alta<-Alta[,-1]

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr1ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr1ifdcd"]
Alta[I[1]] %in% C41[,"dr1ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr1ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr1ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr1igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}} # prijasnji alter: Fr[i,j]<-0


Fr<-cbind(Tur[,1],Fr)
colnames(Fr)[1]<-"seqn"

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_P_mat.csv") # This creates participant vs food codes for serie G, first interview day of 6351 foods repeated 3 out of 9 times. 


##Second interview day of 9450 data - part 3 out of 9 ##

C4<-read.csv("DR2IFF_P.csv")

fe<-c()
for (i in 1:nrow(C4)) {
if (is.na(C4[i,"dr2ikcal"])==TRUE) {
fe<-cbind(fe,c(i))}}

C4<-C4[-fe,]

C1<-C4
C4<-C1

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

C41<-C4[,-grep("dr2.020", colnames(C4))]

Tur<-matrix(rep(NA, length(unique(C41[,"seqn"]))*ncol(C41)),nrow=length(unique(C41[,"seqn"])))
for (i in 1:(length(fes)-1)) {
Tur[i,]<-colMeans(C41[fes[i]:(fes[i+1]-1),])
} 
colnames(Tur)<-colnames(C41)

Alta2<-read.csv("Supporting_Data_3_out_of_9_food_resulting_in_6351_foods_in_grams.csv",header=TRUE)
Alta2<-Alta2[,ncol(Alta2)]
Alta<-Alta2
I<-c()
for (i in 1:length(Alta)) {
if (Alta[i]%in% C41[,"dr2ifdcd"])
I<-cbind(I,i)}

Fr<-matrix(rep(0, length(Alta)*dim(Tur)[1]),nrow=dim(Tur)[1])
colnames(Fr)<-Alta

Alta[I[1]] %in% C41[fes[1]:(fes[1+1]-1),"dr2ifdcd"]
Alta[I[1]] %in% C41[,"dr2ifdcd"]

for (i in 1:dim(Tur)[1]) {
for (j in 1:length(I)) {
if (Alta[I[j]] %in% C41[fes[i]:(fes[i+1]-1),"dr2ifdcd"]) {

sum<-0
for (k in 1:length(fes[i]:(fes[i+1]-1))) {
if (Alta[I[j]]==C41[fes[i]-1+k,"dr2ifdcd"])
sum<-sum+(C41[fes[i]-1+k,"dr2igrms"]/C41[fes[i]-1+k,"bmxwt"])
}

Fr[i,I[j]]<-sum}}}

colnames(Fr)[1]<-"seqn"
Fr<-cbind(Tur[,1],Fr)

write.csv(Fr,"GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_P_mat.csv") # This creates participant vs food codes for serie G, second interview day of 6351 foods repeated 3 out of 9 times. 


##Summing first and second interview days##

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


C1<-read.csv("GRAM_per_kg_3099-FSS_Driff_1_P_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_3099-FSS_Driff_2_P_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

write.csv("GRAM_per_kg_3099-FSS_Driff_both_days_P_mat.csv") # This creates Data-3099 in GRAM_per_kgs for both interview days, but only for Serie G


C1<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_1_P_mat.csv")
C1<-C1[,-1]
colnames(C1)[1]<-"seqn"

C2<-read.csv("GRAM_per_kg_Added_3_outof_9_NHACONS_Driff_2_P_mat.csv")
C2<-C2[,-1]
colnames(C2)[1]<-"seqn"

C1C2<-Mer2(C1,C2)

C3<-cbind(C1C2[,1],(C1C2[,2:ncol(C1)]+C1C2[,(2+ncol(C1)):ncol(C1C2)]))
colnames(C3)[1]<-"seqn"

C3A<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_P_mat.csv")
C3A<-C3A[,-1]

C3_3outof9_plus_all9<-cbind(C3,C3A[,2:ncol(C3A)])
write.csv(C3_3outof9_plus_all9,"GRAM_per_kg_9450-FSS_Driff_both_days_P_mat.csv") # This creates Data-9450 in GRAM_per_kgs for both interview days, but only for Serie G




C3<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_C_mat.csv",header=TRUE)
C3<-C3[,-1]
D3<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_D_mat.csv",header=TRUE)
D3<-D3[,-1]
E3<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_E_mat.csv",header=TRUE)
E3<-E3[,-1]
F3<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_F_mat.csv",header=TRUE)
F3<-F3[,-1]
G3<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_G_mat.csv",header=TRUE)
G3<-G3[,-1]
H3<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_H_mat.csv",header=TRUE)
H3<-H3[,-1]
I3<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_I_mat.csv",header=TRUE)
I3<-I3[,-1]
J3<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_J_mat.csv",header=TRUE)
J3<-J3[,-1]
P3<-read.csv("GRAM_per_kg_3099-FSS_Driff_both_days_P_mat.csv",header=TRUE)
P3<-P3[,-1]

Merge<-rbind(C3,D3,E3,F3,G3,H3,I3,J3,P3)

write.csv(Merge,"Merged_GRAM_per_kg_3099-FSS_Driff_both_days_C-P.csv")





C3<-read.csv("GRAM_per_kg_9450-FSS_Driff_both_days_C_mat.csv",header=TRUE)
C3<-C3[,-1]
D3<-read.csv("GRAM_per_kg_9450-FSS_Driff_both_days_D_mat.csv",header=TRUE)
D3<-D3[,-1]
E3<-read.csv("GRAM_per_kg_9450-FSS_Driff_both_days_E_mat.csv",header=TRUE)
E3<-E3[,-1]
F3<-read.csv("GRAM_per_kg_9450-FSS_Driff_both_days_F_mat.csv",header=TRUE)
F3<-F3[,-1]
G3<-read.csv("GRAM_per_kg_9450-FSS_Driff_both_days_G_mat.csv",header=TRUE)
G3<-G3[,-1]
H3<-read.csv("GRAM_per_kg_9450-FSS_Driff_both_days_H_mat.csv",header=TRUE)
H3<-H3[,-1]
I3<-read.csv("GRAM_per_kg_9450-FSS_Driff_both_days_I_mat.csv",header=TRUE)
I3<-I3[,-1]
J3<-read.csv("GRAM_per_kg_9450-FSS_Driff_both_days_J_mat.csv",header=TRUE)
J3<-J3[,-1]
P3<-read.csv("GRAM_per_kg_9450-FSS_Driff_both_days_P_mat.csv",header=TRUE)
P3<-P3[,-1]

Merge<-rbind(C3,D3,E3,F3,G3,H3,I3,J3,P3)

write.csv(Merge,"Merged_GRAM_per_kg_9450-FSS_Driff_both_days_C-P.csv") # This creates merged Data-9450 in GRAM_per_kgs for both interview days for all series C-P. The similar is obtained for Data-3099, just load "GRAM_per_kg_3099-FSS..." files.  





### Finally how C1C2 files are created for grams per kg###

A<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907.csv") #From Datasets!

A<-A[,-1]

B<-read.csv("Merged_GRAM_per_kg_3099-FSS_Driff_both_days_C-P.csv")

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
write.csv("C1C2_F1_05.csv") # The same should be done with PTFA=2 (only non-anemia cases), males, g/kg and No. days. And the similar procedure was repeated for F50+!


A<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907_mcq053_2.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F1_06.csv")


A<-read.csv("Female_aged_50_and_above.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F5_05.csv")



A<-read.csv("Female_aged_50_and_above_mcq053_2.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F5_06.csv")








A<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907.csv") #From Datasets!

A<-A[,-1]

B<-read.csv("Merged_GRAM_per_kg_9450-FSS_Driff_both_days_C-P.csv")

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
write.csv("C1C2_F1_11.csv") # The same should be done with PTFA=2 (only non-anemia cases), males, g/kg and No. days. And the similar procedure was repeated for F50+!


A<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907_mcq053_2.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F1_12.csv")


A<-read.csv("Female_aged_50_and_above.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F5_11.csv")



A<-read.csv("Female_aged_50_and_above_mcq053_2.csv") #From Datasets!
A<-A[,-1]
C1<-A
C2<-B
colnames(C1)[1]<-"seqn"
colnames(C2)[1]<-"seqn"
C1C2<-Mer2(C1,C2)
C1C2<-C1C2[,-4]
write.csv("C1C2_F5_12.csv")







