
setwd('Prepared_for_Fin_Swe')

files <- list.files()
f <- list()
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}

for (i in 1:length(files)) {
E<-f[i]
#E<-as.numeric(unlist(E))
#E<-E[-length(E)]
if (i==1) {
F<-E}
else {
F<-merge(F,E,by="Food..FoodEx2.description.")}
}

F1<-F[!duplicated(F[,"Food..FoodEx2.description."]),]



finland_cols <- F1[, grep("^Finland", names(F1))]

colnames(finland_cols)<-files

Nutr_for_Finland<-c("Ca","Fe","Mg","Phos","Potassium","Zn","Cu","Se","Vit_E","B1","B2","B3","B6","B12","Vit_K")

Finland<-cbind(F1[,1:2],finland_cols)

Finland_fin<-Finland
colnames(Finland_fin)<-gsub(".csv","",colnames(Finland))

Finland_fin2<-cbind(Finland_fin[,1:2],Finland_fin[,Nutr_for_Finland])

FINland<-Finland_fin2

for (i in 3:ncol(Finland_fin2)) {
if (colnames(Finland_fin2)[i]%in%c("Se","B12","Vit_K")) {
FINland[,i]<-Finland_fin2[,i]/1000000
}
else {
FINland[,i]<-Finland_fin2[,i]/1000}
}


Mat<- FINland
for (i in 3:ncol(FINland)) {
for (j in 1:nrow(FINland)) {
if ((log(FINland[j,i])<(-20))||(is.na(FINland[j,i])==TRUE)) {
Mat[j,i]<--20}
else {
Mat[j,i]<-log(FINland[j,i])}
}}

colnames(Mat)[3:ncol(Mat)]<-c("Calcium","Iron","Magnesium","Phosphorus","Potassium","Zinc","Copper","Selenium","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Vitamin.K..phylloquinone.")

write.csv(Mat,"../Finland_15_nutr_for_FPro.csv")


finland_cols <- F1[, grep("^Sweden", names(F1))]

colnames(finland_cols)<-files

Nutr_for_Finland<-c("Ca","Fe","Mg","Phos","Potassium","Zn","Cu","Se","Vit_E","B1","B2","B3","B6","B12","Vit_K")

Finland<-cbind(F1[,1:2],finland_cols)

Finland_fin<-Finland
colnames(Finland_fin)<-gsub(".csv","",colnames(Finland))

Finland_fin2<-cbind(Finland_fin[,1:2],Finland_fin[,Nutr_for_Finland])

FINland<-Finland_fin2

for (i in 3:ncol(Finland_fin2)) {
if (colnames(Finland_fin2)[i]%in%c("Se","B12","Vit_K")) {
FINland[,i]<-Finland_fin2[,i]/1000000
}
else {
FINland[,i]<-Finland_fin2[,i]/1000}
}


Mat<- FINland
for (i in 3:ncol(FINland)) {
for (j in 1:nrow(FINland)) {
if ((log(FINland[j,i])<(-20))||(is.na(FINland[j,i])==TRUE)) {
Mat[j,i]<--20}
else {
Mat[j,i]<-log(FINland[j,i])}
}}

colnames(Mat)[3:ncol(Mat)]<-c("Calcium","Iron","Magnesium","Phosphorus","Potassium","Zinc","Copper","Selenium","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Vitamin.K..phylloquinone.")

write.csv(Mat,"../Sweden_15_nutr_for_FPro.csv")


setwd('../Prepared_for_FR_GER_I_NE_UK')

files <- list.files()
f <- list()
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}

for (i in 1:length(files)) {
E<-f[i]
#E<-as.numeric(unlist(E))
#E<-E[-length(E)]
if (i==1) {
F<-E}
else {
F<-merge(F,E,by="Food..FoodEx2.description.")}
}

F1<-F[!duplicated(F[,"Food..FoodEx2.description."]),]


finland_cols <- F1[, grep("^France", names(F1))]

colnames(finland_cols)<-files

Nutr_for_Finland<-c("Ca","Fe","Mg","Phos","Potassium","Zn","Cu","Se","Vit_E","B1","B2","B3","B6","B12","Vit_K")

Finland<-cbind(F1[,1:2],finland_cols)

Finland_fin<-Finland
colnames(Finland_fin)<-gsub(".csv","",colnames(Finland))

Finland_fin2<-cbind(Finland_fin[,1:2],Finland_fin[,Nutr_for_Finland])

FINland<-Finland_fin2

for (i in 3:ncol(Finland_fin2)) {
if (colnames(Finland_fin2)[i]%in%c("Se","B12","Vit_K")) {
FINland[,i]<-Finland_fin2[,i]/1000000
}
else {
FINland[,i]<-Finland_fin2[,i]/1000}
}


Mat<- FINland
for (i in 3:ncol(FINland)) {
for (j in 1:nrow(FINland)) {
if ((log(FINland[j,i])<(-20))||(is.na(FINland[j,i])==TRUE)) {
Mat[j,i]<--20}
else {
Mat[j,i]<-log(FINland[j,i])}
}}

colnames(Mat)[3:ncol(Mat)]<-c("Calcium","Iron","Magnesium","Phosphorus","Potassium","Zinc","Copper","Selenium","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Vitamin.K..phylloquinone.")

write.csv(Mat,"../France_15_nutr_for_FPro.csv")

finland_cols <- F1[, grep("^Germany", names(F1))]

colnames(finland_cols)<-files

Nutr_for_Finland<-c("Ca","Fe","Mg","Phos","Potassium","Zn","Cu","Se","Vit_E","B1","B2","B3","B6","B12","Vit_K")

Finland<-cbind(F1[,1:2],finland_cols)

Finland_fin<-Finland
colnames(Finland_fin)<-gsub(".csv","",colnames(Finland))

Finland_fin2<-cbind(Finland_fin[,1:2],Finland_fin[,Nutr_for_Finland])

FINland<-Finland_fin2

for (i in 3:ncol(Finland_fin2)) {
if (colnames(Finland_fin2)[i]%in%c("Se","B12","Vit_K")) {
FINland[,i]<-Finland_fin2[,i]/1000000
}
else {
FINland[,i]<-Finland_fin2[,i]/1000}
}


Mat<- FINland
for (i in 3:ncol(FINland)) {
for (j in 1:nrow(FINland)) {
if ((log(FINland[j,i])<(-20))||(is.na(FINland[j,i])==TRUE)) {
Mat[j,i]<--20}
else {
Mat[j,i]<-log(FINland[j,i])}
}}

colnames(Mat)[3:ncol(Mat)]<-c("Calcium","Iron","Magnesium","Phosphorus","Potassium","Zinc","Copper","Selenium","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Vitamin.K..phylloquinone.")

write.csv(Mat,"../Germany_15_nutr_for_FPro.csv")


finland_cols <- F1[, grep("^Italy", names(F1))]

colnames(finland_cols)<-files

Nutr_for_Finland<-c("Ca","Fe","Mg","Phos","Potassium","Zn","Cu","Se","Vit_E","B1","B2","B3","B6","B12","Vit_K")

Finland<-cbind(F1[,1:2],finland_cols)

Finland_fin<-Finland
colnames(Finland_fin)<-gsub(".csv","",colnames(Finland))

Finland_fin2<-cbind(Finland_fin[,1:2],Finland_fin[,Nutr_for_Finland])

FINland<-Finland_fin2

for (i in 3:ncol(Finland_fin2)) {
if (colnames(Finland_fin2)[i]%in%c("Se","B12","Vit_K")) {
FINland[,i]<-Finland_fin2[,i]/1000000
}
else {
FINland[,i]<-Finland_fin2[,i]/1000}
}


Mat<- FINland
for (i in 3:ncol(FINland)) {
for (j in 1:nrow(FINland)) {
if ((log(FINland[j,i])<(-20))||(is.na(FINland[j,i])==TRUE)) {
Mat[j,i]<--20}
else {
Mat[j,i]<-log(FINland[j,i])}
}}

colnames(Mat)[3:ncol(Mat)]<-c("Calcium","Iron","Magnesium","Phosphorus","Potassium","Zinc","Copper","Selenium","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Vitamin.K..phylloquinone.")

write.csv(Mat,"../Italy_15_nutr_for_FPro.csv")


finland_cols <- F1[, grep("^Netherlands", names(F1))]

colnames(finland_cols)<-files

Nutr_for_Finland<-c("Ca","Fe","Mg","Phos","Potassium","Zn","Cu","Se","Vit_E","B1","B2","B3","B6","B12","Vit_K")

Finland<-cbind(F1[,1:2],finland_cols)

Finland_fin<-Finland
colnames(Finland_fin)<-gsub(".csv","",colnames(Finland))

Finland_fin2<-cbind(Finland_fin[,1:2],Finland_fin[,Nutr_for_Finland])

FINland<-Finland_fin2

for (i in 3:ncol(Finland_fin2)) {
if (colnames(Finland_fin2)[i]%in%c("Se","B12","Vit_K")) {
FINland[,i]<-Finland_fin2[,i]/1000000
}
else {
FINland[,i]<-Finland_fin2[,i]/1000}
}


Mat<- FINland
for (i in 3:ncol(FINland)) {
for (j in 1:nrow(FINland)) {
if ((log(FINland[j,i])<(-20))||(is.na(FINland[j,i])==TRUE)) {
Mat[j,i]<--20}
else {
Mat[j,i]<-log(FINland[j,i])}
}}

colnames(Mat)[3:ncol(Mat)]<-c("Calcium","Iron","Magnesium","Phosphorus","Potassium","Zinc","Copper","Selenium","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Vitamin.K..phylloquinone.")

write.csv(Mat,"../Netherlands_15_nutr_for_FPro.csv")


finland_cols <- F1[, grep("^United.Kingdom", names(F1))]

colnames(finland_cols)<-files

Nutr_for_Finland<-c("Ca","Fe","Mg","Phos","Potassium","Zn","Cu","Se","Vit_E","B1","B2","B3","B6","B12","Vit_K")

Finland<-cbind(F1[,1:2],finland_cols)

Finland_fin<-Finland
colnames(Finland_fin)<-gsub(".csv","",colnames(Finland))

Finland_fin2<-cbind(Finland_fin[,1:2],Finland_fin[,Nutr_for_Finland])

FINland<-Finland_fin2

for (i in 3:ncol(Finland_fin2)) {
if (colnames(Finland_fin2)[i]%in%c("Se","B12","Vit_K")) {
FINland[,i]<-Finland_fin2[,i]/1000000
}
else {
FINland[,i]<-Finland_fin2[,i]/1000}
}


Mat<- FINland
for (i in 3:ncol(FINland)) {
for (j in 1:nrow(FINland)) {
if ((log(FINland[j,i])<(-20))||(is.na(FINland[j,i])==TRUE)) {
Mat[j,i]<--20}
else {
Mat[j,i]<-log(FINland[j,i])}
}}

colnames(Mat)[3:ncol(Mat)]<-c("Calcium","Iron","Magnesium","Phosphorus","Potassium","Zinc","Copper","Selenium","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Vitamin.K..phylloquinone.")

write.csv(Mat,"../United.Kingdom_15_nutr_for_FPro.csv")



