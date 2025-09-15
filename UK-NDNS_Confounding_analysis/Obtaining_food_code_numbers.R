A1<-read.csv("ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.tab",sep="	",header=TRUE)

Codes<-unique(A[,"FoodNumber"])

counts<-table(A[,"FoodNumber"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"FoodNumber"]==as.character(x)))
names(positions)<-repeated_vals

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Cisn3fattyacidsg")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"FoodNumber")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) { #
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.csv")



A2<-read.csv("ndns_rp_yr5-6a_foodleveldietarydata_v2.tab",sep="	",header=TRUE)

Codes<-unique(A[,"FoodNumber"])

counts<-table(A[,"FoodNumber"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"FoodNumber"]==as.character(x)))
names(positions)<-repeated_vals

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Cisn3fattyacidsg")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"FoodNumber")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) { #
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_ndns_rp_yr5-6a_foodleveldietarydata_v2.tab.csv")



A3<-read.csv("ndns_rp_yr7-8a_foodleveldietarydata.tab",sep="	",header=TRUE)

Codes<-unique(A[,"FoodNumber"])

counts<-table(A[,"FoodNumber"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"FoodNumber"]==as.character(x)))
names(positions)<-repeated_vals


Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Cisn3fattyacidsg")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"FoodNumber")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) { #
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_ndns_rp_yr7-8a_foodleveldietarydata.tab.csv")



A4<-read.csv("ndns_rp_yr9a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)

Codes<-unique(A[,"FoodNumber"])

counts<-table(A[,"FoodNumber"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"FoodNumber"]==as.character(x)))
names(positions)<-repeated_vals

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Cisn3fattyacidsg")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"FoodNumber")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) { #
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_ndns_rp_yr9a_foodleveldietarydata_uk_20210831.tab.csv")


A5<-read.csv("ndns_rp_yr10a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)

Codes<-unique(A[,"FoodNumber"])

counts<-table(A[,"FoodNumber"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"FoodNumber"]==as.character(x)))
names(positions)<-repeated_vals

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Cisn3fattyacidsg")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"FoodNumber")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) { #
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_ndns_rp_yr10a_foodleveldietarydata_uk_20210831.tab.csv")



A6<-read.csv("ndns_rp_yr11a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)

Codes<-unique(A[,"FoodNumber"])

counts<-table(A[,"FoodNumber"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"FoodNumber"]==as.character(x)))
names(positions)<-repeated_vals

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Cisn3fattyacidsg")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"FoodNumber")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) { #
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"TotalGrams"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_ndns_rp_yr11a_foodleveldietarydata_uk_20210831.tab.csv")


