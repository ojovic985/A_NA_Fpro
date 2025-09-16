
A<-read.csv('hn23_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#some knhanes years do not contain some nutrients


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn23_all.csv")






A<-read.csv('hn22_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#some knhanes years do not contain some nutrients


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn22_all.csv")


A<-read.csv('hn21_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#some knhanes years do not contain some nutrients


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn21_all.csv")



A<-read.csv('hn20_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#some knhanes years do not contain some nutrients


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn20_all.csv")



A<-read.csv('hn19_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#some knhanes years do not contain some nutrients


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn19_all.csv")



A<-read.csv('hn18_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#some knhanes years do not contain some nutrients


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn18_all.csv")



A<-read.csv('hn17_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#some knhanes years do not contain some nutrients


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn17_all.csv")


A<-read.csv('hn16_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#some knhanes years do not contain some nutrients


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn16_all.csv")




A<-read.csv('hn15_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#some knhanes years do not contain some nutrients


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn15_all.csv")



A<-read.csv('hn14_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#########For hn14 and hn13 (below):
colnames(Weighted_Nutrients)<-gsub("NF_chol","NF_CHOL",colnames(Weighted_Nutrients))
colnames(Weighted_Nutrients)<-gsub("NF_tdf","NF_TDF",colnames(Weighted_Nutrients))
#########For hn14 and hn13 (above)

#some knhanes z=years (not) contain some nutrients – knhanes_year_ingredient_comparison.ods


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn14_all.csv")



A<-read.csv('hn13_24rc.csv',header=TRUE)

A1<-A[,which(colnames(A)=="NF_INTK"):which(colnames(A)=="NF_VITC")]
A2<-A1[!colnames(A1) %in% c("NF_INTK3")]
C<-cbind(A[,"ID"],A2)
colnames(C)[1]<-"ID"
C<-na.omit(C)
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="NF_WATER"):which(colnames(C)=="NF_VITC")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"NF_INTK"]} }
else {
for (k in 1:dezi[j]) {
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

#########For hn14 and hn13 (below):
colnames(Weighted_Nutrients)<-gsub("NF_chol","NF_CHOL",colnames(Weighted_Nutrients))
colnames(Weighted_Nutrients)<-gsub("NF_tdf","NF_TDF",colnames(Weighted_Nutrients))
#########For hn14 and hn13 (above)

#some knhanes z=years (not) contain some nutrients – knhanes_year_ingredient_comparison.ods


Weighted_Nutrients[,"NF_CA"]<-Weighted_Nutrients[,"NF_CA"]/1000
Weighted_Nutrients[,"NF_FE"]<-Weighted_Nutrients[,"NF_FE"]/1000
#Weighted_Nutrients[,"NF_MG"]<-Weighted_Nutrients[,"NF_MG"]/1000
Weighted_Nutrients[,"NF_PHOS"]<-Weighted_Nutrients[,"NF_PHOS"]/1000
Weighted_Nutrients[,"NF_K"]<-Weighted_Nutrients[,"NF_K"]/1000
Weighted_Nutrients[,"NF_NA"]<-Weighted_Nutrients[,"NF_NA"]/1000
#Weighted_Nutrients[,"NF_ZN"]<-Weighted_Nutrients[,"NF_ZN"]/1000
Weighted_Nutrients[,"NF_RETIN"]<-Weighted_Nutrients[,"NF_RETIN"]/1000000
Weighted_Nutrients[,"NF_CAROT"]<-Weighted_Nutrients[,"NF_CAROT"]/1000000
#Weighted_Nutrients[,"NF_VITE"]<-Weighted_Nutrients[,"NF_VITE"]/1000
#Weighted_Nutrients[,"NF_VITD"]<-Weighted_Nutrients[,"NF_VITD"]/1000000
Weighted_Nutrients[,"NF_VITC"]<-Weighted_Nutrients[,"NF_VITC"]/1000
Weighted_Nutrients[,"NF_B1"]<-Weighted_Nutrients[,"NF_B1"]/1000
Weighted_Nutrients[,"NF_B2"]<-Weighted_Nutrients[,"NF_B2"]/1000
Weighted_Nutrients[,"NF_NIAC"]<-Weighted_Nutrients[,"NF_NIAC"]/1000
#Weighted_Nutrients[,"NF_FOLATE"]<-Weighted_Nutrients[,"NF_FOLATE"]/1000000
Weighted_Nutrients[,"NF_CHOL"]<-Weighted_Nutrients[,"NF_CHOL"]/1000
Weighted_Nutrients[,"NF_VA"]<-Weighted_Nutrients[,"NF_VA"]/1000000
#Weighted_Nutrients[,"NF_VA_RAE"]<-Weighted_Nutrients[,"NF_VA_RAE"]/1000000
cols_to_divide1<-grep("^NF_1", colnames(Weighted_Nutrients))
cols_to_divide2<-grep("^NF_2", colnames(Weighted_Nutrients))
Weighted_Nutrients[,cols_to_divide1]<-Weighted_Nutrients[,cols_to_divide1]/1000
Weighted_Nutrients[,cols_to_divide2]<-Weighted_Nutrients[,cols_to_divide2]/1000

Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))


write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_hn13_all.csv")













