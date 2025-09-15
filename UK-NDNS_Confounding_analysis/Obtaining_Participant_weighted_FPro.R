A<-read.csv("ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.tab",sep="	",header=TRUE)
B<-read.csv('N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr1-4a.csv',header=TRUE)
B<-B[,-1]

C<-merge(B,A,by="FoodNumber",all.x=TRUE)
C<-C[order(C[,"seriali"]),]
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {

if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"TotalGrams"])
corsum<-corsum+C[k,"TotalGrams"] } }}
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { # This part of the code is TOO critical when compared to EFSA
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"TotalGrams"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

colnames(Mat)<-c("seriali","FPro")

write.csv(Mat,"ID_and_Weigh_FPro_for_ndns_rp_yr1-4a.csv")



A<-read.csv("ndns_rp_yr5-6a_foodleveldietarydata_v2.tab",sep="	",header=TRUE)
B<-read.csv('N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr5-6a.csv',header=TRUE)
B<-B[,-1]

C<-merge(B,A,by="FoodNumber",all.x=TRUE)
C<-C[order(C[,"seriali"]),]
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {

if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"TotalGrams"])
corsum<-corsum+C[k,"TotalGrams"] } }}
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { # This part of the code is TOO critical when compared to EFSA
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"TotalGrams"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

colnames(Mat)<-c("seriali","FPro")

write.csv(Mat,"ID_and_Weigh_FPro_for_ndns_rp_yr5-6a.csv")



A<-read.csv("ndns_rp_yr7-8a_foodleveldietarydata.tab",sep="	",header=TRUE)
B<-read.csv('N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr7-8a.csv',header=TRUE)
B<-B[,-1]

C<-merge(B,A,by="FoodNumber",all.x=TRUE)
C<-C[order(C[,"seriali"]),]
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {

if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"TotalGrams"])
corsum<-corsum+C[k,"TotalGrams"] } }}
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { # This part of the code is TOO critical when compared to EFSA
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"TotalGrams"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))


colnames(Mat)<-c("seriali","FPro")

write.csv(Mat,"ID_and_Weigh_FPro_for_ndns_rp_yr7-8a.csv")



A<-read.csv("ndns_rp_yr9a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)
B<-read.csv('N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr9a.csv',header=TRUE)
B<-B[,-1]

C<-merge(B,A,by="FoodNumber",all.x=TRUE)
C<-C[order(C[,"seriali"]),]
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {

if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"TotalGrams"])
corsum<-corsum+C[k,"TotalGrams"] } }}
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { # This part of the code is TOO critical when compared to EFSA
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"TotalGrams"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

colnames(Mat)<-c("seriali","FPro")

write.csv(Mat,"ID_and_Weigh_FPro_for_ndns_rp_yr9a.csv")



A<-read.csv("ndns_rp_yr10a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)
B<-read.csv('N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr10a.csv',header=TRUE)
B<-B[,-1]

C<-merge(B,A,by="FoodNumber",all.x=TRUE)
C<-C[order(C[,"seriali"]),]
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {

if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"TotalGrams"])
corsum<-corsum+C[k,"TotalGrams"] } }}
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { # This part of the code is TOO critical when compared to EFSA
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"TotalGrams"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

colnames(Mat)<-c("seriali","FPro")

write.csv(Mat,"ID_and_Weigh_FPro_for_ndns_rp_yr10a.csv")



A<-read.csv("ndns_rp_yr11a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)
B<-read.csv('N_Code_and_FPro_of_nutr_cont­_of_ndns_rp_yr11a.csv',header=TRUE)
B<-B[,-1]

C<-merge(B,A,by="FoodNumber",all.x=TRUE)
C<-C[order(C[,"seriali"]),]
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {

if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"TotalGrams"])
corsum<-corsum+C[k,"TotalGrams"] } }}
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { # This part of the code is TOO critical when compared to EFSA
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"TotalGrams"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

colnames(Mat)<-c("seriali","FPro")

write.csv(Mat,"ID_and_Weigh_FPro_for_ndns_rp_yr11a.csv")


