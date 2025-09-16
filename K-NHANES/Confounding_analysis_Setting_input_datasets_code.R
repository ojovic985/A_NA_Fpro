A<-read.csv("hn23_all.csv",header=TRUE)

B1<-read.csv("ID_and_Weigh_FPro_for_hn23_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn23_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")] 
Mat2<-cbind(Mat1, B4)

############# NON-PREGNANT WOMEN SELECTION

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn23<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg")]
############# NON-PREGNANT WOMEN SELECTION


A<-read.csv("hn22_all.csv",header=TRUE)
B1<-read.csv("ID_and_Weigh_FPro_for_hn22_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn22_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")] 
Mat2<-cbind(Mat1, B4)

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn22<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg")]


A<-read.csv("hn21_all.csv",header=TRUE)
B1<-read.csv("ID_and_Weigh_FPro_for_hn21_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn21_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")] 
Mat2<-cbind(Mat1, B4)

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn21<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg")]


A<-read.csv("hn20_all.csv",header=TRUE)
B1<-read.csv("ID_and_Weigh_FPro_for_hn20_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn20_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")]
Mat2<-cbind(Mat1, B4)

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn20<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg")]


A<-read.csv("hn19_all.csv",header=TRUE)
B1<-read.csv("ID_and_Weigh_FPro_for_hn19_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn19_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")] 
Mat2<-cbind(Mat1, B4)

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn19<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg")]


A<-read.csv("hn18_all.csv",header=TRUE)
B1<-read.csv("ID_and_Weigh_FPro_for_hn18_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn18_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")] 
Mat2<-cbind(Mat1, B4)

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn18<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg","NF_VA")]


A<-read.csv("hn17_all.csv",header=TRUE)
B1<-read.csv("ID_and_Weigh_Fpro_for_hn17_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn17_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")] 
Mat2<-cbind(Mat1, B4)

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn17<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg","NF_VA")]


A<-read.csv("hn16_all.csv",header=TRUE)
B1<-read.csv("ID_and_Weigh_FPro_for_hn16_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn16_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")] 
Mat2<-cbind(Mat1, B4)

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn16<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg","NF_VA")]

A<-read.csv("hn15_all.csv",header=TRUE)
colnames(A)[2]<-"ID"
B1<-read.csv("ID_and_Weigh_FPro_for_hn15_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn15_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")] 
Mat2<-cbind(Mat1, B4)

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn15<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg")]

A<-read.csv("hn14_all.csv",header=TRUE)
colnames(A)[2]<-"ID"
B1<-read.csv("ID_and_Weigh_FPro_for_hn14_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn14_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")] 
Mat2<-cbind(Mat1, B4)

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn14<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg")]

A<-read.csv("hn13_all.csv",header=TRUE)
colnames(A)[2]<-"ID"
B1<-read.csv("ID_and_Weigh_FPro_for_hn13_all_kindg1_n21&22.csv",header=TRUE)
B1<-B1[,-1]
colnames(B1)<-c("ID","FPro")
B2<-read.csv("ID_and_Weighted_Nutrients_for_hn13_all.csv",header=TRUE)
B2<-B2[,-1]
colnames(B2)[1]<-c("ID")
M1<-merge(A,B1, by="ID")
M2<-merge(M1,B2, by="ID")

B4<-M2[,which(colnames(M2)=="NF_WATER"):which(colnames(M2)=="NF_VITC")]
Mat1<-M2[colnames(M2) %in% c("ID","sex","age","edu","HE_HB","HE_BMI","ainc","BD2_1","BS3_2","HE_prg","D_1_1","FPro","psu","kstrata","wt_itvex","wt_tot")] 
Mat2<-cbind(Mat1, B4)

fe<-c()
for (j in 1:nrow(Mat2)) {
if (( Mat2[j,"age"]>14)&&( Mat2[j,"sex"]==2)&&( is.na(Mat2[j,"HE_prg"])==FALSE)) {
if ((Mat2[j,"HE_prg"])==0) {
fe<-cbind(fe,c(j))}}}

Mat3<-Mat2[fe,]
Mat3_hn13<-Mat3[!colnames(Mat3) %in% c("sex","HE_prg")]



Mat_full_0<-rbind(Mat3_hn23, Mat3_hn22, Mat3_hn21, Mat3_hn20, Mat3_hn19, Mat3_hn18, Mat3_hn17, Mat3_hn16)


Mat_full_01<-rbind(Mat3_hn15, Mat3_hn14, Mat3_hn13)

Mat2f<-Mat_full_0[!colnames(Mat_full_0) %in% colnames(Mat_full_01)]

Mat_full_1<-Mat_full_0[!colnames(Mat_full_0) %in% colnames(Mat2f)]

Mat_full_02<-Mat_full_01[, colnames(Mat_full_1)]

Mat_full<-rbind(Mat_full_1,Mat_full_02)

Mat_full[,"wt_itvex"]<-Mat_full[,"wt_itvex"]/11
Mat_full[,"wt_tot"]<-Mat_full[,"wt_tot"]/11


####
#after sorting to sex=2 (woman) and 15-49 & 50+
#age
#edu 4 – high education, the rest<4 – University graduate or higher
#HE_HB – hemoglobin level in g/dL (<12 g/dL - anemia)
#HE_BMI
#ainc (income in months)
#BD2_1 – 4&5 = 1 (drinking alcohol 7 cups or more), 0 – less than that or unknown – otherwise #(different to 4 or 5.
#BS3_2 – average daily smoking volume in cigarettes, numbers 888 and 999 should be turned into #zeros!
#HE_prg 0 – no pregnancy, 1 – yes, 8 – not applicable - male.
#D_1_1 – subjective health 1 – very good (0 – denoting absence of imperfect subjective health), #2 -goodness, 3 – normal, 4 – bad, 5- very bad, 9 – unknown no response (1 – denoting presence of #imperfect subjective health)

Ha<-duplicated(Mat_full[,"ID"])
any(Ha==TRUE)
#[1] FALSE

fe<-c()
for (j in 1:nrow(Mat_full)) {
if ((is.na(Mat_full[j,"HE_HB"])==TRUE) ||(is.na(Mat_full[j,"ainc"])==TRUE)|| (is.na(Mat_full[j,"HE_BMI"])==TRUE)|| (is.na(Mat_full[j,"D_1_1"])==TRUE)||(is.na(Mat_full[j,"FPro"])==TRUE)) {
fe<-cbind(fe,c(j))}}

Mat_full_N<-Mat_full[-fe,]
Mat_full<-Mat_full_N

NA_s<-array(ncol(Mat_full))
for (j in 1:ncol(Mat_full)) {
NA_s[j]<-sum(is.na(Mat_full[,j]))}
NA_s

Var_Uni_edu<-rep(0,nrow(Mat_full))
Var_much_alco<-rep(0,nrow(Mat_full))
Var_smoking<-rep(0,nrow(Mat_full))
Var_Imperf_health<-rep(0,nrow(Mat_full))
Anaemia<-rep(0,nrow(Mat_full))

for (j in 1:nrow(Mat_full)) {
if ((is.na(Mat_full[j,"edu"])==FALSE)&&(Mat_full[j,"edu"]==4)) {
Var_Uni_edu[j]<-1}
if ((Mat_full[j,"BD2_1"]==4)||(Mat_full[j,"BD2_1"]==5) ){
Var_much_alco[j]<-1}
if ((Mat_full[j,"BS3_2"]==888)||(Mat_full[j,"BS3_2"]==999) ){
Var_smoking[j]<-0}
else {
Var_smoking[j]<-Mat_full[j,"BS3_2"]}
if (Mat_full[j,"D_1_1"]==1) {
Var_Imperf_health[j]<-0}
else {
Var_Imperf_health[j]<-1}
if (Mat_full[j,"HE_HB"]<12.0) {
Anaemia[j]<-1}
}

Mat_var<-cbind(Var_Uni_edu,Var_much_alco,Var_smoking,Var_Imperf_health,Anaemia)

Mat_full_A<-cbind(Mat_full[,which(colnames(Mat_full)=="ID"):which(colnames(Mat_full)=="FPro")],Mat_var,Mat_full[,which(colnames(Mat_full)=="NF_WATER"):which(colnames(Mat_full)=="NF_VITC")])

Mat_full1<-Mat_full_A

write.csv(Mat_full1,"F_Weighted_nutrients_ready_2023-2013_kindg1_n21&22.csv")

fe_age<-c()
for (j in 1:nrow(Mat_full1)) {
if (Mat_full1[j,"age"]>49) {
fe_age<-cbind(fe_age,c(j))}}

Mat_F15_49<-Mat_full1[-fe_age,]
Mat_F50_plus<-Mat_full1[fe_age,]

write.csv(Mat_F15_49,"F15-49_Weighted_nutrients_ready_2023-2013_kindg1_n21&22.csv")
write.csv(Mat_F50_plus,"F50plus_Weighted_nutrients_ready_2023-2013_kindg1_n21&22.csv")
