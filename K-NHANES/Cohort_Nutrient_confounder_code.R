### All-F Cohort statistics

library(stats)

A<-read.csv("F_Weighted_nutrients_ready_2023-2013_kindg1_n21&22.csv",header=TRUE)

A<-A[,-1]

A<-A[,which(colnames(A)=="ID"):which(colnames(A)=="NF_VITC")]
B<- A[which(colnames(A)=="psu"):which(colnames(A)=="Anaemia")]

data<-B[!colnames(B) %in% c("wt_itvex","edu","D_1_1","BD2_1","BS3_2","HE_HB","FPro")]

Mat<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
Mat<-cbind(Mat,B[,"FPro"])
colnames(Mat)[ncol(Mat)]<-"FPro"

## rows for svyglm
#data1<-data[!colnames(data) %in% c("psu","wt_tot","kstrata")] 
#data2<-data[colnames(data) %in% c("psu","wt_tot","kstrata")]
#data3<-cbind(data1,data2)
## rows for svyglm

data3<-data[!colnames(data) %in% c("psu","kstrata","wt_tot")] # row for GLM

Matu<-matrix(rep(0,ncol(Mat)*2),nrow=2)
Matu_coef<-matrix(rep(0,ncol(Mat)*2),nrow=2)
Conf_vector<-c()
for (j in 1:ncol(Mat)) {

Mat1<-Mat[!colnames(Mat) %in% colnames(Mat)[j]]
data3_2<-cbind(data3,Mat[,colnames(Mat)[j]])
colnames(data3_2)[ncol(data3_2)]<-colnames(Mat)[j]
Matfull2<-c()
for (i in 1:ncol(Mat1)) {
data4<-cbind(data3_2,Mat1[,i])
colnames(data4)[ncol(data4)]<-colnames(Mat1)[i]
#design<-svydesign(id=~psu, strata=~kstrata,weights=~wt_tot,data=data4,nest=TRUE) # row for svyglm

fifth_var<-colnames(data3_2)[ncol(data3_2)]
last_col <- colnames(data4)[ncol(data4)]
formula_str <- paste("Anaemia~age+ainc+HE_BMI+",fifth_var,"+ Var_Uni_edu + Var_much_alco +  Var_smoking + Var_Imperf_health+", last_col)
model_formula <- as.formula(formula_str)
#model <- svyglm(model_formula, design = design, family = quasibinomial()) # for svyglm
model <- glm(model_formula, family = binomial, data = data4) # for glm
Matfull2<-rbind(Matfull2,summary(model)$coefficients[5,])
}
Matu[1,j]<-max(Matfull2[,4])
Matu[2,j]<-min(Matfull2[,4])
Matu_coef[1,j]<-Matfull2[which.max(Matfull2[,4]),1]
Matu_coef[2,j]<-Matfull2[which.min(Matfull2[,4]),1]
Conf_vector<-cbind(Conf_vector,colnames(Mat1)[which.max(Matfull2[,4])])
}
Matu_2<-rbind(Matu, Matu_coef,Conf_vector)
Mat_final<-as.data.frame(Matu_2)
colnames(Mat_final)<-colnames(Mat)
rownames(Mat_final)<-c("max_p","min_p","Coef(max_map)","Coef(min_p)","Nut_top_conf")
write.csv(Mat_final,"Max&Min_p_val_&coef_final_statistics_with_KNHANES_AllF-Nutrient-glm.csv")

### And To obtain results for FPro - only:

rownames(Matfull2)<-colnames(Mat1)
write.csv(Matfull2,"FPro_p_val_&coef_final_statistics_with_KNHANES_AllF-Nutrient-glm.csv")

### F15-49 Cohort statistics

A<-read.csv("F15-49_Weighted_nutrients_ready_2023-2013_kindg1_n21&22.csv",header=TRUE)

A<-A[,-1]

A<-A[,which(colnames(A)=="ID"):which(colnames(A)=="NF_VITC")]
B<- A[which(colnames(A)=="psu"):which(colnames(A)=="Anaemia")]

data<-B[!colnames(B) %in% c("wt_itvex","edu","D_1_1","BD2_1","BS3_2","HE_HB","FPro")]

Mat<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
Mat<-cbind(Mat,B[,"FPro"])
colnames(Mat)[ncol(Mat)]<-"FPro"

## rows for svyglm
#data1<-data[!colnames(data) %in% c("psu","wt_tot","kstrata")] 
#data2<-data[colnames(data) %in% c("psu","wt_tot","kstrata")]
#data3<-cbind(data1,data2)
## rows for svyglm

data3<-data[!colnames(data) %in% c("psu","kstrata","wt_tot")] # row for GLM

Matu<-matrix(rep(0,ncol(Mat)*2),nrow=2)
Matu_coef<-matrix(rep(0,ncol(Mat)*2),nrow=2)
Conf_vector<-c()
for (j in 1:ncol(Mat)) {

Mat1<-Mat[!colnames(Mat) %in% colnames(Mat)[j]]
data3_2<-cbind(data3,Mat[,colnames(Mat)[j]])
colnames(data3_2)[ncol(data3_2)]<-colnames(Mat)[j]
Matfull2<-c()
for (i in 1:ncol(Mat1)) {
data4<-cbind(data3_2,Mat1[,i])
colnames(data4)[ncol(data4)]<-colnames(Mat1)[i]
#design<-svydesign(id=~psu, strata=~kstrata,weights=~wt_tot,data=data4,nest=TRUE) # row for svyglm

fifth_var<-colnames(data3_2)[ncol(data3_2)]
last_col <- colnames(data4)[ncol(data4)]
formula_str <- paste("Anaemia~age+ainc+HE_BMI+",fifth_var,"+ Var_Uni_edu + Var_much_alco +  Var_smoking + Var_Imperf_health+", last_col)
model_formula <- as.formula(formula_str)
#model <- svyglm(model_formula, design = design, family = quasibinomial()) # for svyglm
model <- glm(model_formula, family = binomial, data = data4) # for glm
Matfull2<-rbind(Matfull2,summary(model)$coefficients[5,])
}
Matu[1,j]<-max(Matfull2[,4])
Matu[2,j]<-min(Matfull2[,4])
Matu_coef[1,j]<-Matfull2[which.max(Matfull2[,4]),1]
Matu_coef[2,j]<-Matfull2[which.min(Matfull2[,4]),1]
Conf_vector<-cbind(Conf_vector,colnames(Mat1)[which.max(Matfull2[,4])])
}
Matu_2<-rbind(Matu, Matu_coef,Conf_vector)
Mat_final<-as.data.frame(Matu_2)
colnames(Mat_final)<-colnames(Mat)
rownames(Mat_final)<-c("max_p","min_p","Coef(max_map)","Coef(min_p)","Nut_top_conf")
write.csv(Mat_final,"Max&Min_p_val_&coef_final_statistics_with_KNHANES_F15-49-Nutrient-glm.csv")

### And To obtain results for FPro - only:

rownames(Matfull2)<-colnames(Mat1)

write.csv(Matfull2,"FPro_p_val_&coef_final_statistics_with_KNHANES_F15-49-Nutrient-glm.csv")

