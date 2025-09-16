### All-F Cohort statistics

library(survey)

A<-read.csv("F_Weighted_nutrients_ready_2023-2013_kindg1_n21&22.csv",header=TRUE)

A<-A[,-1]

A<-A[,which(colnames(A)=="ID"):which(colnames(A)=="NF_VITC")]
B<- A[which(colnames(A)=="psu"):which(colnames(A)=="Anaemia")]

data<-B[!colnames(B) %in% c("wt_itvex","edu","D_1_1","BD2_1","BS3_2","HE_HB","FPro")]

## rows for svyglm
#data1<-data[!colnames(data) %in% c("psu","wt_tot","kstrata")] 
#data2<-data[colnames(data) %in% c("psu","wt_tot","kstrata")]
#data3<-cbind(data1,data2)
## rows for svyglm

data3<-data[!colnames(data) %in% c("psu","kstrata","wt_tot")] # row for GLM

Mat<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
Mat<-cbind(Mat,B[,"FPro"])
colnames(Mat)[ncol(Mat)]<-"FPro"

Matu<-matrix(rep(0,ncol(Mat)*2),nrow=2)
Matu_coef<-matrix(rep(0,ncol(Mat)*2),nrow=2)

for (j in 1:ncol(Mat)) {
Ha<-c()
data4<-cbind(data3,Mat[,j])
colnames(data4)[ncol(data4)]<-colnames(Mat)[j]

#design<-svydesign(id=~psu, strata=~kstrata,weights=~wt_tot,data=data4,nest=TRUE) # row for svyglm

variab <- colnames(data4)[ncol(data4)]
formula_str <- paste("Anaemia~", variab,"+age")
model_formula <- as.formula(formula_str)

#model <- svyglm(model_formula, design = design, family = quasibinomial())
model <- glm(model_formula, family = binomial, data = data4) # for glm

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("Anaemia~", variab,"+age+HE_BMI")
model_formula <- as.formula(formula_str)

#model <- svyglm(model_formula, design = design, family = quasibinomial())
model <- glm(model_formula, family = binomial, data = data4) # for glm

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("Anaemia~", variab,"+age+HE_BMI+ainc")
model_formula <- as.formula(formula_str)

#model <- svyglm(model_formula, design = design, family = quasibinomial())
model <- glm(model_formula, family = binomial, data = data4) # for glm

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("Anaemia~", variab,"+age + ainc + HE_BMI+Var_Imperf_health")
model_formula <- as.formula(formula_str)

#model <- svyglm(model_formula, design = design, family = quasibinomial())
model <- glm(model_formula, family = binomial, data = data4) # for glm

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("Anaemia~", variab,"+age + ainc + HE_BMI+ Var_Uni_edu + Var_much_alco +  Var_smoking + Var_Imperf_health")
model_formula <- as.formula(formula_str)

#model <- svyglm(model_formula, design = design, family = quasibinomial())
model <- glm(model_formula, family = binomial, data = data4) # for glm

Ha<-rbind(Ha,summary(model)$coefficients[2,])

Matu[1,j]<-max(Ha[,4])
Matu[2,j]<-min(Ha[,4])
Matu_coef[1,j]<-Ha[which.max(Ha[,4]),1]
Matu_coef[2,j]<-Ha[which.min(Ha[,4]),1]
}

Mat_fin<-rbind(Matu,Matu_coef)

colnames(Mat_fin)<-colnames(Mat)

rownames(Mat_fin)<-c("max_p","min_p","Coef(max_map)","Coef(min_p)")

write.csv(Mat_fin,"NonNut_Max&Min_p_val_&coef_final_statistics_with_KNHANES_AllF-nonNutrient-glm.csv")

### And To obtain results for FPro - only:

rownames(Ha)<-c("Anaemia ~ FPro+age","Anaemia ~ FPro+age+HE_BMI","Anaemia ~ FPro+age+HE_BMI+ainc","Anaemia ~ FPro+age + ainc + HE_BMI+ Var_Imperf_health","Anaemia ~ FPro+age+ainc+HE_BMI+ Var_Uni_edu + Var_much_alco +  Var_smoking + Var_Imperf_health")

write.csv(Ha,"NonNut_FPro_p_val_&coef_final_statistics_with_KNHANES_AllF-nonNutrient-glm.csv")


### F15-49 Cohort statistics

A<-read.csv("F15-49_Weighted_nutrients_ready_2023-2013_kindg1_n21&22.csv",header=TRUE)

A<-A[,-1]

A<-A[,which(colnames(A)=="ID"):which(colnames(A)=="NF_VITC")]
B<- A[which(colnames(A)=="psu"):which(colnames(A)=="Anaemia")]

data<-B[!colnames(B) %in% c("wt_itvex","edu","D_1_1","BD2_1","BS3_2","HE_HB","FPro")]

## rows for svyglm
#data1<-data[!colnames(data) %in% c("psu","wt_tot","kstrata")] 
#data2<-data[colnames(data) %in% c("psu","wt_tot","kstrata")]
#data3<-cbind(data1,data2)
## rows for svyglm

data3<-data[!colnames(data) %in% c("psu","kstrata","wt_tot")] # row for GLM

Mat<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
Mat<-cbind(Mat,B[,"FPro"])
colnames(Mat)[ncol(Mat)]<-"FPro"

Matu<-matrix(rep(0,ncol(Mat)*2),nrow=2)
Matu_coef<-matrix(rep(0,ncol(Mat)*2),nrow=2)

for (j in 1:ncol(Mat)) {
Ha<-c()
data4<-cbind(data3,Mat[,j])
colnames(data4)[ncol(data4)]<-colnames(Mat)[j]

#design<-svydesign(id=~psu, strata=~kstrata,weights=~wt_tot,data=data4,nest=TRUE) # row for svyglm

variab <- colnames(data4)[ncol(data4)]
formula_str <- paste("Anaemia~", variab,"+age")
model_formula <- as.formula(formula_str)

#model <- svyglm(model_formula, design = design, family = quasibinomial())
model <- glm(model_formula, family = binomial, data = data4) # for glm

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("Anaemia~", variab,"+age+HE_BMI")
model_formula <- as.formula(formula_str)

#model <- svyglm(model_formula, design = design, family = quasibinomial())
model <- glm(model_formula, family = binomial, data = data4) # for glm

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("Anaemia~", variab,"+age+HE_BMI+ainc")
model_formula <- as.formula(formula_str)

#model <- svyglm(model_formula, design = design, family = quasibinomial())
model <- glm(model_formula, family = binomial, data = data4) # for glm

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("Anaemia~", variab,"+age + ainc + HE_BMI+Var_Imperf_health")
model_formula <- as.formula(formula_str)

#model <- svyglm(model_formula, design = design, family = quasibinomial())
model <- glm(model_formula, family = binomial, data = data4) # for glm

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("Anaemia~", variab,"+age + ainc + HE_BMI+ Var_Uni_edu + Var_much_alco +  Var_smoking + Var_Imperf_health")
model_formula <- as.formula(formula_str)

#model <- svyglm(model_formula, design = design, family = quasibinomial())
model <- glm(model_formula, family = binomial, data = data4) # for glm

Ha<-rbind(Ha,summary(model)$coefficients[2,])

Matu[1,j]<-max(Ha[,4])
Matu[2,j]<-min(Ha[,4])
Matu_coef[1,j]<-Ha[which.max(Ha[,4]),1]
Matu_coef[2,j]<-Ha[which.min(Ha[,4]),1]
}

Mat_fin<-rbind(Matu,Matu_coef)

colnames(Mat_fin)<-colnames(Mat)

rownames(Mat_fin)<-c("max_p","min_p","Coef(max_map)","Coef(min_p)")

write.csv(Mat_fin,"Diff_NonNut_Max&Min_p_val_&coef_final_statistics_with_KNHANES_F15-49-nonNutrient-glm.csv")

### And To obtain results for FPro - only:

rownames(Ha)<-c("Anaemia ~ FPro+age","Anaemia ~ FPro+age+HE_BMI","Anaemia ~ FPro+age+HE_BMI+ainc","Anaemia ~ FPro+age + ainc + HE_BMI+ Var_Imperf_health","Anaemia ~ FPro+age+ainc+HE_BMI+ Var_Uni_edu + Var_much_alco +  Var_smoking + Var_Imperf_health")

write.csv(Ha,"Diff_NonNut_FPro_p_val_&coef_final_statistics_with_KNHANES_F15-49-nonNutrient-glm.csv")

