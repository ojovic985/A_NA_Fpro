###### For All-F statistics #### 

library(survey)

A1<-read.csv("Female_aged_15-49_with_NEG_UrTest_with_Av_FPro_plus_11_socioeconom_vars_and_62_AVER_foods_vars_7864_cases.csv",header=TRUE)
A1<-A1[,-1]

A2<-read.csv("Female_aged_50_anemia_Av_FPro_plus_11_socioeconom_vars_and_62_AVER_foods_vars_9928_cases.csv",header=TRUE)
A2<-A2[,-1]

A<-rbind(A1,A2)

M<-read.csv("Series_C-P_sdmvpsu_stra_Adj_wtmec2yr.csv",header=TRUE)
M<-M[,-1]

F<-merge(A,M,by="seqn")

data1<-F[,which(colnames(F)=="ridageyr"):which(colnames(F)=="T2")]
data2<-F[,which(colnames(F)=="sdmvpsu"):which(colnames(F)=="wtmec2yr")]
data3<-cbind(data1,data2)
data3<-data3[!colnames(data3) %in% c("Av_FPro")]
Mat<-F[,which(colnames(F)=="Protein"):which(colnames(F)=="Fatty.acids..total.polyunsaturated")]
Mat<-cbind(Mat,F[,"Av_FPro"])
colnames(Mat)[ncol(Mat)]<-"FPro"

Matu<-matrix(rep(0,ncol(Mat)*2),nrow=2)
Matu_coef<-matrix(rep(0,ncol(Mat)*2),nrow=2)

for (j in 1:ncol(Mat)) {

data4<-cbind(data3,Mat[,j])
colnames(data4)[ncol(data4)]<-colnames(Mat)[j]
design <- svydesign(id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr, data = data4, nest = TRUE)

variab <- colnames(data4)[ncol(data4)]
formula_str <- paste("B~", variab,"+ridageyr")
model_formula <- as.formula(formula_str)

model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-summary(model)$coefficients[2,]

formula_str <- paste("B~", variab,"+ridageyr+bmxbmi")
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("B~", variab,"+ridageyr+bmxbmi+indfmpir")
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("B~", variab,"+ridageyr + indfmpir + bmxbmi+R1+R2+R3+R4")
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("B~", variab,"+ridageyr + indfmpir + bmxbmi+R1+R2+R3+R4+T2")
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("B~", variab,"+ridageyr + indfmpir + bmxbmi+R1+R2+R3+R4+ No_diploma_1 + Much_alco_1 +  Smoked_rec_1 + T2")
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-rbind(Ha,summary(model)$coefficients[2,])

Matu[1,j]<-max(Ha[,4])
Matu[2,j]<-min(Ha[,4])
Matu_coef[1,j]<-Ha[which.max(Ha[,4]),1]
Matu_coef[2,j]<-Ha[which.min(Ha[,4]),1]
}

Mat_fin<-rbind(Matu,Matu_coef)
colnames(Mat_fin)<-colnames(Mat)
rownames(Mat_fin)<-c("max_p","min_p","Coef(max_map)","Coef(min_p)")
write.csv(Mat_fin,"NonNut_Max&Min_p_val_&coef_final_statistics_with_US-NHANES_AllF-nonNutrient-svyglm.csv")


### And To obtain results for FPro - only:

rownames(Ha)<-c("Anaemia~ FPro+age","Anaemia ~ FPro+age+bmi", "Anaemia ~ FPro+age+bmi+indfmpir","Anaemia ~ FPro+age+bmi+indfmpir+R1+R2+R3+R4","Anaemia ~ FPro+age+bmi+indfmpir+R1+R2+R3+R4+Var_Imperf_health","Anaemia ~ FPro+age+bmi+indfmpir+ R1 + R2 + R3 + R4+Var_Imperf_health +No_diploma_1 + Much_alco_1 +  Smoked_rec_1")

write.csv(Ha,"NonNut_FPro_p_val_&coef_final_statistics_with_US-NHANES_AllF-nonNutrient-svyglm.csv")




###### For F15-49 statistics #### 

library(survey)

A1<-read.csv("Female_aged_15-49_with_NEG_UrTest_with_Av_FPro_plus_11_socioeconom_vars_and_62_AVER_foods_vars_7864_cases.csv",header=TRUE)
A1<-A1[,-1]

A<-A1
M<-read.csv("Series_C-P_sdmvpsu_stra_Adj_wtmec2yr.csv",header=TRUE)
M<-M[,-1]

F<-merge(A,M,by="seqn")

data1<-F[,which(colnames(F)=="ridageyr"):which(colnames(F)=="T2")]
data2<-F[,which(colnames(F)=="sdmvpsu"):which(colnames(F)=="wtmec2yr")]
data3<-cbind(data1,data2)
data3<-data3[!colnames(data3) %in% c("Av_FPro")]
Mat<-F[,which(colnames(F)=="Protein"):which(colnames(F)=="Fatty.acids..total.polyunsaturated")]
Mat<-cbind(Mat,F[,"Av_FPro"])
colnames(Mat)[ncol(Mat)]<-"FPro"

Matu<-matrix(rep(0,ncol(Mat)*2),nrow=2)
Matu_coef<-matrix(rep(0,ncol(Mat)*2),nrow=2)

for (j in 1:ncol(Mat)) {

data4<-cbind(data3,Mat[,j])
colnames(data4)[ncol(data4)]<-colnames(Mat)[j]
design <- svydesign(id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr, data = data4, nest = TRUE)

variab <- colnames(data4)[ncol(data4)]
formula_str <- paste("B~", variab,"+ridageyr")
model_formula <- as.formula(formula_str)

model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-summary(model)$coefficients[2,]

formula_str <- paste("B~", variab,"+ridageyr+bmxbmi")
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("B~", variab,"+ridageyr+bmxbmi+indfmpir")
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("B~", variab,"+ridageyr + indfmpir + bmxbmi+R1+R2+R3+R4")
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("B~", variab,"+ridageyr + indfmpir + bmxbmi+R1+R2+R3+R4+T2")
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-rbind(Ha,summary(model)$coefficients[2,])

formula_str <- paste("B~", variab,"+ridageyr + indfmpir + bmxbmi+R1+R2+R3+R4+ No_diploma_1 + Much_alco_1 +  Smoked_rec_1 + T2")
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = design, family = quasibinomial())

Ha<-rbind(Ha,summary(model)$coefficients[2,])

Matu[1,j]<-max(Ha[,4])
Matu[2,j]<-min(Ha[,4])
Matu_coef[1,j]<-Ha[which.max(Ha[,4]),1]
Matu_coef[2,j]<-Ha[which.min(Ha[,4]),1]
}

Mat_fin<-rbind(Matu,Matu_coef)
colnames(Mat_fin)<-colnames(Mat)
rownames(Mat_fin)<-c("max_p","min_p","Coef(max_map)","Coef(min_p)")
write.csv(Mat_fin,"NonNut_Max&Min_p_val_&coef_final_statistics_with_US-NHANES_F15-49-nonNutrient-svyglm.csv")


### And To obtain results for FPro - only:

rownames(Ha)<-c("Anaemia~ FPro+age","Anaemia ~ FPro+age+bmi", "Anaemia ~ FPro+age+bmi+indfmpir","Anaemia ~ FPro+age+bmi+indfmpir+R1+R2+R3+R4","Anaemia ~ FPro+age+bmi+indfmpir+R1+R2+R3+R4+Var_Imperf_health","Anaemia ~ FPro+age+bmi+indfmpir+ R1 + R2 + R3 + R4+Var_Imperf_health +No_diploma_1 + Much_alco_1 +  Smoked_rec_1")

write.csv(Ha,"NonNut_FPro_p_val_&coef_final_statistics_with_US-NHANES_F15-49-nonNutrient-svyglm.csv")


