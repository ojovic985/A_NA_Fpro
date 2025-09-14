###### For All-F statistics #### 

A1<-read.csv("Female_aged_15-49_with_NEG_UrTest_with_Av_FPro_plus_11_socioeconom_vars_and_62_AVER_foods_vars_7864_cases.csv",header=TRUE)
A1<-A1[,-1]

A2<-read.csv("Female_aged_50_anemia_Av_FPro_plus_11_socioeconom_vars_and_62_AVER_foods_vars_9928_cases.csv",header=TRUE)
A2<-A2[,-1]

A<-rbind(A1,A2)

##For svyglm
#M<-read.csv("Series_C-P_sdmvpsu_stra_Adj_wtmec2yr.csv",header=TRUE)
#M<-M[,-1]
#F<-merge(A,M,by="seqn")
## For svyglm

F<-A # for glm
data1<-F[,which(colnames(F)=="ridageyr"):(which(colnames(F)=="Protein")-1)]

### rows for svyglm
#data2<-F[,which(colnames(F)=="sdmvpsu"):which(colnames(F)=="wtmec2yr")]
#data3<-cbind(data1,data2)
#data3<-data3[!colnames(data3) %in% c("Av_FPro")]
#### rows for svyglm

data3<-data1[!colnames(data1) %in% c("Av_FPro")] # row for GLM

Mat<-F[,which(colnames(F)=="Protein"):which(colnames(F)=="Fatty.acids..total.polyunsaturated")]
Mat<-cbind(Mat,F[,"Av_FPro"])
colnames(Mat)[ncol(Mat)]<-"FPro"

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
#design <- svydesign(id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr, data = data4, nest = TRUE) # row for svyglm
fifth_var<-colnames(data3_2)[ncol(data3_2)]
last_col <- colnames(data4)[ncol(data4)]
formula_str <- paste("B~ridageyr + indfmpir + bmxbmi+",fifth_var,"+ R1 + R2 + R3 + R4 + T2 + No_diploma_1 + Much_alco_1 + Smoked_rec_1 +", last_col)
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
write.csv(Mat_final,"Max&Min_p_val_&coef_final_statistics_with_US-NHANES_AllF-Nutrient-glm.csv")

### And To obtain results for FPro - only:

rownames(Matfull2)<-colnames(Mat1)
write.csv(Matfull2,"FPro_p_val_&coef_final_statistics_with_US-NHANES_AllF-Nutrient-glm.csv")



###### For F15-49 statistics #### 

A1<-read.csv("Female_aged_15-49_with_NEG_UrTest_with_Av_FPro_plus_11_socioeconom_vars_and_62_AVER_foods_vars_7864_cases.csv",header=TRUE)
A1<-A1[,-1]

A<-A1

F<-A # for glm
data1<-F[,which(colnames(F)=="ridageyr"):(which(colnames(F)=="Protein")-1)]

### rows for svyglm
#data2<-F[,which(colnames(F)=="sdmvpsu"):which(colnames(F)=="wtmec2yr")]
#data3<-cbind(data1,data2)
#data3<-data3[!colnames(data3) %in% c("Av_FPro")]
#### rows for svyglm

data3<-data1[!colnames(data1) %in% c("Av_FPro")] # row for GLM

Mat<-F[,which(colnames(F)=="Protein"):which(colnames(F)=="Fatty.acids..total.polyunsaturated")]
Mat<-cbind(Mat,F[,"Av_FPro"])
colnames(Mat)[ncol(Mat)]<-"FPro"

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
#design <- svydesign(id = ~sdmvpsu, strata = ~sdmvstra, weights = ~wtmec2yr, data = data4, nest = TRUE) # row for svyglm
fifth_var<-colnames(data3_2)[ncol(data3_2)]
last_col <- colnames(data4)[ncol(data4)]
formula_str <- paste("B~ridageyr + indfmpir + bmxbmi+",fifth_var,"+ R1 + R2 + R3 + R4 + T2 + No_diploma_1 + Much_alco_1 + Smoked_rec_1 +", last_col)
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

write.csv(Mat_final,"Max&Min_p_val_&coef_final_statistics_with_US-NHANES_F15-49-Nutrient-glm.csv")

### And To obtain results for FPro - only:

rownames(Matfull2)<-colnames(Mat1)

write.csv(Matfull2,"FPro_p_val_&coef_final_statistics_with_US-NHANES_F15-49-Nutrient-glm.csv")


