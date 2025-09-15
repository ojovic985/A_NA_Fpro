
library(survey)

data<-read.csv("Complex_survey_design_data_NonNut.csv",header=TRUE)
data<-data[,-1]

library(survey)

ndns_design<-svydesign(id=~area,strata=~astrata1,weights=~Comb_weights,data=data,nest=TRUE)

check_strata_validity<-function(strata_var) {
design<-svydesign(
id = ~ area,
strata = as.formula(paste0("~",strata_var)),weights=~ Comb_weights,data=data,nest=TRUE)

n_strata<-length(unique(data[[strata_var]]))
psu_per_stratum<-table(data[[strata_var]],data$area)
strata_with_1_psu<-sum(rowSums(psu_per_stratum>0)==1)

list( strata=strata_var, single_psu_strata=strata_with_1_psu, total_strata=n_strata, design=design) }

strata_results<-lapply(paste0("astrata",1:5),check_strata_validity)

sapply(strata_results, function(x) x$single_psu_strata)


strata_vars<-paste0("astrata",1:5)
check_strata_validity<-function(strata_var) {
psu_per_stratum<-table(data[[strata_var]],data$area)
strata_with_1_psu<-sum(rowSums(psu_per_stratum>0)==1)
return(strata_with_1_psu) }

single_psu_counts<-sapply(strata_vars, check_strata_validity)
best_strata<-strata_vars[which.min(single_psu_counts)]
cat("Using:",best_strata,"\n")

strata_formula=as.formula(paste0("~",best_strata))

ndns_design<-svydesign(id=~area, strata=strata_formula,weights=~ Comb_weights,data=data,nest=TRUE)

#ndns_design<-svydesign(id=~area, strata=~astrata4,weights=~ Comb_weights,data=data,nest=TRUE)

A1<-read.csv("ID_and_Weighted_Nutrients_for_ndns_rp_yr1-4a.csv",header=TRUE)
A1<-A1[,-1]
A2<-read.csv("ID_and_Weighted_Nutrients_for_ndns_rp_yr5-6a.csv",header=TRUE)
A2<-A2[,-1]
A3<-read.csv("ID_and_Weighted_Nutrients_for_ndns_rp_yr7-8a.csv",header=TRUE)
A3<-A3[,-1]
A4<-read.csv("ID_and_Weighted_Nutrients_for_ndns_rp_yr9a.csv",header=TRUE)
A4<-A4[,-1]
A5<-read.csv("ID_and_Weighted_Nutrients_for_ndns_rp_yr10a.csv",header=TRUE)
A5<-A5[,-1]
A6<-read.csv("ID_and_Weighted_Nutrients_for_ndns_rp_yr11a.csv",header=TRUE)
A6<-A6[,-1]

Auk<-rbind(A1,A2,A3,A4,A5,A6)

data2<-merge(data,Auk,by="seriali")

data3<-data[!colnames(data) %in% c("FPro")]

data2_2<-data2[,which(colnames(data2)=="FPro"):which(colnames(data2)=="Transfattyacidsg")]

data2_3<-data2_2[!colnames(data2_2) %in% colnames(data3)]


Mat<-cbind(data2_3[,2:ncol(data2_3)], data2_3[,1])
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
ndns_design<-svydesign(id=~area, strata=strata_formula,weights=~ Comb_weights,data=data4,nest=TRUE)
fifth_var<-colnames(data3_2)[ncol(data3_2)]
last_col <- colnames(data4)[ncol(data4)]
formula_str <- paste("anaemia~ age+bmival + ethgr2 +",fifth_var,"+ degree + Imperf_health +", last_col)
model_formula <- as.formula(formula_str)
model <- svyglm(model_formula, design = ndns_design, family = quasibinomial())
Matfull2<-rbind(Matfull2,summary(model)$coefficients[5,])
}
Matu[1,j]<-max(Matfull2[,4])
Matu[2,j]<-min(Matfull2[,4])
Matu_coef[1,j]<-Matfull2[which.max(Matfull2[,4]),1]
Matu_coef[2,j]<-Matfull2[which.min(Matfull2[,4]),1]
Conf_vector<-cbind(Conf_vector,colnames(Mat1)[which.max(Matfull2[,4])])}

Matu_2<-rbind(Matu, Matu_coef,Conf_vector)

Mat_final<-as.data.frame(Matu_2)

colnames(Mat_final)<-colnames(Mat)
rownames(Mat_final)<-c("max_p","min_p","Coef(max_map)","Coef(min_p)","Nut_top_conf")

write.csv(Mat_final,"Max&Min_p_val_&coef_final_statistics_with_UK-NDNS_AllF-Nutrient-svyglm_max_obs_confs.csv")

# FPro only results:

rownames(Matfull2)<-colnames(Mat1)
write.csv(Matfull2,"FPro_p_val_&coef_final_statistics_with_UK-NDNS_AllF-Nutrient-svyglm_max_obs_confs.csv")








