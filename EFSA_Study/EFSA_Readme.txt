#EFSA analysis must be run in the following order:

1) source('Preparing_Food_Nutr_for_FPro_calc.R') # run after oening R Studio

2) source('Obtaining_15_US-NHANES_matching_nutrients.R') # run after oening R Studio

3) exec(open("RF_to_predict_FPro_15nutr.py").read()) # run after oening python3, see requirements for Python in python_versions.txt

4) source('Merging_FPro_and_Food.R') # run after oening R Studio

5) source('Confounding_data_preparation_set_code.R') # run after oening R Studio

6) source('Plotting_four_subfigures_A-D_.R') # run after oening R Studio

7) exec(open("Merging_four_figures.py").read()) # run after oening python3, see requirements for Python in python_versions.txt


#Info on how some data were pre-prepared from original sources and how they got here (i.e. these codes below ARE NOT SUPPOSED TO BE RUN ):

#Using the code to put originally downloaded file "fsd-full-export-2025-07-15.csv" into files <25 MB:

A<-read.csv("fsd-full-export-2025-07-15.csv",header=TRUE)

A1<-A[1:round(nrow(A)/4),]
A2<-A[(1+1*round(nrow(A)/4)):round(nrow(A)/2),]
A3<-A[(1+round(nrow(A)/2)):(1*round(nrow(A)/4)+round(nrow(A)/2)),]
A4<-A[(1+(1*round(nrow(A)/4)+round(nrow(A)/2))):nrow(A),]

A5=rbind(A1,A2,A3,A4)
all.equal(A,A5)
#[1] TRUE


write.csv(A1,"fsd-full-export-2025-07-15_part_1.csv")
write.csv(A2,"fsd-full-export-2025-07-15_part_2.csv")
write.csv(A3,"fsd-full-export-2025-07-15_part_3.csv")
write.csv(A4,"fsd-full-export-2025-07-15_part_4.csv")

#The similar was done for "Foodex 2 L7 dashboard.csv":

A <- read.csv("Foodex 2 L7 dashboard.csv", header = TRUE, fileEncoding = "UTF-16")

A1<-A[1:round(nrow(A)/8),]
A2<-A[(1+1*round(nrow(A)/8)):(2*round(nrow(A)/8)),]
A3<-A[(1+2*round(nrow(A)/8)):(3*round(nrow(A)/8)),]
A4<-A[(1+3*round(nrow(A)/8)):(4*round(nrow(A)/8)),]
A5<-A[(1+4*round(nrow(A)/8)):(5*round(nrow(A)/8)),]
A6<-A[(1+5*round(nrow(A)/8)):(6*round(nrow(A)/8)),]
A7<-A[(1+6*round(nrow(A)/8)):(7*round(nrow(A)/8)),]
A8<-A[(1+7*round(nrow(A)/8)):nrow(A),]

write.csv(A1,"Foodex_2_L7_dashboard_part_1.csv")
write.csv(A2,"Foodex_2_L7_dashboard_part_2.csv")
write.csv(A3,"Foodex_2_L7_dashboard_part_3.csv")
write.csv(A4,"Foodex_2_L7_dashboard_part_4.csv")
write.csv(A5,"Foodex_2_L7_dashboard_part_5.csv")
write.csv(A6,"Foodex_2_L7_dashboard_part_6.csv")
write.csv(A7,"Foodex_2_L7_dashboard_part_7.csv")
write.csv(A8,"Foodex_2_L7_dashboard_part_8.csv")

A9=rbind(A1,A2,A3,A4,A5,A6,A7,A8)
all.equal(A,A9)
#[1] TRUE


#The similar was done for "BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country.csv":

A<-read.csv("BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country.csv",header=TRUE)

A1<-A[1:round(nrow(A)/3),]
A2<-A[(1+1*round(nrow(A)/3)):(2*round(nrow(A)/3)),]
A3<-A[(1+2*round(nrow(A)/3)):nrow(A),]

A4=rbind(A1,A2,A3)
all.equal(A,A4)
#[1] TRUE

write.csv(A1,"BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_1.csv")
write.csv(A2,"BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_2.csv")
write.csv(A3,"BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_3.csv")

Q1<-read.csv("BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_1.csv",header=TRUE)
Q2<-read.csv("BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_1.csv",header=TRUE)
Q3<-read.csv("BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_1.csv",header=TRUE)

Q<-rbind(Q1,Q2,Q3)
Q<-Q[,-1]

