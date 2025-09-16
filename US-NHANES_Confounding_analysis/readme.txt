Confounding variable analysis must be run in the following order:

1) Obtaining_gram-weighted_average_of_FPro_for_Participants_Av_FPro.R - This produces gram-weighted average of FPro (labeled here on this Github as 'Av_FPro') for each participant from consumed foods. IMPORTANT: In the Suppl. information it is labeled as "PW-t-E(FPro)" (Which is abbreviation for "Participant-Wise Transformed Average Estimate of FPro" - meaning the same). One obtains files: "Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro.csv" and "Female_aged_50_and_above_with_Av_FPro.csv".
'Av_FPro'

2) Obtaining_weighted_averages_per_day_of_nutrients_AFTER_obtaining_Av_FPro.R - This produces a weighted average of grams of all consumed food codes and 62-variable nutrient content of consumed food codes and outputs 62-sized vector for each participant

3) Confounding_analysis_Setting_input_datasets_code.R # This prepares input files for Non-nutrient confounder statistics and Nutrient confounder statistics. 

4) Survey_Non-nutrient_confounder_code.R, Survey_Nutrient_confounder_code.R, Cohort_Nutrient_confounder_code.R, Cohort_Non-nutrient_confounder_code.R


Using the code Code_to_create_C1C2_files_from_Driff_datasets_and_food_sets.R in main directory (i.e. "A_NA_Fpro/" ) lead to obtain C1C2 files. The files in the current directory, are simply equal-rounded splits of the whole files of the corresponding whole file to be able to be deposited to Github due to 25 MB restriction for a file. For example:

A<-read.csv("C1C2_all_values_F5_08.csv",header=TRUE)
A<-A[,-1]

A1<-A[1:round(nrow(A)/8),]
A2<-A[(1+1*round(nrow(A)/8)):(2*round(nrow(A)/8)),]
A3<-A[(1+2*round(nrow(A)/8)):(3*round(nrow(A)/8)),]
A4<-A[(1+3*round(nrow(A)/8)):(4*round(nrow(A)/8)),]
A5<-A[(1+4*round(nrow(A)/8)):(5*round(nrow(A)/8)),]
A6<-A[(1+5*round(nrow(A)/8)):(6*round(nrow(A)/8)),]
A7<-A[(1+6*round(nrow(A)/8)):(7*round(nrow(A)/8)),]
A8<-A[(1+7*round(nrow(A)/8)):nrow(A),]


write.csv(A1,"C1C2_all_values_part_1_of_8_F5_08.csv")
write.csv(A2,"C1C2_all_values_part_2_of_8_F5_08.csv")
write.csv(A3,"C1C2_all_values_part_3_of_8_F5_08.csv")
write.csv(A4,"C1C2_all_values_part_4_of_8_F5_08.csv")
write.csv(A5,"C1C2_all_values_part_5_of_8_F5_08.csv")
write.csv(A6,"C1C2_all_values_part_6_of_8_F5_08.csv")
write.csv(A7,"C1C2_all_values_part_7_of_8_F5_08.csv")
write.csv(A8,"C1C2_all_values_part_8_of_8_F5_08.csv")


