AFTER YOU HAVE DOWNLOADED KNHANES DATA FROM HERE:

# GO TO:  https://knhanes.kdca.go.kr/knhanes/eng/main.do

-> SURVEY DATA -> Go to download data - 

AND DOWNLOAD 

hn23_24rc.sas7bdat
hn22_24rc.sas7bdat
hn21_24rc.sas7bdat
hn20_24rc.sas7bdat
hn19_24rc.sas7bdat
hn18_24rc.sas7bdat
hn17_24rc.sas7bdat
hn16_24rc.sas7bdat
hn15_24rc.sas7bdat
hn14_24rc.sas7bdat
hn13_24rc.sas7bdat

hn23_all.sas7bdat
hn22_all.sas7bdat
hn21_all.sas7bdat
hn20_all.sas7bdat
hn19_all.sas7bdat
hn18_all.sas7bdat
hn17_all.sas7bdat
hn16_all.sas7bdat
hn15_all.sas7bdat
hn14_all.sas7bdat
hn13_all.sas7bdat

#Then AFTER downloading them:

# Install if not already installed
install.packages("haven")

# Now these files must be in the SAME folder as downloaded K-NHANES_Confounding_analysis from Github 

#THEN, ONLY THEN YOU CAN PROCEED EXECUTING R FILES IN THE FOLLOWING ORDER:

1. Obtaining_files_all.csv_.R

2. Obtaining_files_24rc.csv_.R

3. Obtaining_F_codes_nutr_cont_per_100g.R

4. Obtaining_42_US-NHANES_matching_nutrients_train_test_files.R

5. Obtaining_36_US-NHANES_matching_nutrients_train_test_files.R

6. Obtaining_FPro_ready_files_for_2015-2013.R

7. Obtaining_FPro_ready_files_for_2023-2016.R

8. Run PYTHON SCRYPT RF_to_predict_FPro_42nutr.py and python scrypt RF_to_predict_FPro_36nutr.py, see python package versions in python_versions.txt

9. Obtaining_FPro&Food_code_files.R

10. Obtaining_weighted_FPro_for_participants.R

11. Obtaining_weighted_nutrients_for_participants.R

12. Confounding_analysis_Setting_input_datasets_code

13. Survey_NonNutrient_confounder_code.R

14. Survey_Nutrient_confounder_code.R

15. Cohort_NonNutrient_confounder_code.R

16. Cohort_Nutrient_confounder_code.R
 




