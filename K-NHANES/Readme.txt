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

#THEN, ONLY THEN YOU CAN PROCEED EXECUTING R and PYTHON3 FILES IN THE FOLLOWING ORDER:

1. source('Obtaining_files_all.csv_.R') # Run R Studio

2. source('Obtaining_files_24rc.csv_.R') # Run R Studio

3. source('Obtaining_F_codes_nutr_cont_per_100g.R') # Run R Studio

4. source('Obtaining_42_US-NHANES_matching_nutrients_train_test_files.R') # Run R Studio

5. source('Obtaining_36_US-NHANES_matching_nutrients_train_test_files.R') # Run R Studio

6. source('Obtaining_FPro_ready_files_for_2015-2013.R') # Run R Studio

7. source('Obtaining_FPro_ready_files_for_2023-2016.R') # Run R Studio

8. exec(open("RF_to_predict_FPro_42nutr.py").read()) # Run Python3 scrypt, see python package versions in python_versions.txt

9. exec(open("RF_to_predict_FPro_36nutr.py").read()) # Run Python3 scrypt, see python package versions in python_versions.txt

10. source('Obtaining_FPro&Food_code_files.R') # Run R Studio

11. source('Obtaining_weighted_FPro_for_participants.R') # Run R Studio

12. source('Obtaining_weighted_nutrients_for_participants.R') # Run R Studio

13. source('Confounding_analysis_Setting_input_datasets_code.R') # Run R Studio

14. source('Survey_NonNutrient_confounder_code.R') # Run R Studio

15. source('Survey_Nutrient_confounder_code.R') # Run R Studio

16. source('Cohort_NonNutrient_confounder_code.R') # Run R Studio

17. source('Cohort_Nutrient_confounder_code.R') # Run R Studio
 




