After obtaining license for UK-NDNS data, one downloads NDNS_UK_2008-2023_6533tab data

One goes to UKDA-6533-tab/tab and copies all codes into that same folder.

Then, ONLY THEN, one proceeds running the following files in order:

1. source('Obtaining_food_code_numbers.R') # Run R Studio

2. source('Obtaining_food_codes_prepared_for_FPro_calculation.R') # Run R Studio

3. exec(open("RF_to_predict_FPro_33nutr.py").read()) # Run python3 scrypt, see dependencies in python_versions.txt

4. source('Obtaining_FPro&Food_codes_assigned.R') # Run R Studio

5. source('Obtaining_Participant_weighted_FPro.R') # Run R Studio

6. source('Obtaining_weighted_nutrients.R') # Run R Studio

7. source('Building_Confounder_dataset.R') # Run R Studio

8. source('Building_NonNut_Complex_Survey_Design_data.R') # Run R Studio

9. source('Survey_All-F_NonNutrient_Confounder_Results.R') # Run R Studio

10. source('Survey_All-F_Nutrient_Confounder_Results.R') # Run R Studio




