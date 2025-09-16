After obtaining license for UK-NDNS data, one downloads NDNS_UK_2008-2023_6533tab data

One goes to UKDA-6533-tab/tab and copies all codes into that same folder.

Then, ONLY THEN, one proceeds running the following files in order:

1. Obtaining_food_code_numbers.R

2. Obtaining_food_codes_prepared_for_FPro_calculation.R

3. Run python scrypt to predict FPro of all food codes RF_to_predict_FPro_33nutr.py - see python versions in: python_versions.txt

4. Obtaining_FPro&Food_codes_assigned.R

5. Obtaining_Participant_weighted_FPro.R 

6. Obtaining_weighted_nutrients.R

7. Building_Confounder_dataset.R

8. Building_NonNut_Complex_Survey_Design_data.R

9. Survey_All-F_NonNutrient_Confounder_Results.R

10. Survey_All-F_Nutrient_Confounder_Results.R 




