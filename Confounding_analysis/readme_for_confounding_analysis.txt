Confounding variable analysis must be run in the following order:

1) Obtaining_gram-weighted_average_of_FPro_for_Participants_Av_FPro.R - This produces gram-weighted average of FPro (labeled here on this Github as 'Av_FPro') for each participant from consumed foods. IMPORTANT: In the Suppl. information it is labeled as "PW-t-E(FPro)" (Which is abbreviation for "Participant-Wise Transformed Average Estimate of FPro" - meaning the same).
'Av_FPro'

2) Obtaining_gram_sums_per_day_of_nutrients_AFTER_obtaining_Av_FPro.R - This produces a linear combination of grams of all consumed food codes and 62-variable nutrient content of consumed food codes and outputs 62-sized vector for each participant

3) Confounding_analysis_Part_3_code.R # This reproduces results for Part 3 of the Supplementary information in the study "Food Processing Score Is Associated with Anaemia Among Non-Pregnant Women of NHANES". For Part 4, we continue further

4) T-score_UVE_PLS_Logistic_Regr.py - This is a in-depth coded combination of UVE-PLS and Logistic regression which uses T-scores of significant latent variables for regressing against target variable (which is anemia status). The result outputs significant variables which remained not-eliminated due to their higher stabilities than optimal threshold for variable elimination used for the last final regression. This regression, actually itself also provides statistical measures of accuracy, but also variable stabilities as mean values of regression coefficients divided by standard deviation when using 30-fold cross-validation for each original variable. This result, by itself is actually a proof of significant Av_FPro, because of its higher stability than 3 (p<0.001) (actually equallying roughly 14) combined with relatively high regression coefficient for F-Pro in the final model when predicting anemia status. But, it is not 100% certain that the broad readership would entirely follow the meaning of such obtained stability for regression coefficient of Av_FPro. For that reason, the selected significant variables from T-score_UVE_PLS_Logistic_Regr.py - were simply submitted to final Binomial regression (last step - 5) 

5) FINAL_code_for_General_Confounding_variable_analysis_Part_4_and_its_results.R - Both code and its results (with #) are in the file confirming statistical significance of FPro when adjusted to socio-economic variables. 


Confounder_var_set.csv is obtained in A_NA_Fpro/Datasets using the program Obtaining_confounder_var_set.R after the program 



