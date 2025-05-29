This gihub repository reproduces the results of the study:

"Food Processing Score Is Associated with Anaemia Among Non-Pregnant Women of NHANES"

The main reproduction code is labeled as: "MAIN_github_code_Approach_1_2_and_3_results_for_each_p_value_3.R" - this reproduces 1) Cohen's d statistic, 2) Standardized differences in means at 95% confidence, and 3) logit(p) for 63 variables (62 nutrients and FPro) as statistical differences between Non-A and Food subsets plus size of Non-A and A Food subsets (2 next values), a vector of total 191 values for each outcome defined by 24 study groups and 8 thresholds. I.e. total matrix of 192x191, but split into 8 excel sheets (denoting 8 thresholds of t-test).

The next most important is Confounding variable analysis for which a separate folder "Confounding analysis is dedicated"

Other codes explain how the data was prepared starting from NHANES and FNDDS databases. However without original data, as there was no reason to put original NHANES data on github since DRIFF datasets contain far more than 25 MB (pre-pandemic DR1IFF contains more than 100 MB), and they can easily be downloaded from NHANES (e.g.years 2007-08: https://wwwn.cdc.gov/Nchs/Nhanes/search/datapage.aspx?Component=Dietary&CycleBeginYear=2007 download XPT file and turned it into CSV via:

## code ##
library(foreign)
library(Hmisc)
#Turning SAS (of NHANES) into CSV           (later used for python and R programs)
xpt = sasxport.get("DR1IFF_E.XPT")
#Processing SAS dataset FETIB_D 	 
write.csv(xpt, file="DR1IFF_E.csv")
## end of code ##

So there was no reason to download turn SAS into R and upload all such hundreds of MB, even GB-s of authentic original information from NHANES data. Of course that the code above can be looped and that dozens of files regarding demographis, nutrient and lab information can be retrieved. Our codes explain how when all necessary files are downloaded, the exact data are obtained, and the final results are obtained.

In particular: Folder Datasets contains three datasets (F15-49, F50+) with and without participants treated for anemia (PTFA =1 or 2, so a total of 2x2 = 4 files). We actually considered other dataset but they were not part of the study due to too small number of anemic cases and/or too small fraction of anemic participants with Children aged 5-11, 12-14, and Males 15-65. The same folder (Datasets) also contains a long code on how exactly two datasets (F15-49 and F50+) were created from the very original NHANES data (which means you need to follow instructions to download necessary files before running the code itself).

Codes calculation of Fpro.R is followed by ML_prod.py. these two codes depict how output files containing 63 features in the folder "FNDDS_Fpro_calc_prepared" were obtained with example of 2007-2008 year. In brief, in that part we followed by reference "Machine learning prediction of the degree of food processing" of G. Menichetti et al. ( doi: 10.1038/s41467-023-37457-1 ) and their github repository to create predictions of Fpro for years 2003-2020. from these nine files in folder "FNDDS_Fpro_calc_prepared" we obtained Data_3099_food_codes_g_per_100g.csv by launching the code "Creating_Data3099.R" in directory "FNDDS_Fpro_calc_prepared" - they represent all FNDDS foods that were present in all the studied nine NHANES years 2003-2020, with their median values (i.e. median of total nine values) for 62 nutrients and Fpro. In the similar way we obtained Data_9450_food_codes_g_per_100g.csv by launching the code Creating_Data9450.R in directory "FNDDS_Fpro_calc_prepared" - they represent all FNDDS foods that were present at least three times in the studied nine NHANES years 2003-2020 with their median values for features.

"C1C2_" files represent Participant vs. Food code data. For each participant obtained from corresponding dataset (from directory "Datasets") values for all food codes were obtained depending on the grouping criteria used ( (1) total grams consumed by participant on both interview days,  (2) number of in interview days (0, or 1 or 2) of that food code being consumed by the given participant, and (3) total grams consumed by participant on both interview days divided by his/her body weight). The analysis also depends on the smaller food code dataset (Data3099) or larger food code dataset (Data9450) being used, so they are different in sizes. So, for two datasets (F15-49 (F1 -labeled) , F50+ (F5 - labeled)), three grouping criteria, two different sizes of data and whether participants being treated for anemia or not are also accounted (mcq053 in NHANES, or "PTFA" in article) - there are a total of 24 different C1C2 files, or 12 per dataset. These are all depicted in "Case_1-24_ie_F1_1-12_F5_1-12_description.csv".  The code how they were all obtained is in "Code_to_create_C1C2_files_from_Driff_datasets_and_food_sets.R" Read the that code file, do NOT just run it, because some parts of the codes must be repeated for different NHANES years - that is well commented via '#' in the file. "C1C2_" files contain only columns with at least 30 non-zero values, as they are all labeled as such, otherwise they would be larger than 25 MB. One of these C1C2 isn't larger than 25 MB, and we provide it also as "C1C2_M1_04.csv". We provide it only as an example how originally produced C1C2 files look like, otherwise that specific file isn't used by the program, neither its derivative "C1C2_30_or_more_non-zero_values_M1_04.csv", but this is just as an example. The other example use 

The final results are provided in Supplementary_table_results_main.xlsx (there are also given labels as in "Case_1-36_ie_F1_1-12_F5_1-12_M1_1-12_description.csv"). Results obtained when the Main code is run for each of p-values (i.e. 0.025, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8) are all summarized in that file Supplementary_table_results_main.xlsx file (we added p=0.9, just to show that too few A pool cases are then obtained (X65-column N(A food codes))). 

For reproducing Figure 1, please run the code Producing_Figure_1.R. For reproducing Figure 2. Please run code Producing_Figure_2.R in subdirectory "For_producing_figure_2".


