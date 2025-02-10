This gihub repository reproduces the results of the study:

"Higher degree of food processing relates to more anemic consumers"

The main reproduction code is labeled as: "MAIN_github_code_Approach_1_and_2_results_for_each_p_value.R" - this reproduces statistical differences in logit(p) (63 feature value) plus size of NA and A, and standardized (95% CI stand.) difference in means, with difference in means (63 feature value) and 95% CI error margin (additional 63 feature value) yielding a vector of 191 values for ecah case and outcome.
Other codes explain how the data was prepared starting from NHANES and FNDDS databases. However without original data, as there was no reason to put original NHANES data on github since DRIFF datasets contain far more than 25 MB (pre-pandemic DR1IFF contains more than 100 MB), and they can easily be downloaded from NHANES (e.g.years 2007-08: https://wwwn.cdc.gov/Nchs/Nhanes/search/datapage.aspx?Component=Dietary&CycleBeginYear=2007 download XPT file and turned it into CSV via:

## code ##
library(foreign)
library(Hmisc)
#Turning SAS (of NHANES) into CSV           (later used for python and R programs)
xpt = sasxport.get("DR1IFF_E.XPT")
#Processing SAS dataset FETIB_D 	 
write.csv(xpt, file="DR1IFF_E.csv")
## end of code ##

So there was no reason to download turn SAS into R and upload all such hundreds of MB, even GB-s of authentic original information from NHANES data. Of course that the code above can be looped and that dozens of files regarding demographis, nutrient and lab information can be retrieved. Our codes explain how when all necessary files are downloaded, the exact data are obtain, and the final results are obtained.

In particular: Folder Datasets contains three datasets (F15-49, F50+ and M66+) with and without participants treated for anemia (so a total of 3x2 = 6 files) and subfolder which wasn't the main part of the study due to too small number of anemic cases and/or too small fraction of anemic participants with Children aged 5-11, 12-14, and Males 15-65. the subfolder also contains a long code on how exactly all these datasets were created from the very original NHANES data (which means you need to follow instructions to download necessary files before running the code itself).
