
#### USING Variables taken from file Results_from_T-score_UVE_PLS_Logistic_Regr.py

A<-read.csv("Xnumfum2_repF_with_NEG_UrTest_seqn_age_Anemia_aged_15-49_8907_with_Av_FPro_plus_11_socioeconom_and_62_SUMS_per_day_foods_vars_ends_in_7864_cases.csv", header=TRUE)
selvec<-c('ridageyr','Av_FPro','Much_alco_1','Smoked_rec_1','indfmpir','bmxbmi'
,'R1','R3','R4','T2','Carbohydrate','Theobromine','Magnesium','Zinc'
,'Vitamin.E..alpha.tocopherol.','Riboflavin','Vitamin.B.12'
,'Choline..total','X4.00','X12.00','X18.02','X18.03','X22.01'
,'Fatty.acids..total.polyunsaturated')

A<-A[,-c(1,2)]
Xsel<-A[,colnames(A) %in% selvec]
data<-cbind(A[,"B"],Xsel)
data<-cbind(data[,1],scale(data[,2:ncol(data)]))
colnames(data)[1]<-"B"
data<-as.data.frame(data)

library(stats)

model <- glm(B ~., data = data,family = binomial)

summary(model)


##### RESULTS: ####

# 1) Results for: A<-read.csv("Xnumfum2_repF_with_NEG_UrTest_seqn_age_Anemia_aged_15-49_8907_with_Av_FPro_plus_11_socioeconom_and_62_SUMS_per_day_foods_vars_ends_in_7864_cases.csv", header=TRUE)

# model <- glm(B ~., data = data,family = binomial)

# summary(model)

# Call:
# glm(formula = B ~ ., family = binomial, data = data)

# Coefficients:
#                                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -2.23654    0.04410 -50.715  < 2e-16 ***
# ridageyr                            0.26903    0.03712   7.247 4.25e-13 ***
# Av_FPro                             0.14819    0.04342   3.413 0.000643 ***
# Much_alco_1                        -0.05530    0.04181  -1.323 0.185986    
# Smoked_rec_1                       -0.23221    0.04202  -5.526 3.28e-08 ***
# indfmpir                           -0.14998    0.03980  -3.768 0.000165 ***
# bmxbmi                              0.05213    0.03578   1.457 0.145146    
# R1                                 -0.05085    0.04430  -1.148 0.251079    
# R3                                 -0.48571    0.05864  -8.283  < 2e-16 ***
# R4                                  0.41032    0.04173   9.834  < 2e-16 ***
# T2                                  0.04912    0.03950   1.244 0.213678    
# Carbohydrate                        0.06819    0.05076   1.343 0.179150    
# Theobromine                        -0.07242    0.04137  -1.751 0.080002 .  
# Magnesium                           0.10326    0.06716   1.537 0.124190    
# Zinc                               -0.17487    0.06655  -2.628 0.008595 ** 
# Vitamin.E..alpha.tocopherol.       -0.16717    0.06566  -2.546 0.010900 *  
# Riboflavin                         -0.05129    0.07223  -0.710 0.477626    
# Vitamin.B.12                       -0.05845    0.06479  -0.902 0.367047    
# Choline..total                     -0.02771    0.06284  -0.441 0.659252    
# X4.00                              -0.01059    0.04751  -0.223 0.823623    
# X12.00                              0.07413    0.03146   2.357 0.018437 *  
# X18.02                              0.36841    1.22260   0.301 0.763162    
# X18.03                              0.11974    0.14935   0.802 0.422721    
# X22.01                             -0.17538    0.06051  -2.898 0.003752 ** 
# Fatty.acids..total.polyunsaturated -0.31994    1.34392  -0.238 0.811834    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 6060.6  on 7863  degrees of freedom
# Residual deviance: 5338.3  on 7839  degrees of freedom
# AIC: 5388.3

# Number of Fisher Scoring iterations: 5




A<-read.csv("Xnumfum2_repF_with_NEG_UrTest_seqn_age_Anemia_aged_15-49_8907_with_Av_FPro_plus_8_socioeconom_and_62_SUMS_per_day_foods_vars_8907_cases.csv", header=TRUE)
selvec<-c('ridageyr','Av_FPro','indfmpir','bmxbmi','R1','R3','R4','T2','Protein'
,'Carbohydrate','Caffeine','Theobromine','Magnesium','Sodium','Zinc'
,'Riboflavin','Vitamin.B.12','Vitamin.E..added','X4.00','X12.00','X18.02'
,'X18.03','X18.04','X22.01','Fatty.acids..total.polyunsaturated')

A<-A[,-c(1,2)]
Xsel<-A[,colnames(A) %in% selvec]
data<-cbind(A[,"B"],Xsel)
data<-cbind(data[,1],scale(data[,2:ncol(data)]))
colnames(data)[1]<-"B"
data<-as.data.frame(data)

library(stats)

model <- glm(B ~., data = data,family = binomial)

summary(model)


# 2) Results for: A<-read.csv("Xnumfum2_repF_with_NEG_UrTest_seqn_age_Anemia_aged_15-49_8907_with_Av_FPro_plus_8_socioeconom_and_62_SUMS_per_day_foods_vars_8907_cases.csv", header=TRUE)

# model <- glm(B ~., data = data,family = binomial)

# summary(model)

# Call:
# glm(formula = B ~ ., family = binomial, data = data)

# Coefficients:
#                                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -2.23361    0.04085 -54.681  < 2e-16 ***
# ridageyr                            0.29442    0.03663   8.038 9.14e-16 ***
# Av_FPro                             0.12881    0.04212   3.059  0.00222 ** 
# indfmpir                           -0.09958    0.03701  -2.690  0.00714 ** 
# bmxbmi                              0.07375    0.03350   2.201  0.02771 *  
# R1                                 -0.05221    0.04388  -1.190  0.23412    
# R3                                 -0.47300    0.05614  -8.426  < 2e-16 ***
# R4                                  0.39863    0.04142   9.624  < 2e-16 ***
# T2                                  0.04572    0.03744   1.221  0.22203    
# Protein                            -0.01961    0.08079  -0.243  0.80825    
# Carbohydrate                        0.08447    0.05001   1.689  0.09118 .  
# Caffeine                           -0.07314    0.04750  -1.540  0.12356    
# Theobromine                        -0.06365    0.03871  -1.644  0.10016    
# Magnesium                           0.07265    0.05842   1.244  0.21365    
# Sodium                             -0.06630    0.06523  -1.016  0.30941    
# Zinc                               -0.15459    0.07145  -2.163  0.03050 *  
# Riboflavin                         -0.04660    0.06951  -0.670  0.50259    
# Vitamin.B.12                       -0.06656    0.06161  -1.080  0.27997    
# Vitamin.E..added                   -0.05940    0.04638  -1.281  0.20035    
# X4.00                               0.02159    0.04422   0.488  0.62544    
# X12.00                              0.07163    0.02957   2.423  0.01541 *  
# X18.02                             -0.43519    1.20401  -0.361  0.71777    
# X18.03                              0.08399    0.14499   0.579  0.56240    
# X18.04                             -0.06175    0.04477  -1.379  0.16777    
# X22.01                             -0.15365    0.05739  -2.677  0.00742 ** 
# Fatty.acids..total.polyunsaturated  0.43956    1.32568   0.332  0.74021    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 6749.1  on 8906  degrees of freedom
# Residual deviance: 6021.1  on 8881  degrees of freedom
# AIC: 6073.1

# Number of Fisher Scoring iterations: 5








A<-read.csv("Female_aged_50_anemia_Av_FPro_plus_11_socioeconom_vars_and_62_SUMS_per_day_foods_vars_9928_cases.csv", header=TRUE)
selvec<-c('ridageyr','Av_FPro','Smoked_rec_1','indfmpir','R2','R3','R4','T2'
,'Theobromine','Magnesium','Phosphorus','Selenium','Thiamin','X20.04'
,'X22.01','X22.5.n.3')


A<-A[,-c(1,2)]
Xsel<-A[,colnames(A) %in% selvec]
data<-cbind(A[,"B"],Xsel)
data<-cbind(data[,1],scale(data[,2:ncol(data)]))
colnames(data)[1]<-"B"
data<-as.data.frame(data)

library(stats)

model <- glm(B ~., data = data,family = binomial)

summary(model)


# 3) Results for: A<-read.csv("Female_aged_50_anemia_Av_FPro_plus_11_socioeconom_vars_and_62_SUMS_per_day_foods_vars_9928_cases.csv", header=TRUE)

# model <- glm(B ~., data = data,family = binomial)

# summary(model)

# Call:
# glm(formula = B ~ ., family = binomial, data = data)

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -2.36458    0.03931 -60.159  < 2e-16 ***
# ridageyr      0.24749    0.03620   6.837 8.08e-12 ***
# Av_FPro       0.10644    0.03719   2.862  0.00421 ** 
# Smoked_rec_1 -0.29716    0.04245  -7.000 2.56e-12 ***
# indfmpir     -0.15230    0.03738  -4.074 4.62e-05 ***
# R2           -0.08658    0.04300  -2.014  0.04405 *  
# R3           -0.19981    0.04990  -4.004 6.23e-05 ***
# R4            0.38205    0.03946   9.681  < 2e-16 ***
# T2            0.14679    0.04466   3.287  0.00101 ** 
# Theobromine  -0.07516    0.04138  -1.817  0.06929 .  
# Magnesium    -0.28578    0.06805  -4.200 2.67e-05 ***
# Phosphorus    0.30730    0.07614   4.036 5.44e-05 ***
# Selenium     -0.21270    0.07094  -2.998  0.00272 ** 
# Thiamin      -0.06646    0.05585  -1.190  0.23406    
# X20.04        0.08159    0.04551   1.793  0.07298 .  
# X22.01       -0.05481    0.05388  -1.017  0.30906    
# X22.5.n.3    -0.05553    0.04815  -1.153  0.24873    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 6706.9  on 9927  degrees of freedom
# Residual deviance: 6170.3  on 9911  degrees of freedom
# AIC: 6204.3

# Number of Fisher Scoring iterations: 5


















