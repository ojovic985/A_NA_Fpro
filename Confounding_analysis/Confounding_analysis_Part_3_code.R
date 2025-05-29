
Mer2<-function(X1,X2){
X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {   
if (is.na(match(X1["seqn"][i,1],X2["seqn"][,1]))==FALSE) {
I<-cbind(I,i)   
J<-cbind(J,match(X1["seqn"][i,1],X2["seqn"][,1]))}
}
X1X2<-cbind(X1[I,],X2[J,])
return(X1X2)}

B<-read.csv("Confounder_var_set.csv",header=TRUE) ### Dataset produced in     A_NA_Fpro /Datasets/
B<-B[,-1]
B<-B[,-2]

A<-read.csv("Female_aged_50_and_above_with_Av_FPro.csv",header=TRUE) ### Dataset produced in Obtaining_gram-weighted_average_of_FPro_for_Participants_Av_FPro
A<-A[,-1]

E<-Mer2(A,B)
F<-E[,-c(5,6,13)]

colnames(F)[7]<-"R1"
colnames(F)[8]<-"R2"
colnames(F)[9]<-"R3"
colnames(F)[10]<-"R4"
colnames(F)[11]<-"T2"

D2<-E[,-c(5:12)]
F3<-D2[,-6]
colnames(F3)[5]<-"R5"


data<-F[,-1]
data3<-F3[,-1]




library(stats)

model <- glm(B ~ bmxbmi, data = data,family = binomial)

summary(model)

Teh_B<-summary(model)$coefficients[2,] #B~bmxbmi

#> summary(model)$coefficients[2,]
#   Estimate   Std. Error      z value     Pr(>|z|) 
#2.655552e-02 4.014385e-03 6.615091e+00 3.713239e-11 

model <- glm( Av_FPro ~ bmxbmi, data = data)

summary(model)

Teh_FPro<-summary(model)$coefficients[2,]

#> summary(model)$coefficients[2,]
#    Estimate   Std. Error      t value     Pr(>|t|) 
#2.233038e-03 2.025151e-04 1.102653e+01 4.143073e-28 


model <- glm(B ~ Av_FPro, data = data,family = binomial)

Main<-summary(model)$coefficients

#> Main
#             Estimate Std. Error    z value     Pr(>|z|)
#(Intercept) -2.845740  0.1431340 -19.881649 5.867302e-88
#Av_FPro      1.142434  0.2200404   5.191928 2.081275e-07


model <- glm(B ~ ridageyr, data = data,family = binomial)

Teh_B<-rbind(Teh_B,summary(model)$coefficients[2,])

rownames(Teh_B)<-c("A~bmxbmi","A~age")


model <- glm(Av_FPro ~ ridageyr, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)<-c("FPro~bmxbmi","FPro~age")


model <- glm(B ~ indfmpir, data = data,family = binomial)


Teh_B<-rbind(Teh_B,summary(model)$coefficients[2,])

rownames(Teh_B)[3]<-c("A~indfmpir")

model <- glm(Av_FPro ~ indfmpir, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[3]<-c("FPro~indfmpir")

model <- glm(Av_FPro ~ R1, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[4]<-c("FPro~R1")

model <- glm(Av_FPro ~ R2, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[5]<-c("FPro~R2")

model <- glm(Av_FPro ~ R3, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[6]<-c("FPro~R3")

model <- glm(Av_FPro ~ R4, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[7]<-c("FPro~R4")

model <- glm(Av_FPro ~ R5, data = data3)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[8]<-c("FPro~R5")

model <- glm(Av_FPro ~ T2, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[9]<-c("FPro~Non-Excl_Heal")

model <- glm(B ~ Av_FPro + bmxbmi, data = data,family = binomial)

Teh_FPro_2<-summary(model)$coefficients[2,]

model <- glm(B ~ Av_FPro + ridageyr, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[1]<-c("FPro_in_A~FPro+bmxbmi")
rownames(Teh_FPro_2)[2]<-c("FPro_in_A~FPro+age")

model <- glm(B ~ Av_FPro + indfmpir, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[3]<-c("FPro_in_A~FPro+indfmpir")

model <- glm(B ~ Av_FPro + R1, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[4]<-c("FPro_in_A~FPro+R1")

model <- glm(B ~ Av_FPro + R2, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[5]<-c("FPro_in_A~FPro+R2")

model <- glm(B ~ Av_FPro + R3, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[6]<-c("FPro_in_A~FPro+R3")

model <- glm(B ~ Av_FPro + R4, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[7]<-c("FPro_in_A~FPro+R4")

model <- glm(B ~ Av_FPro + R5, data = data3,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[8]<-c("FPro_in_A~FPro+R5")

model <- glm(B ~ Av_FPro + T2, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[9]<-c("FPro_in_A~FPro+Non-Excl_Heal")

model_full <- glm(B ~ Av_FPro + ridageyr + indfmpir + bmxbmi + R1 + R2 + R3 + R4 + T2, family = binomial, data = data)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model_full)$coefficients[2,])

rownames(Teh_FPro_2)[10]<-c("FPro_in_A~. (all conf.)")

Mat_full<-rbind(Main[2,],Teh_B,Teh_FPro,Teh_FPro_2)

rownames(Mat_full)[1]<-"A~FPro"

H<-read.csv("Education_Alcohol_Smoking_C-P.csv",header=TRUE)
H<-H[,-1]
H1<-Mer2(A,H)

H1<-H1[,-c(2,3,4,5)]

EE<-Mer2(F,H1)
F<-EE[,-12]

data<-F[,-1]

library(stats)

model_full <- glm(B ~ Av_FPro + ridageyr + indfmpir + bmxbmi + R1 + R2 + R3 + R4 + T2+No_diploma_1+Much_alco_1+Smoked_rec_1, family = binomial, data = data)

Mat_full<-rbind(Mat_full,summary(model_full)$coefficients[2,])

rownames(Mat_full)[nrow(Mat_full)]<-c("FPro_in_A~. (all conf. +3*)")

Mat_full


### OUTPUT - has to be reproduced as below

#                                 Estimate   Std. Error   z value     Pr(>|z|)
# A~FPro                        1.142434026 0.2200404217  5.191928 2.081275e-07
# A~bmxbmi                      0.026555520 0.0040143849  6.615091 3.713239e-11
# A~age                         0.021998008 0.0035766503  6.150450 7.726354e-10
# A~indfmpir                   -0.163549927 0.0225946162 -7.238447 4.538523e-13
# FPro~bmxbmi                   0.002233038 0.0002025151 11.026528 4.143073e-28
# FPro~age                     -0.001483447 0.0001668928 -8.888622 7.263081e-19
# FPro~indfmpir                -0.005814717 0.0009941138 -5.849146 5.097172e-09
# FPro~R1                      -0.008024009 0.0045045963 -1.781294 7.489504e-02
# FPro~R2                      -0.026513689 0.0051017243 -5.197005 2.065495e-07
# FPro~R3                      -0.018921790 0.0030342836 -6.235999 4.671878e-10
# FPro~R4                       0.053181484 0.0036040260 14.756132 9.214234e-49
# FPro~R5                      -0.016809821 0.0055355451 -3.036706 2.397996e-03
# FPro~Non-Excl_Heal            0.036252383 0.0049760602  7.285359 3.452305e-13
# FPro_in_A~FPro+bmxbmi         0.994074670 0.2210466796  4.497126 6.887832e-06
# FPro_in_A~FPro+age            1.288926338 0.2226193044  5.789823 7.046069e-09
# FPro_in_A~FPro+indfmpir       1.045105052 0.2195484398  4.760248 1.933551e-06
# FPro_in_A~FPro+R1             1.140655419 0.2200581497  5.183427 2.178452e-07
# FPro_in_A~FPro+R2             1.108104628 0.2203710039  5.028359 4.946938e-07
# FPro_in_A~FPro+R3             1.007950757 0.2223979423  4.532195 5.837403e-06
# FPro_in_A~FPro+R4             0.597476289 0.2251951019  2.653150 7.974452e-03
# FPro_in_A~FPro+R5             1.127840659 0.2199276106  5.128236 2.924699e-07
# FPro_in_A~FPro+Non-Excl_Heal  1.055569482 0.2203758866  4.789859 1.668983e-06
# FPro_in_A~. (all conf.)       0.643038143 0.2295484948  2.801317 5.089447e-03
# FPro_in_A~. (all conf. +3*)   0.787779649 0.2323691787  3.390207 6.983983e-04


write.csv(F,"Female_aged_50_anemia_Av_FPro_plus_11_socioeconom_vars.csv")

F<-read.csv("Female_aged_50_anemia_Av_FPro_plus_11_socioeconom_vars.csv",header=TRUE)
F<-F[,-1]

HG<-read.csv("Female_aged_50_and_above_with_Av_FPro_and_SUMS_per_day_of_62_consumed_nutrients_under_1888_removed-codes-terms.csv",header=TRUE) ### Dataset produced in Obtaining_gram_sums_per_day_of_nutrients_AFTER_obtaining_Av_FPro

TR<-Mer2(F,HG)
TR2<-TR[,-c(15:19)]

write.csv(TR2,"Female_aged_50_anemia_Av_FPro_plus_11_socioeconom_vars_and_62_SUMS_per_day_foods_vars_9928_cases.csv")  ####################### MARK for T-score_UVE_PLS_Logistic_Regr



##### Beginning with F15-49 dataset


B<-read.csv("Confounder_var_set.csv",header=TRUE)  ### Dataset produced in A_NA_Fpro /Datasets/
B<-B[,-1]
B<-B[,-2]

A<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro.csv",header=TRUE) ### Dataset produced in Obtaining_gram-weighted_average_of_FPro_for_Participants_Av_FPro
A<-A[,-1]

E<-Mer2(A,B)
F<-E[,-c(5,6,13)]

colnames(F)[7]<-"R1"
colnames(F)[8]<-"R2"
colnames(F)[9]<-"R3"
colnames(F)[10]<-"R4"
colnames(F)[11]<-"T2"

D2<-E[,-c(5:12)]
F3<-D2[,-6]
colnames(F3)[5]<-"R5"


data<-F[,-1]
data3<-F3[,-1]




library(stats)

model <- glm(B ~ bmxbmi, data = data,family = binomial)

summary(model)

Teh_B<-summary(model)$coefficients[2,] #B~bmxbmi


model <- glm( Av_FPro ~ bmxbmi, data = data)

summary(model)

Teh_FPro<-summary(model)$coefficients[2,]
 


model <- glm(B ~ Av_FPro, data = data,family = binomial)

Main<-summary(model)$coefficients


model <- glm(B ~ ridageyr, data = data,family = binomial)

Teh_B<-rbind(Teh_B,summary(model)$coefficients[2,])

rownames(Teh_B)<-c("A~bmxbmi","A~age")


model <- glm(Av_FPro ~ ridageyr, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)<-c("FPro~bmxbmi","FPro~age")


model <- glm(B ~ indfmpir, data = data,family = binomial)


Teh_B<-rbind(Teh_B,summary(model)$coefficients[2,])

rownames(Teh_B)[3]<-c("A~indfmpir")

model <- glm(Av_FPro ~ indfmpir, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[3]<-c("FPro~indfmpir")

model <- glm(Av_FPro ~ R1, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[4]<-c("FPro~R1")

model <- glm(Av_FPro ~ R2, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[5]<-c("FPro~R2")

model <- glm(Av_FPro ~ R3, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[6]<-c("FPro~R3")

model <- glm(Av_FPro ~ R4, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[7]<-c("FPro~R4")

model <- glm(Av_FPro ~ R5, data = data3)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[8]<-c("FPro~R5")

model <- glm(Av_FPro ~ T2, data = data)

Teh_FPro<-rbind(Teh_FPro,summary(model)$coefficients[2,])

rownames(Teh_FPro)[9]<-c("FPro~Non-Excl_Heal")

model <- glm(B ~ Av_FPro + bmxbmi, data = data,family = binomial)

Teh_FPro_2<-summary(model)$coefficients[2,]

model <- glm(B ~ Av_FPro + ridageyr, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[1]<-c("FPro_in_A~FPro+bmxbmi")
rownames(Teh_FPro_2)[2]<-c("FPro_in_A~FPro+age")

model <- glm(B ~ Av_FPro + indfmpir, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[3]<-c("FPro_in_A~FPro+indfmpir")

model <- glm(B ~ Av_FPro + R1, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[4]<-c("FPro_in_A~FPro+R1")

model <- glm(B ~ Av_FPro + R2, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[5]<-c("FPro_in_A~FPro+R2")

model <- glm(B ~ Av_FPro + R3, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[6]<-c("FPro_in_A~FPro+R3")

model <- glm(B ~ Av_FPro + R4, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[7]<-c("FPro_in_A~FPro+R4")

model <- glm(B ~ Av_FPro + R5, data = data3,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[8]<-c("FPro_in_A~FPro+R5")

model <- glm(B ~ Av_FPro + T2, data = data,family = binomial)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model)$coefficients[2,])

rownames(Teh_FPro_2)[9]<-c("FPro_in_A~FPro+Non-Excl_Heal")

model_full <- glm(B ~ Av_FPro + ridageyr + indfmpir + bmxbmi + R1 + R2 + R3 + R4 + T2, family = binomial, data = data)

Teh_FPro_2<-rbind(Teh_FPro_2,summary(model_full)$coefficients[2,])

rownames(Teh_FPro_2)[10]<-c("FPro_in_A~. (all conf.)")

Mat_full<-rbind(Main[2,],Teh_B,Teh_FPro,Teh_FPro_2)

rownames(Mat_full)[1]<-"A~FPro"

write.csv(F,"Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro_plus_8_socioeconom_vars.csv")

A<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro_plus_8_socioeconom_vars.csv",header=TRUE)
A<-A[,-1]

B<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro_and_SUMS_per_day_of_62_consumed_nutrients_under_1888_removed-codes-terms.csv",header=TRUE) ### Dataset produced in Obtaining_gram_sums_per_day_of_nutrients_AFTER_obtaining_Av_FPro
B<-B[,-1]

C<-Mer2(A,B)
C1<-C[,-c(12:15)]

write.csv(C1,"Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro_plus_8_socioeconom_and_62_SUMS_per_day_foods_vars_8907_cases.csv")  ################# MARK for T-score_UVE_PLS_Logistic_Regr


H<-read.csv("Education_Alcohol_Smoking_C-P.csv",header=TRUE)
H<-H[,-1]
H1<-Mer2(A,H)

H1<-H1[,-c(2,3,4,5)]

EE<-Mer2(F,H1)
F<-EE[,-12]

data<-F[,-1]

library(stats)

model_full <- glm(B ~ Av_FPro + ridageyr + indfmpir + bmxbmi + R1 + R2 + R3 + R4 + T2+No_diploma_1+Much_alco_1+Smoked_rec_1, family = binomial, data = data)

Mat_full<-rbind(Mat_full,summary(model_full)$coefficients[2,])

rownames(Mat_full)[nrow(Mat_full)]<-c("FPro_in_A~. (all conf. +3*)")

Mat_full


HG<-read.csv("Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro_and_SUMS_per_day_of_62_consumed_nutrients_under_1888_removed-codes-terms.csv",header=TRUE) ### Dataset produced in Obtaining_gram_sums_per_day_of_nutrients_AFTER_obtaining_Av_FPro

TR<-Mer2(F,HG)
TR2<-TR[,-c(15:19)]

write.csv(TR2,"Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro_plus_11_socioeconom_and_62_SUMS_per_day_foods_vars_ends_in_7864_cases.csv") ################# MARK for T-score_UVE_PLS_Logistic_Regr







