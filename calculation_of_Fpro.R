
https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fndds-download-databases/

https://github.com/menicgiulia/MLFoodProcessing/tree/main/input_data/USDA%20Raw%20Data/FNDDS/2007_2008

#download FNDDS2007_2008.mdb

library(Hmisc)
A<-mdb.get('FNDDS2007_2008.mdb')
write.csv(A[2],"FNDDS_2007-08_2.csv")
write.csv(A[10],"FNDDS_2007-08_10.csv")

A1<-read.csv("FNDDS_2007-08_2.csv",header=TRUE)
A2<-read.csv("FNDDS_2007-08_10.csv",header=TRUE)
E<-matrix(rep(NA, (dim(A1)[1]/65)*66),nrow=dim(A1)[1]/65)
colnames(E)[2:ncol(E)]<-A2[1:65,3]
colnames(E)[1]<-"FNDDSNutVal.Food.code"

for (i in 1:nrow(E)) {
E[i,1]<-A1[(1+((i-1)*65)),2]
E[i,2:ncol(E)]<-A1[(1+((i-1)*65)):(i*65),6]}
#write.csv(E,"E_2013-14.csv")
F<-E[,-5]
G<-cbind(F[,1:20],F[,22:39],F[,41:ncol(F)])
write.csv(G,"PREPARED_2007-2008_Nutrient_Values.csv")



A<-read.csv("PREPARED_2007-2008_Nutrient_Values.csv",header=TRUE)
Fe<-A[,-c(1,2)]
Xe<-Fe
c_g<-c(1,2,3,4,5,8,9,41:62)
c_mg<-c(6,7,10,11,12,13,14,15,16,17,22,27,28,29,30,31,34,38,40)
c_ug<-c(18,19,20,21,23,24,25,26,32,33,35,36,37,39)
Ze<-Xe
for (i in 1:ncol(Xe)) {
if (i %in% c_g) {
Ze[,i]<-Xe[,i]} 
else {
if (i %in% c_mg) {
Ze[,i]<-0.001*Xe[,i] }
else {
Ze[,i]<-0.000001*Xe[,i]}}
}

Se<-Ze
for (i in 1:ncol(Ze)) {
Se[,i]<-log(Ze[,i]) }

for (i in 1:nrow(Se)) {
for (j in 1:ncol(Se)) {
if (Se[i,j]< (-20)) {
Se[i,j]<-(-20)}}}

colnames(Se)<-c('Protein','Total Fat','Carbohydrate','Alcohol','Water','Caffeine','Theobromine','Sugars, total','Fiber, total dietary','Calcium','Iron','Magnesium','Phosphorus','Potassium','Sodium','Zinc','Copper','Selenium','Retinol','Carotene, beta','Carotene, alpha','Vitamin E (alpha-tocopherol)','Vitamin D (D2 + D3)','Cryptoxanthin, beta','Lycopene','Lutein + zeaxanthin','Vitamin C','Thiamin','Riboflavin','Niacin','Vitamin B-6','Folate, total','Vitamin B-12','Choline, total','Vitamin K (phylloquinone)','Folic acid','Folate, food','Vitamin E, added','Vitamin B-12, added','Cholesterol','Fatty acids, total saturated','4:00','6:00','8:00','10:00','12:00','14:00','16:00','18:00','18:01','18:02','18:03','20:04','22:6 n-3','16:01','18:04','20:01','20:5 n-3','22:01','22:5 n-3','Fatty acids, total monounsaturated','Fatty acids, total polyunsaturated')

write.csv(Se,"X62_set_2007-2008.csv") # This file will be used as input in ML_prod.py, see KEY INPUT, line 77 in ML_prod.py

# Go to https://github.com/menicgiulia/MLFoodProcessing/tree/main/input_data and download: FNDDS_2009-10_Training_Data.csv
#open python program: ML_prod.py



#For other years? Years 2009-2020 are actually the same as 2007-2008 (except different input FNDDS file is used, but the slight difference in coding is for years 2003-2004 and 2005-2006, so we provide cooresponding codes for them below).

library(Hmisc)
A<-mdb.get('FNDDS2003_2004.mdb') # Downloaded from FNDDS
write.csv(A[2],"FNDDS_2003-04_2.csv")
write.csv(A[10],"FNDDS_2003-04_10.csv")

A1<-read.csv("FNDDS_2003-04_2.csv",header=TRUE)
A2<-read.csv("FNDDS_2003-04_10.csv",header=TRUE)
E<-matrix(rep(NA, (dim(A1)[1]/63)*64),nrow=dim(A1)[1]/63)
colnames(E)[2:ncol(E)]<-A2[1:63,3]
colnames(E)[1]<-"FNDDSNutVal.Food.code"
for (i in 1:nrow(E)) {
E[i,1]<-A1[(1+((i-1)*63)),2]
E[i,2:ncol(E)]<-A1[(1+((i-1)*63)):(i*63),6]}
F<-E[,-5]
te<-rep(0,dim(F)[1])
G<-cbind(F[,1:20],F[,22:24],t(t(te)),F[,25:34],t(t(te)),F[,35:37],F[,39:ncol(F)])
colnames(G)[24]<-"Vitamin D (D2 + D3)"
colnames(G)[35]<-"Choline, total"
write.csv(G,"PREPARED_2003-2004_Nutrient_Values.csv")

A<-read.csv("PREPARED_2007-2008_Nutrient_Values.csv",header=TRUE)
Fe<-A[,-c(1,2)]
Xe<-Fe
c_g<-c(1,2,3,4,5,8,9,41:62)
c_mg<-c(6,7,10,11,12,13,14,15,16,17,22,27,28,29,30,31,34,38,40)
c_ug<-c(18,19,20,21,23,24,25,26,32,33,35,36,37,39)
Ze<-Xe
for (i in 1:ncol(Xe)) {
if (i %in% c_g) {
Ze[,i]<-Xe[,i]} 
else {
if (i %in% c_mg) {
Ze[,i]<-0.001*Xe[,i] }
else {
Ze[,i]<-0.000001*Xe[,i]}}
}

Se<-Ze
for (i in 1:ncol(Ze)) {
Se[,i]<-log(Ze[,i]) }

for (i in 1:nrow(Se)) {
for (j in 1:ncol(Se)) {
if (Se[i,j]< (-20)) {
Se[i,j]<-(-20)}}}

colnames(Se)<-c('Protein','Total Fat','Carbohydrate','Alcohol','Water','Caffeine','Theobromine','Sugars, total','Fiber, total dietary','Calcium','Iron','Magnesium','Phosphorus','Potassium','Sodium','Zinc','Copper','Selenium','Retinol','Carotene, beta','Carotene, alpha','Vitamin E (alpha-tocopherol)','Vitamin D (D2 + D3)','Cryptoxanthin, beta','Lycopene','Lutein + zeaxanthin','Vitamin C','Thiamin','Riboflavin','Niacin','Vitamin B-6','Folate, total','Vitamin B-12','Choline, total','Vitamin K (phylloquinone)','Folic acid','Folate, food','Vitamin E, added','Vitamin B-12, added','Cholesterol','Fatty acids, total saturated','4:00','6:00','8:00','10:00','12:00','14:00','16:00','18:00','18:01','18:02','18:03','20:04','22:6 n-3','16:01','18:04','20:01','20:5 n-3','22:01','22:5 n-3','Fatty acids, total monounsaturated','Fatty acids, total polyunsaturated')

write.csv(Se,"X62_set_2003-2004.csv") # This file will be used as input in ML_prod.py, see KEY INPUT, line 77 in ML_prod.py







library(Hmisc)
A<-mdb.get('FNDDS2005_2006.mdb') # Downloaded from FNDDS
write.csv(A[2],"FNDDS_2005-06_2.csv")
write.csv(A[10],"FNDDS_2005-06_10.csv")

A1<-read.csv("FNDDS_2005-06_2.csv",header=TRUE)
A2<-read.csv("FNDDS_2005-06_10.csv",header=TRUE)
E<-matrix(rep(NA, (dim(A1)[1]/64)*65),nrow=dim(A1)[1]/64)
colnames(E)[2:ncol(E)]<-A2[1:64,3]
colnames(E)[1]<-"FNDDSNutVal.Food.code"
for (i in 1:nrow(E)) {
E[i,1]<-A1[(1+((i-1)*64)),2]
E[i,2:ncol(E)]<-A1[(1+((i-1)*64)):(i*64),6]}
#write.csv(E,"E_2013-14.csv")
F<-E[,-5]
te<-rep(0,dim(F)[1])
G<-cbind(F[,1:20],F[,22:24],t(t(te)),F[,25:38],F[,40:ncol(F)])
colnames(G)[24]<-"Vitamin D (D2 + D3)"
write.csv(G,"PREPARED_2005-2006_Nutrient_Values.csv")

A<-read.csv("PREPARED_2005-2006_Nutrient_Values.csv",header=TRUE)
Fe<-A[,-c(1,2)]
Xe<-Fe
c_g<-c(1,2,3,4,5,8,9,41:62)
c_mg<-c(6,7,10,11,12,13,14,15,16,17,22,27,28,29,30,31,34,38,40)
c_ug<-c(18,19,20,21,23,24,25,26,32,33,35,36,37,39)
Ze<-Xe
for (i in 1:ncol(Xe)) {
if (i %in% c_g) {
Ze[,i]<-Xe[,i]} 
else {
if (i %in% c_mg) {
Ze[,i]<-0.001*Xe[,i] }
else {
Ze[,i]<-0.000001*Xe[,i]}}
}

Se<-Ze
for (i in 1:ncol(Ze)) {
Se[,i]<-log(Ze[,i]) }

for (i in 1:nrow(Se)) {
for (j in 1:ncol(Se)) {
if (Se[i,j]< (-20)) {
Se[i,j]<-(-20)}}}

colnames(Se)<-c('Protein','Total Fat','Carbohydrate','Alcohol','Water','Caffeine','Theobromine','Sugars, total','Fiber, total dietary','Calcium','Iron','Magnesium','Phosphorus','Potassium','Sodium','Zinc','Copper','Selenium','Retinol','Carotene, beta','Carotene, alpha','Vitamin E (alpha-tocopherol)','Vitamin D (D2 + D3)','Cryptoxanthin, beta','Lycopene','Lutein + zeaxanthin','Vitamin C','Thiamin','Riboflavin','Niacin','Vitamin B-6','Folate, total','Vitamin B-12','Choline, total','Vitamin K (phylloquinone)','Folic acid','Folate, food','Vitamin E, added','Vitamin B-12, added','Cholesterol','Fatty acids, total saturated','4:00','6:00','8:00','10:00','12:00','14:00','16:00','18:00','18:01','18:02','18:03','20:04','22:6 n-3','16:01','18:04','20:01','20:5 n-3','22:01','22:5 n-3','Fatty acids, total monounsaturated','Fatty acids, total polyunsaturated')

write.csv(Se,"X62_set_2005-2006.csv") # This file will be used as input in ML_prod.py, see KEY INPUT, line 77 in ML_prod.py





