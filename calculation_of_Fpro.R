
https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fndds-download-databases/

https://github.com/menicgiulia/MLFoodProcessing/tree/main/input_data/USDA%20Raw%20Data/FNDDS/2007_2008

#download FNDDS2007_2008.mdb

library(Hmisc)
A<-mdb.get('FNDDS2007_2008.mdb')
write.csv(A[2],"FNDDS_2007-08_2.csv")
write.csv(A[10],"FNDDS_2007-08_10.csv")

A1<-read.csv("FNDDS_2009-10_2.csv",header=TRUE)
A2<-read.csv("FNDDS_2009-10_12.csv",header=TRUE)
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

write.csv(Se,"X62_set_2007-2008.csv")

# Go to https://github.com/menicgiulia/MLFoodProcessing/tree/main/input_data and download: FNDDS_2009-10_Training_Data.csv
#open python program: ML_prod.py

