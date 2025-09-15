A<-read.csv("ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.tab",sep="	",header=TRUE)

A1<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Transfattyacidsg")]
A2<-cbind(A1,A[,"TotalGrams"])
colnames(A2)[ncol(A2)]<-"TotalGrams"
C<-cbind(A[,"seriali"],A[,"MainFoodGroupCode"],A2)
colnames(C)[1]<-"seriali"
colnames(C)[2]<-"MainFoodGroupCode"
C<-na.omit(C)
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="Proteing"):which(colnames(C)=="Transfattyacidsg")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"TotalGrams"] }} }
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { 
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

Weighted_Nutrients[,"Sodiummg"]<-Weighted_Nutrients[,"Sodiummg"]/1000 # "Sodiummg"   
colnames(Weighted_Nutrients)<-gsub("Sodiummg","Sodiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Potassiummg"]<-Weighted_Nutrients[,"Potassiummg"]/1000 # "Potassiummg" 
colnames(Weighted_Nutrients)<-gsub("Potassiummg","Potassiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Magnesiummg"]<-Weighted_Nutrients[,"Magnesiummg"]/1000 # "Magnesiummg" 
colnames(Weighted_Nutrients)<-gsub("Magnesiummg","Magnesiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Calciummg"]<-Weighted_Nutrients[,"Calciummg"]/1000 # "Calciummg" 
colnames(Weighted_Nutrients)<-gsub("Calciummg","Calciumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Phosphorusmg"]<-Weighted_Nutrients[,"Phosphorusmg"]/1000 # "Phosphorusmg"   
colnames(Weighted_Nutrients)<-gsub("Phosphorusmg","Phosphorusg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Ironmg"]<-Weighted_Nutrients[,"Ironmg"]/1000 # "Ironmg" 
colnames(Weighted_Nutrients)<-gsub("Ironmg","Irong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Haemironmg"]<-Weighted_Nutrients[,"Haemironmg"]/1000 # "Haemironmg"
colnames(Weighted_Nutrients)<-gsub("Haemironmg","Haemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Nonhaemironmg"]<-Weighted_Nutrients[,"Nonhaemironmg"]/1000 # "Nonhaemironmg" 
colnames(Weighted_Nutrients)<-gsub("Nonhaemironmg","Nonhaemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Coppermg"]<-Weighted_Nutrients[,"Coppermg"]/1000 # "Coppermg" 
colnames(Weighted_Nutrients)<-gsub("Coppermg","Copperg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Zincmg"]<-Weighted_Nutrients[,"Zincmg"]/1000 # "Zincmg"
colnames(Weighted_Nutrients)<-gsub("Zincmg","Zincg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Chloridemg"]<-Weighted_Nutrients[,"Chloridemg"]/1000 # "Chloridemg"  
colnames(Weighted_Nutrients)<-gsub("Chloridemg","Chlorideg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Retinolµg"]<-Weighted_Nutrients[,"Retinolµg"]/1000000 # "Retinolµg" 
colnames(Weighted_Nutrients)<-gsub("Retinolµg","Retinolg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Totalcaroteneµg"]<-Weighted_Nutrients[,"Totalcaroteneµg"]/1000000 #  "Totalcaroteneµg" 
colnames(Weighted_Nutrients)<-gsub("Totalcaroteneµg","Totalcaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Alphacaroteneµg"]<-Weighted_Nutrients[,"Alphacaroteneµg"]/1000000 # "Alphacaroteneµg"
colnames(Weighted_Nutrients)<-gsub("Alphacaroteneµg","Alphacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacaroteneµg"]<-Weighted_Nutrients[,"Betacaroteneµg"]/1000000 # "Betacaroteneµg"   
colnames(Weighted_Nutrients)<-gsub("Betacaroteneµg","Betacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacryptoxanthinµg"]<-Weighted_Nutrients[,"Betacryptoxanthinµg"]/1000000 # "Betacryptoxanthinµg"
colnames(Weighted_Nutrients)<-gsub("Betacryptoxanthinµg","Betacryptoxanthing",colnames(Weighted_Nutrients))


Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]<-Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]/1000000 # "VitaminAretinolequivalentsµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminAretinolequivalentsµg","VitaminAretinolequivalentsg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminDµg"]<-Weighted_Nutrients[,"VitaminDµg"]/1000000 # "VitaminDµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminDµg","VitaminDg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminEmg"]<-Weighted_Nutrients[,"VitaminEmg"]/1000 # "VitaminEmg" 
colnames(Weighted_Nutrients)<-gsub("VitaminEmg","VitaminEg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Thiaminmg"]<-Weighted_Nutrients[,"Thiaminmg"]/1000 # "Thiaminmg"
colnames(Weighted_Nutrients)<-gsub("Thiaminmg","Thiaming",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Riboflavinmg"]<-Weighted_Nutrients[,"Riboflavinmg"]/1000 # "Riboflavinmg"   
colnames(Weighted_Nutrients)<-gsub("Riboflavinmg","Riboflaving",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Niacinequivalentmg"]<-Weighted_Nutrients[,"Niacinequivalentmg"]/1000 # "Niacinequivalentmg"
colnames(Weighted_Nutrients)<-gsub("Niacinequivalentmg","Niacinequivalentg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB6mg"]<-Weighted_Nutrients[,"VitaminB6mg"]/1000 # "VitaminB6mg" 
colnames(Weighted_Nutrients)<-gsub("VitaminB6mg","VitaminB6g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB12µg"]<-Weighted_Nutrients[,"VitaminB12µg"]/1000000 # "VitaminB12µg"
colnames(Weighted_Nutrients)<-gsub("VitaminB12µg","VitaminB12g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Folateµg"]<-Weighted_Nutrients[,"Folateµg"]/1000000 # "Folateµg" 
colnames(Weighted_Nutrients)<-gsub("Folateµg","Folateg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Pantothenicacidmg"]<-Weighted_Nutrients[,"Pantothenicacidmg"]/1000 # "Pantothenicacidmg"
colnames(Weighted_Nutrients)<-gsub("Pantothenicacidmg","Pantothenicacidg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Biotinµg"]<-Weighted_Nutrients[,"Biotinµg"]/1000000 # "Biotinµg"
colnames(Weighted_Nutrients)<-gsub("Biotinµg","Bioting",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminCmg"]<-Weighted_Nutrients[,"VitaminCmg"]/1000 # "VitaminCmg"
colnames(Weighted_Nutrients)<-gsub("VitaminCmg","VitaminCg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Manganesemg"]<-Weighted_Nutrients[,"Manganesemg"]/1000 # "Manganesemg"
colnames(Weighted_Nutrients)<-gsub("Manganesemg","Manganeseg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Iodineµg"]<-Weighted_Nutrients[,"Iodineµg"]/1000000 # "Iodineµg"
colnames(Weighted_Nutrients)<-gsub("Iodineµg","Iodineg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Seleniumµg"]<-Weighted_Nutrients[,"Seleniumµg"]/1000000 # "Seleniumµg"
colnames(Weighted_Nutrients)<-gsub("Seleniumµg","Seleniumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Cholesterolmg"]<-Weighted_Nutrients[,"Cholesterolmg"]/1000 # "Cholesterolmg"
colnames(Weighted_Nutrients)<-gsub("Cholesterolmg","Cholesterolg",colnames(Weighted_Nutrients))


Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))
colnames(Mat_Weight)[1]<-"seriali"

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_ndns_rp_yr1-4a.csv")






A<-read.csv("ndns_rp_yr5-6a_foodleveldietarydata_v2.tab",sep="	",header=TRUE)

A1<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Transfattyacidsg")]
A2<-cbind(A1,A[,"TotalGrams"])
colnames(A2)[ncol(A2)]<-"TotalGrams"
C<-cbind(A[,"seriali"],A[,"MainFoodGroupCode"],A2)
colnames(C)[1]<-"seriali"
colnames(C)[2]<-"MainFoodGroupCode"
C<-na.omit(C)
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="Proteing"):which(colnames(C)=="Transfattyacidsg")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"TotalGrams"] }} }
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { 
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

Weighted_Nutrients[,"Sodiummg"]<-Weighted_Nutrients[,"Sodiummg"]/1000 # "Sodiummg"   
colnames(Weighted_Nutrients)<-gsub("Sodiummg","Sodiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Potassiummg"]<-Weighted_Nutrients[,"Potassiummg"]/1000 # "Potassiummg" 
colnames(Weighted_Nutrients)<-gsub("Potassiummg","Potassiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Magnesiummg"]<-Weighted_Nutrients[,"Magnesiummg"]/1000 # "Magnesiummg" 
colnames(Weighted_Nutrients)<-gsub("Magnesiummg","Magnesiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Calciummg"]<-Weighted_Nutrients[,"Calciummg"]/1000 # "Calciummg" 
colnames(Weighted_Nutrients)<-gsub("Calciummg","Calciumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Phosphorusmg"]<-Weighted_Nutrients[,"Phosphorusmg"]/1000 # "Phosphorusmg"   
colnames(Weighted_Nutrients)<-gsub("Phosphorusmg","Phosphorusg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Ironmg"]<-Weighted_Nutrients[,"Ironmg"]/1000 # "Ironmg" 
colnames(Weighted_Nutrients)<-gsub("Ironmg","Irong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Haemironmg"]<-Weighted_Nutrients[,"Haemironmg"]/1000 # "Haemironmg"
colnames(Weighted_Nutrients)<-gsub("Haemironmg","Haemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Nonhaemironmg"]<-Weighted_Nutrients[,"Nonhaemironmg"]/1000 # "Nonhaemironmg" 
colnames(Weighted_Nutrients)<-gsub("Nonhaemironmg","Nonhaemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Coppermg"]<-Weighted_Nutrients[,"Coppermg"]/1000 # "Coppermg" 
colnames(Weighted_Nutrients)<-gsub("Coppermg","Copperg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Zincmg"]<-Weighted_Nutrients[,"Zincmg"]/1000 # "Zincmg"
colnames(Weighted_Nutrients)<-gsub("Zincmg","Zincg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Chloridemg"]<-Weighted_Nutrients[,"Chloridemg"]/1000 # "Chloridemg"  
colnames(Weighted_Nutrients)<-gsub("Chloridemg","Chlorideg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Retinolµg"]<-Weighted_Nutrients[,"Retinolµg"]/1000000 # "Retinolµg" 
colnames(Weighted_Nutrients)<-gsub("Retinolµg","Retinolg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Totalcaroteneµg"]<-Weighted_Nutrients[,"Totalcaroteneµg"]/1000000 #  "Totalcaroteneµg" 
colnames(Weighted_Nutrients)<-gsub("Totalcaroteneµg","Totalcaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Alphacaroteneµg"]<-Weighted_Nutrients[,"Alphacaroteneµg"]/1000000 # "Alphacaroteneµg"
colnames(Weighted_Nutrients)<-gsub("Alphacaroteneµg","Alphacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacaroteneµg"]<-Weighted_Nutrients[,"Betacaroteneµg"]/1000000 # "Betacaroteneµg"   
colnames(Weighted_Nutrients)<-gsub("Betacaroteneµg","Betacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacryptoxanthinµg"]<-Weighted_Nutrients[,"Betacryptoxanthinµg"]/1000000 # "Betacryptoxanthinµg"
colnames(Weighted_Nutrients)<-gsub("Betacryptoxanthinµg","Betacryptoxanthing",colnames(Weighted_Nutrients))


Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]<-Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]/1000000 # "VitaminAretinolequivalentsµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminAretinolequivalentsµg","VitaminAretinolequivalentsg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminDµg"]<-Weighted_Nutrients[,"VitaminDµg"]/1000000 # "VitaminDµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminDµg","VitaminDg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminEmg"]<-Weighted_Nutrients[,"VitaminEmg"]/1000 # "VitaminEmg" 
colnames(Weighted_Nutrients)<-gsub("VitaminEmg","VitaminEg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Thiaminmg"]<-Weighted_Nutrients[,"Thiaminmg"]/1000 # "Thiaminmg"
colnames(Weighted_Nutrients)<-gsub("Thiaminmg","Thiaming",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Riboflavinmg"]<-Weighted_Nutrients[,"Riboflavinmg"]/1000 # "Riboflavinmg"   
colnames(Weighted_Nutrients)<-gsub("Riboflavinmg","Riboflaving",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Niacinequivalentmg"]<-Weighted_Nutrients[,"Niacinequivalentmg"]/1000 # "Niacinequivalentmg"
colnames(Weighted_Nutrients)<-gsub("Niacinequivalentmg","Niacinequivalentg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB6mg"]<-Weighted_Nutrients[,"VitaminB6mg"]/1000 # "VitaminB6mg" 
colnames(Weighted_Nutrients)<-gsub("VitaminB6mg","VitaminB6g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB12µg"]<-Weighted_Nutrients[,"VitaminB12µg"]/1000000 # "VitaminB12µg"
colnames(Weighted_Nutrients)<-gsub("VitaminB12µg","VitaminB12g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Folateµg"]<-Weighted_Nutrients[,"Folateµg"]/1000000 # "Folateµg" 
colnames(Weighted_Nutrients)<-gsub("Folateµg","Folateg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Pantothenicacidmg"]<-Weighted_Nutrients[,"Pantothenicacidmg"]/1000 # "Pantothenicacidmg"
colnames(Weighted_Nutrients)<-gsub("Pantothenicacidmg","Pantothenicacidg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Biotinµg"]<-Weighted_Nutrients[,"Biotinµg"]/1000000 # "Biotinµg"
colnames(Weighted_Nutrients)<-gsub("Biotinµg","Bioting",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminCmg"]<-Weighted_Nutrients[,"VitaminCmg"]/1000 # "VitaminCmg"
colnames(Weighted_Nutrients)<-gsub("VitaminCmg","VitaminCg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Manganesemg"]<-Weighted_Nutrients[,"Manganesemg"]/1000 # "Manganesemg"
colnames(Weighted_Nutrients)<-gsub("Manganesemg","Manganeseg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Iodineµg"]<-Weighted_Nutrients[,"Iodineµg"]/1000000 # "Iodineµg"
colnames(Weighted_Nutrients)<-gsub("Iodineµg","Iodineg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Seleniumµg"]<-Weighted_Nutrients[,"Seleniumµg"]/1000000 # "Seleniumµg"
colnames(Weighted_Nutrients)<-gsub("Seleniumµg","Seleniumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Cholesterolmg"]<-Weighted_Nutrients[,"Cholesterolmg"]/1000 # "Cholesterolmg"
colnames(Weighted_Nutrients)<-gsub("Cholesterolmg","Cholesterolg",colnames(Weighted_Nutrients))


Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))
colnames(Mat_Weight)[1]<-"seriali"

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_ndns_rp_yr5-6a.csv")





A<-read.csv("ndns_rp_yr7-8a_foodleveldietarydata.tab",sep="	",header=TRUE)

A1<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Transfattyacidsg")]
A2<-cbind(A1,A[,"TotalGrams"])
colnames(A2)[ncol(A2)]<-"TotalGrams"
C<-cbind(A[,"seriali"],A[,"MainFoodGroupCode"],A2)
colnames(C)[1]<-"seriali"
colnames(C)[2]<-"MainFoodGroupCode"
C<-na.omit(C)
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="Proteing"):which(colnames(C)=="Transfattyacidsg")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"TotalGrams"] }} }
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { 
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

Weighted_Nutrients[,"Sodiummg"]<-Weighted_Nutrients[,"Sodiummg"]/1000 # "Sodiummg"   
colnames(Weighted_Nutrients)<-gsub("Sodiummg","Sodiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Potassiummg"]<-Weighted_Nutrients[,"Potassiummg"]/1000 # "Potassiummg" 
colnames(Weighted_Nutrients)<-gsub("Potassiummg","Potassiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Magnesiummg"]<-Weighted_Nutrients[,"Magnesiummg"]/1000 # "Magnesiummg" 
colnames(Weighted_Nutrients)<-gsub("Magnesiummg","Magnesiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Calciummg"]<-Weighted_Nutrients[,"Calciummg"]/1000 # "Calciummg" 
colnames(Weighted_Nutrients)<-gsub("Calciummg","Calciumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Phosphorusmg"]<-Weighted_Nutrients[,"Phosphorusmg"]/1000 # "Phosphorusmg"   
colnames(Weighted_Nutrients)<-gsub("Phosphorusmg","Phosphorusg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Ironmg"]<-Weighted_Nutrients[,"Ironmg"]/1000 # "Ironmg" 
colnames(Weighted_Nutrients)<-gsub("Ironmg","Irong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Haemironmg"]<-Weighted_Nutrients[,"Haemironmg"]/1000 # "Haemironmg"
colnames(Weighted_Nutrients)<-gsub("Haemironmg","Haemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Nonhaemironmg"]<-Weighted_Nutrients[,"Nonhaemironmg"]/1000 # "Nonhaemironmg" 
colnames(Weighted_Nutrients)<-gsub("Nonhaemironmg","Nonhaemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Coppermg"]<-Weighted_Nutrients[,"Coppermg"]/1000 # "Coppermg" 
colnames(Weighted_Nutrients)<-gsub("Coppermg","Copperg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Zincmg"]<-Weighted_Nutrients[,"Zincmg"]/1000 # "Zincmg"
colnames(Weighted_Nutrients)<-gsub("Zincmg","Zincg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Chloridemg"]<-Weighted_Nutrients[,"Chloridemg"]/1000 # "Chloridemg"  
colnames(Weighted_Nutrients)<-gsub("Chloridemg","Chlorideg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Retinolµg"]<-Weighted_Nutrients[,"Retinolµg"]/1000000 # "Retinolµg" 
colnames(Weighted_Nutrients)<-gsub("Retinolµg","Retinolg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Totalcaroteneµg"]<-Weighted_Nutrients[,"Totalcaroteneµg"]/1000000 #  "Totalcaroteneµg" 
colnames(Weighted_Nutrients)<-gsub("Totalcaroteneµg","Totalcaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Alphacaroteneµg"]<-Weighted_Nutrients[,"Alphacaroteneµg"]/1000000 # "Alphacaroteneµg"
colnames(Weighted_Nutrients)<-gsub("Alphacaroteneµg","Alphacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacaroteneµg"]<-Weighted_Nutrients[,"Betacaroteneµg"]/1000000 # "Betacaroteneµg"   
colnames(Weighted_Nutrients)<-gsub("Betacaroteneµg","Betacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacryptoxanthinµg"]<-Weighted_Nutrients[,"Betacryptoxanthinµg"]/1000000 # "Betacryptoxanthinµg"
colnames(Weighted_Nutrients)<-gsub("Betacryptoxanthinµg","Betacryptoxanthing",colnames(Weighted_Nutrients))


Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]<-Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]/1000000 # "VitaminAretinolequivalentsµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminAretinolequivalentsµg","VitaminAretinolequivalentsg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminDµg"]<-Weighted_Nutrients[,"VitaminDµg"]/1000000 # "VitaminDµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminDµg","VitaminDg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminEmg"]<-Weighted_Nutrients[,"VitaminEmg"]/1000 # "VitaminEmg" 
colnames(Weighted_Nutrients)<-gsub("VitaminEmg","VitaminEg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Thiaminmg"]<-Weighted_Nutrients[,"Thiaminmg"]/1000 # "Thiaminmg"
colnames(Weighted_Nutrients)<-gsub("Thiaminmg","Thiaming",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Riboflavinmg"]<-Weighted_Nutrients[,"Riboflavinmg"]/1000 # "Riboflavinmg"   
colnames(Weighted_Nutrients)<-gsub("Riboflavinmg","Riboflaving",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Niacinequivalentmg"]<-Weighted_Nutrients[,"Niacinequivalentmg"]/1000 # "Niacinequivalentmg"
colnames(Weighted_Nutrients)<-gsub("Niacinequivalentmg","Niacinequivalentg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB6mg"]<-Weighted_Nutrients[,"VitaminB6mg"]/1000 # "VitaminB6mg" 
colnames(Weighted_Nutrients)<-gsub("VitaminB6mg","VitaminB6g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB12µg"]<-Weighted_Nutrients[,"VitaminB12µg"]/1000000 # "VitaminB12µg"
colnames(Weighted_Nutrients)<-gsub("VitaminB12µg","VitaminB12g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Folateµg"]<-Weighted_Nutrients[,"Folateµg"]/1000000 # "Folateµg" 
colnames(Weighted_Nutrients)<-gsub("Folateµg","Folateg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Pantothenicacidmg"]<-Weighted_Nutrients[,"Pantothenicacidmg"]/1000 # "Pantothenicacidmg"
colnames(Weighted_Nutrients)<-gsub("Pantothenicacidmg","Pantothenicacidg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Biotinµg"]<-Weighted_Nutrients[,"Biotinµg"]/1000000 # "Biotinµg"
colnames(Weighted_Nutrients)<-gsub("Biotinµg","Bioting",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminCmg"]<-Weighted_Nutrients[,"VitaminCmg"]/1000 # "VitaminCmg"
colnames(Weighted_Nutrients)<-gsub("VitaminCmg","VitaminCg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Manganesemg"]<-Weighted_Nutrients[,"Manganesemg"]/1000 # "Manganesemg"
colnames(Weighted_Nutrients)<-gsub("Manganesemg","Manganeseg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Iodineµg"]<-Weighted_Nutrients[,"Iodineµg"]/1000000 # "Iodineµg"
colnames(Weighted_Nutrients)<-gsub("Iodineµg","Iodineg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Seleniumµg"]<-Weighted_Nutrients[,"Seleniumµg"]/1000000 # "Seleniumµg"
colnames(Weighted_Nutrients)<-gsub("Seleniumµg","Seleniumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Cholesterolmg"]<-Weighted_Nutrients[,"Cholesterolmg"]/1000 # "Cholesterolmg"
colnames(Weighted_Nutrients)<-gsub("Cholesterolmg","Cholesterolg",colnames(Weighted_Nutrients))


Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))
colnames(Mat_Weight)[1]<-"seriali"

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_ndns_rp_yr7-8a.csv")


A<-read.csv("ndns_rp_yr9a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)

A1<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Transfattyacidsg")]
A2<-cbind(A1,A[,"TotalGrams"])
colnames(A2)[ncol(A2)]<-"TotalGrams"
C<-cbind(A[,"seriali"],A[,"MainFoodGroupCode"],A2)
colnames(C)[1]<-"seriali"
colnames(C)[2]<-"MainFoodGroupCode"
C<-na.omit(C)
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="Proteing"):which(colnames(C)=="Transfattyacidsg")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"TotalGrams"] }} }
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { 
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

Weighted_Nutrients[,"Sodiummg"]<-Weighted_Nutrients[,"Sodiummg"]/1000 # "Sodiummg"   
colnames(Weighted_Nutrients)<-gsub("Sodiummg","Sodiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Potassiummg"]<-Weighted_Nutrients[,"Potassiummg"]/1000 # "Potassiummg" 
colnames(Weighted_Nutrients)<-gsub("Potassiummg","Potassiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Magnesiummg"]<-Weighted_Nutrients[,"Magnesiummg"]/1000 # "Magnesiummg" 
colnames(Weighted_Nutrients)<-gsub("Magnesiummg","Magnesiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Calciummg"]<-Weighted_Nutrients[,"Calciummg"]/1000 # "Calciummg" 
colnames(Weighted_Nutrients)<-gsub("Calciummg","Calciumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Phosphorusmg"]<-Weighted_Nutrients[,"Phosphorusmg"]/1000 # "Phosphorusmg"   
colnames(Weighted_Nutrients)<-gsub("Phosphorusmg","Phosphorusg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Ironmg"]<-Weighted_Nutrients[,"Ironmg"]/1000 # "Ironmg" 
colnames(Weighted_Nutrients)<-gsub("Ironmg","Irong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Haemironmg"]<-Weighted_Nutrients[,"Haemironmg"]/1000 # "Haemironmg"
colnames(Weighted_Nutrients)<-gsub("Haemironmg","Haemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Nonhaemironmg"]<-Weighted_Nutrients[,"Nonhaemironmg"]/1000 # "Nonhaemironmg" 
colnames(Weighted_Nutrients)<-gsub("Nonhaemironmg","Nonhaemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Coppermg"]<-Weighted_Nutrients[,"Coppermg"]/1000 # "Coppermg" 
colnames(Weighted_Nutrients)<-gsub("Coppermg","Copperg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Zincmg"]<-Weighted_Nutrients[,"Zincmg"]/1000 # "Zincmg"
colnames(Weighted_Nutrients)<-gsub("Zincmg","Zincg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Chloridemg"]<-Weighted_Nutrients[,"Chloridemg"]/1000 # "Chloridemg"  
colnames(Weighted_Nutrients)<-gsub("Chloridemg","Chlorideg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Retinolµg"]<-Weighted_Nutrients[,"Retinolµg"]/1000000 # "Retinolµg" 
colnames(Weighted_Nutrients)<-gsub("Retinolµg","Retinolg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Totalcaroteneµg"]<-Weighted_Nutrients[,"Totalcaroteneµg"]/1000000 #  "Totalcaroteneµg" 
colnames(Weighted_Nutrients)<-gsub("Totalcaroteneµg","Totalcaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Alphacaroteneµg"]<-Weighted_Nutrients[,"Alphacaroteneµg"]/1000000 # "Alphacaroteneµg"
colnames(Weighted_Nutrients)<-gsub("Alphacaroteneµg","Alphacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacaroteneµg"]<-Weighted_Nutrients[,"Betacaroteneµg"]/1000000 # "Betacaroteneµg"   
colnames(Weighted_Nutrients)<-gsub("Betacaroteneµg","Betacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacryptoxanthinµg"]<-Weighted_Nutrients[,"Betacryptoxanthinµg"]/1000000 # "Betacryptoxanthinµg"
colnames(Weighted_Nutrients)<-gsub("Betacryptoxanthinµg","Betacryptoxanthing",colnames(Weighted_Nutrients))


Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]<-Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]/1000000 # "VitaminAretinolequivalentsµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminAretinolequivalentsµg","VitaminAretinolequivalentsg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminDµg"]<-Weighted_Nutrients[,"VitaminDµg"]/1000000 # "VitaminDµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminDµg","VitaminDg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminEmg"]<-Weighted_Nutrients[,"VitaminEmg"]/1000 # "VitaminEmg" 
colnames(Weighted_Nutrients)<-gsub("VitaminEmg","VitaminEg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Thiaminmg"]<-Weighted_Nutrients[,"Thiaminmg"]/1000 # "Thiaminmg"
colnames(Weighted_Nutrients)<-gsub("Thiaminmg","Thiaming",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Riboflavinmg"]<-Weighted_Nutrients[,"Riboflavinmg"]/1000 # "Riboflavinmg"   
colnames(Weighted_Nutrients)<-gsub("Riboflavinmg","Riboflaving",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Niacinequivalentmg"]<-Weighted_Nutrients[,"Niacinequivalentmg"]/1000 # "Niacinequivalentmg"
colnames(Weighted_Nutrients)<-gsub("Niacinequivalentmg","Niacinequivalentg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB6mg"]<-Weighted_Nutrients[,"VitaminB6mg"]/1000 # "VitaminB6mg" 
colnames(Weighted_Nutrients)<-gsub("VitaminB6mg","VitaminB6g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB12µg"]<-Weighted_Nutrients[,"VitaminB12µg"]/1000000 # "VitaminB12µg"
colnames(Weighted_Nutrients)<-gsub("VitaminB12µg","VitaminB12g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Folateµg"]<-Weighted_Nutrients[,"Folateµg"]/1000000 # "Folateµg" 
colnames(Weighted_Nutrients)<-gsub("Folateµg","Folateg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Pantothenicacidmg"]<-Weighted_Nutrients[,"Pantothenicacidmg"]/1000 # "Pantothenicacidmg"
colnames(Weighted_Nutrients)<-gsub("Pantothenicacidmg","Pantothenicacidg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Biotinµg"]<-Weighted_Nutrients[,"Biotinµg"]/1000000 # "Biotinµg"
colnames(Weighted_Nutrients)<-gsub("Biotinµg","Bioting",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminCmg"]<-Weighted_Nutrients[,"VitaminCmg"]/1000 # "VitaminCmg"
colnames(Weighted_Nutrients)<-gsub("VitaminCmg","VitaminCg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Manganesemg"]<-Weighted_Nutrients[,"Manganesemg"]/1000 # "Manganesemg"
colnames(Weighted_Nutrients)<-gsub("Manganesemg","Manganeseg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Iodineµg"]<-Weighted_Nutrients[,"Iodineµg"]/1000000 # "Iodineµg"
colnames(Weighted_Nutrients)<-gsub("Iodineµg","Iodineg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Seleniumµg"]<-Weighted_Nutrients[,"Seleniumµg"]/1000000 # "Seleniumµg"
colnames(Weighted_Nutrients)<-gsub("Seleniumµg","Seleniumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Cholesterolmg"]<-Weighted_Nutrients[,"Cholesterolmg"]/1000 # "Cholesterolmg"
colnames(Weighted_Nutrients)<-gsub("Cholesterolmg","Cholesterolg",colnames(Weighted_Nutrients))


Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))
colnames(Mat_Weight)[1]<-"seriali"

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_ndns_rp_yr9a.csv")




A<-read.csv("ndns_rp_yr10a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)

A1<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Transfattyacidsg")]
A2<-cbind(A1,A[,"TotalGrams"])
colnames(A2)[ncol(A2)]<-"TotalGrams"
C<-cbind(A[,"seriali"],A[,"MainFoodGroupCode"],A2)
colnames(C)[1]<-"seriali"
colnames(C)[2]<-"MainFoodGroupCode"
C<-na.omit(C)
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="Proteing"):which(colnames(C)=="Transfattyacidsg")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"TotalGrams"] }} }
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { 
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

Weighted_Nutrients[,"Sodiummg"]<-Weighted_Nutrients[,"Sodiummg"]/1000 # "Sodiummg"   
colnames(Weighted_Nutrients)<-gsub("Sodiummg","Sodiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Potassiummg"]<-Weighted_Nutrients[,"Potassiummg"]/1000 # "Potassiummg" 
colnames(Weighted_Nutrients)<-gsub("Potassiummg","Potassiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Magnesiummg"]<-Weighted_Nutrients[,"Magnesiummg"]/1000 # "Magnesiummg" 
colnames(Weighted_Nutrients)<-gsub("Magnesiummg","Magnesiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Calciummg"]<-Weighted_Nutrients[,"Calciummg"]/1000 # "Calciummg" 
colnames(Weighted_Nutrients)<-gsub("Calciummg","Calciumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Phosphorusmg"]<-Weighted_Nutrients[,"Phosphorusmg"]/1000 # "Phosphorusmg"   
colnames(Weighted_Nutrients)<-gsub("Phosphorusmg","Phosphorusg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Ironmg"]<-Weighted_Nutrients[,"Ironmg"]/1000 # "Ironmg" 
colnames(Weighted_Nutrients)<-gsub("Ironmg","Irong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Haemironmg"]<-Weighted_Nutrients[,"Haemironmg"]/1000 # "Haemironmg"
colnames(Weighted_Nutrients)<-gsub("Haemironmg","Haemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Nonhaemironmg"]<-Weighted_Nutrients[,"Nonhaemironmg"]/1000 # "Nonhaemironmg" 
colnames(Weighted_Nutrients)<-gsub("Nonhaemironmg","Nonhaemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Coppermg"]<-Weighted_Nutrients[,"Coppermg"]/1000 # "Coppermg" 
colnames(Weighted_Nutrients)<-gsub("Coppermg","Copperg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Zincmg"]<-Weighted_Nutrients[,"Zincmg"]/1000 # "Zincmg"
colnames(Weighted_Nutrients)<-gsub("Zincmg","Zincg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Chloridemg"]<-Weighted_Nutrients[,"Chloridemg"]/1000 # "Chloridemg"  
colnames(Weighted_Nutrients)<-gsub("Chloridemg","Chlorideg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Retinolµg"]<-Weighted_Nutrients[,"Retinolµg"]/1000000 # "Retinolµg" 
colnames(Weighted_Nutrients)<-gsub("Retinolµg","Retinolg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Totalcaroteneµg"]<-Weighted_Nutrients[,"Totalcaroteneµg"]/1000000 #  "Totalcaroteneµg" 
colnames(Weighted_Nutrients)<-gsub("Totalcaroteneµg","Totalcaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Alphacaroteneµg"]<-Weighted_Nutrients[,"Alphacaroteneµg"]/1000000 # "Alphacaroteneµg"
colnames(Weighted_Nutrients)<-gsub("Alphacaroteneµg","Alphacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacaroteneµg"]<-Weighted_Nutrients[,"Betacaroteneµg"]/1000000 # "Betacaroteneµg"   
colnames(Weighted_Nutrients)<-gsub("Betacaroteneµg","Betacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacryptoxanthinµg"]<-Weighted_Nutrients[,"Betacryptoxanthinµg"]/1000000 # "Betacryptoxanthinµg"
colnames(Weighted_Nutrients)<-gsub("Betacryptoxanthinµg","Betacryptoxanthing",colnames(Weighted_Nutrients))


Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]<-Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]/1000000 # "VitaminAretinolequivalentsµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminAretinolequivalentsµg","VitaminAretinolequivalentsg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminDµg"]<-Weighted_Nutrients[,"VitaminDµg"]/1000000 # "VitaminDµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminDµg","VitaminDg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminEmg"]<-Weighted_Nutrients[,"VitaminEmg"]/1000 # "VitaminEmg" 
colnames(Weighted_Nutrients)<-gsub("VitaminEmg","VitaminEg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Thiaminmg"]<-Weighted_Nutrients[,"Thiaminmg"]/1000 # "Thiaminmg"
colnames(Weighted_Nutrients)<-gsub("Thiaminmg","Thiaming",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Riboflavinmg"]<-Weighted_Nutrients[,"Riboflavinmg"]/1000 # "Riboflavinmg"   
colnames(Weighted_Nutrients)<-gsub("Riboflavinmg","Riboflaving",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Niacinequivalentmg"]<-Weighted_Nutrients[,"Niacinequivalentmg"]/1000 # "Niacinequivalentmg"
colnames(Weighted_Nutrients)<-gsub("Niacinequivalentmg","Niacinequivalentg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB6mg"]<-Weighted_Nutrients[,"VitaminB6mg"]/1000 # "VitaminB6mg" 
colnames(Weighted_Nutrients)<-gsub("VitaminB6mg","VitaminB6g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB12µg"]<-Weighted_Nutrients[,"VitaminB12µg"]/1000000 # "VitaminB12µg"
colnames(Weighted_Nutrients)<-gsub("VitaminB12µg","VitaminB12g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Folateµg"]<-Weighted_Nutrients[,"Folateµg"]/1000000 # "Folateµg" 
colnames(Weighted_Nutrients)<-gsub("Folateµg","Folateg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Pantothenicacidmg"]<-Weighted_Nutrients[,"Pantothenicacidmg"]/1000 # "Pantothenicacidmg"
colnames(Weighted_Nutrients)<-gsub("Pantothenicacidmg","Pantothenicacidg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Biotinµg"]<-Weighted_Nutrients[,"Biotinµg"]/1000000 # "Biotinµg"
colnames(Weighted_Nutrients)<-gsub("Biotinµg","Bioting",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminCmg"]<-Weighted_Nutrients[,"VitaminCmg"]/1000 # "VitaminCmg"
colnames(Weighted_Nutrients)<-gsub("VitaminCmg","VitaminCg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Manganesemg"]<-Weighted_Nutrients[,"Manganesemg"]/1000 # "Manganesemg"
colnames(Weighted_Nutrients)<-gsub("Manganesemg","Manganeseg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Iodineµg"]<-Weighted_Nutrients[,"Iodineµg"]/1000000 # "Iodineµg"
colnames(Weighted_Nutrients)<-gsub("Iodineµg","Iodineg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Seleniumµg"]<-Weighted_Nutrients[,"Seleniumµg"]/1000000 # "Seleniumµg"
colnames(Weighted_Nutrients)<-gsub("Seleniumµg","Seleniumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Cholesterolmg"]<-Weighted_Nutrients[,"Cholesterolmg"]/1000 # "Cholesterolmg"
colnames(Weighted_Nutrients)<-gsub("Cholesterolmg","Cholesterolg",colnames(Weighted_Nutrients))


Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))
colnames(Mat_Weight)[1]<-"seriali"

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_ndns_rp_yr10a.csv")




A<-read.csv("ndns_rp_yr11a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)

A1<-A[,which(colnames(A)=="Proteing"):which(colnames(A)=="Transfattyacidsg")]
A2<-cbind(A1,A[,"TotalGrams"])
colnames(A2)[ncol(A2)]<-"TotalGrams"
C<-cbind(A[,"seriali"],A[,"MainFoodGroupCode"],A2)
colnames(C)[1]<-"seriali"
colnames(C)[2]<-"MainFoodGroupCode"
C<-na.omit(C)
counts<-table(C[,"seriali"])
dezi<-as.numeric(counts)
C1<-C[,which(colnames(C)=="Proteing"):which(colnames(C)=="Transfattyacidsg")]
Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Nutrients)<-colnames(C1)
Weighted_Nutrients<-matrix(rep(0,length(dezi)*length(colnames(C1))),nrow=length(dezi))
colnames(Weighted_Nutrients)<-colnames(C1)
G<-as.matrix(C1)
for (j in 1:length(dezi)) {
kbr<-0
if (j==1) {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k] %in% c(45,48,49,51,57,58))) {
Nutrients[j,]<-Nutrients[j,]+G[k,]
kbr<-kbr+C[k,"TotalGrams"] }} }
else {
for (k in 1:dezi[j]) {
if (!(C$MainFoodGroupCode[k+cumsum(dezi)[j-1]] %in% c(45,48,49,51,57,58))) { 
Nutrients[j,]<-Nutrients[j,]+G[k+cumsum(dezi)[j-1],]
kbr<-kbr+C[k+cumsum(dezi)[j-1],"TotalGrams"] }}}
Weighted_Nutrients[j,]<-Nutrients[j,]/kbr}

repeated_vals<-names(counts)

Weighted_Nutrients[,"Sodiummg"]<-Weighted_Nutrients[,"Sodiummg"]/1000 # "Sodiummg"   
colnames(Weighted_Nutrients)<-gsub("Sodiummg","Sodiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Potassiummg"]<-Weighted_Nutrients[,"Potassiummg"]/1000 # "Potassiummg" 
colnames(Weighted_Nutrients)<-gsub("Potassiummg","Potassiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Magnesiummg"]<-Weighted_Nutrients[,"Magnesiummg"]/1000 # "Magnesiummg" 
colnames(Weighted_Nutrients)<-gsub("Magnesiummg","Magnesiumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Calciummg"]<-Weighted_Nutrients[,"Calciummg"]/1000 # "Calciummg" 
colnames(Weighted_Nutrients)<-gsub("Calciummg","Calciumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Phosphorusmg"]<-Weighted_Nutrients[,"Phosphorusmg"]/1000 # "Phosphorusmg"   
colnames(Weighted_Nutrients)<-gsub("Phosphorusmg","Phosphorusg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Ironmg"]<-Weighted_Nutrients[,"Ironmg"]/1000 # "Ironmg" 
colnames(Weighted_Nutrients)<-gsub("Ironmg","Irong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Haemironmg"]<-Weighted_Nutrients[,"Haemironmg"]/1000 # "Haemironmg"
colnames(Weighted_Nutrients)<-gsub("Haemironmg","Haemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Nonhaemironmg"]<-Weighted_Nutrients[,"Nonhaemironmg"]/1000 # "Nonhaemironmg" 
colnames(Weighted_Nutrients)<-gsub("Nonhaemironmg","Nonhaemirong",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Coppermg"]<-Weighted_Nutrients[,"Coppermg"]/1000 # "Coppermg" 
colnames(Weighted_Nutrients)<-gsub("Coppermg","Copperg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Zincmg"]<-Weighted_Nutrients[,"Zincmg"]/1000 # "Zincmg"
colnames(Weighted_Nutrients)<-gsub("Zincmg","Zincg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Chloridemg"]<-Weighted_Nutrients[,"Chloridemg"]/1000 # "Chloridemg"  
colnames(Weighted_Nutrients)<-gsub("Chloridemg","Chlorideg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Retinolµg"]<-Weighted_Nutrients[,"Retinolµg"]/1000000 # "Retinolµg" 
colnames(Weighted_Nutrients)<-gsub("Retinolµg","Retinolg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Totalcaroteneµg"]<-Weighted_Nutrients[,"Totalcaroteneµg"]/1000000 #  "Totalcaroteneµg" 
colnames(Weighted_Nutrients)<-gsub("Totalcaroteneµg","Totalcaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Alphacaroteneµg"]<-Weighted_Nutrients[,"Alphacaroteneµg"]/1000000 # "Alphacaroteneµg"
colnames(Weighted_Nutrients)<-gsub("Alphacaroteneµg","Alphacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacaroteneµg"]<-Weighted_Nutrients[,"Betacaroteneµg"]/1000000 # "Betacaroteneµg"   
colnames(Weighted_Nutrients)<-gsub("Betacaroteneµg","Betacaroteneg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Betacryptoxanthinµg"]<-Weighted_Nutrients[,"Betacryptoxanthinµg"]/1000000 # "Betacryptoxanthinµg"
colnames(Weighted_Nutrients)<-gsub("Betacryptoxanthinµg","Betacryptoxanthing",colnames(Weighted_Nutrients))


Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]<-Weighted_Nutrients[,"VitaminAretinolequivalentsµg"]/1000000 # "VitaminAretinolequivalentsµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminAretinolequivalentsµg","VitaminAretinolequivalentsg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminDµg"]<-Weighted_Nutrients[,"VitaminDµg"]/1000000 # "VitaminDµg" 
colnames(Weighted_Nutrients)<-gsub("VitaminDµg","VitaminDg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminEmg"]<-Weighted_Nutrients[,"VitaminEmg"]/1000 # "VitaminEmg" 
colnames(Weighted_Nutrients)<-gsub("VitaminEmg","VitaminEg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Thiaminmg"]<-Weighted_Nutrients[,"Thiaminmg"]/1000 # "Thiaminmg"
colnames(Weighted_Nutrients)<-gsub("Thiaminmg","Thiaming",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Riboflavinmg"]<-Weighted_Nutrients[,"Riboflavinmg"]/1000 # "Riboflavinmg"   
colnames(Weighted_Nutrients)<-gsub("Riboflavinmg","Riboflaving",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Niacinequivalentmg"]<-Weighted_Nutrients[,"Niacinequivalentmg"]/1000 # "Niacinequivalentmg"
colnames(Weighted_Nutrients)<-gsub("Niacinequivalentmg","Niacinequivalentg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB6mg"]<-Weighted_Nutrients[,"VitaminB6mg"]/1000 # "VitaminB6mg" 
colnames(Weighted_Nutrients)<-gsub("VitaminB6mg","VitaminB6g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminB12µg"]<-Weighted_Nutrients[,"VitaminB12µg"]/1000000 # "VitaminB12µg"
colnames(Weighted_Nutrients)<-gsub("VitaminB12µg","VitaminB12g",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Folateµg"]<-Weighted_Nutrients[,"Folateµg"]/1000000 # "Folateµg" 
colnames(Weighted_Nutrients)<-gsub("Folateµg","Folateg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Pantothenicacidmg"]<-Weighted_Nutrients[,"Pantothenicacidmg"]/1000 # "Pantothenicacidmg"
colnames(Weighted_Nutrients)<-gsub("Pantothenicacidmg","Pantothenicacidg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Biotinµg"]<-Weighted_Nutrients[,"Biotinµg"]/1000000 # "Biotinµg"
colnames(Weighted_Nutrients)<-gsub("Biotinµg","Bioting",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"VitaminCmg"]<-Weighted_Nutrients[,"VitaminCmg"]/1000 # "VitaminCmg"
colnames(Weighted_Nutrients)<-gsub("VitaminCmg","VitaminCg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Manganesemg"]<-Weighted_Nutrients[,"Manganesemg"]/1000 # "Manganesemg"
colnames(Weighted_Nutrients)<-gsub("Manganesemg","Manganeseg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Iodineµg"]<-Weighted_Nutrients[,"Iodineµg"]/1000000 # "Iodineµg"
colnames(Weighted_Nutrients)<-gsub("Iodineµg","Iodineg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Seleniumµg"]<-Weighted_Nutrients[,"Seleniumµg"]/1000000 # "Seleniumµg"
colnames(Weighted_Nutrients)<-gsub("Seleniumµg","Seleniumg",colnames(Weighted_Nutrients))
Weighted_Nutrients[,"Cholesterolmg"]<-Weighted_Nutrients[,"Cholesterolmg"]/1000 # "Cholesterolmg"
colnames(Weighted_Nutrients)<-gsub("Cholesterolmg","Cholesterolg",colnames(Weighted_Nutrients))


Mat_Weight<-data.frame(cbind(t(t(repeated_vals)),Weighted_Nutrients))
colnames(Mat_Weight)[1]<-"seriali"

write.csv(Mat_Weight,"ID_and_Weighted_Nutrients_for_ndns_rp_yr11a.csv")




