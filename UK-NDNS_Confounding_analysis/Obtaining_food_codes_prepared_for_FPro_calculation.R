At<-read.csv("F_codes_ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.csv",header=TRUE)
At<-At[,-1]

B<-read.csv("Data_9450_food_codes_g_per_100g.csv",header=TRUE)
B<-B[,-1]

G<-A
colnames(G)<-c("Protein","Total.Fat","Carbohydrate","Sodium","Potassium","Calcium","Magnesium","Phosphorus","Iron","Haemironmg","Nonhaemironmg","Copper","Zinc","Chloridemg","Retinol","Totalcarotenemg","Carotene..alpha","Carotene..beta","Cryptoxanthin..beta","VitaminAretinolequivalentsmg","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Folic.acid","Pantothenicacidmg","Biotinmg","Vitamin.C","Alcohol","Water","Sugars..total","Othersugarsmg","Starchmg","Glucosemg","Fructosemg","Sucrosemg","Maltosemg","Lactosemg","Nonmilkextrinsicsugarsmg","Intrinsicandmilksugarsmg","FreeSugarsmg","Fiber..total.dietary","Englystfibremg","Totalnitrogenmg","Manganesemg","Iodinemg","Selenium","Cholesterol","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Cisn6fattyacidsg","Cisn3fattyacidsg","FoodNumber")

G1<-G[,!grepl("mg",names(G))]

hu<-t(t(as.vector(G1[,"Cisn6fattyacidsg"]+G1[,"Cisn3fattyacidsg"])))

G2<-G1[!colnames(G1) %in% c("Cisn6fattyacidsg","Cisn3fattyacidsg")]
colnames(G2)[ncol(G2)]<-"Food.code"
G3<-cbind(G2,hu)
colnames(G3)[ncol(G3)]<-"Fatty.acids..total.polyunsaturated"

B1<-B[colnames(B) %in% colnames(G3)]

A_tr<-read.csv("Train.csv",header=TRUE)
A_te<-read.csv("Test.csv",header=TRUE)

A1_tr<-A_tr[colnames(A_tr) %in% colnames(B1)]
A1_te<-A_te[colnames(A_te) %in% colnames(B1)]

B2<-B1[,-ncol(B1)]

A2_tr<-A1_tr[,colnames(B2)]
A2_te<-A1_te[,colnames(B2)]
A3_tr<-cbind(A2_tr,A_tr[,"target"]); colnames(A3_tr)[ncol(A3_tr)]<-"target"
A3_te<-cbind(A2_te,A_te[,"target"]); colnames(A3_te)[ncol(A3_te)]<-"target"


write.csv(A3_tr,"Train_33nutr.csv")
write.csv(A3_te,"Test_33nutr.csv")

G4<-G3[,colnames(B1)]

G5<-G4

G5[,"Sodium"]<-G4[,"Sodium"]/1000
G5[,"Potassium"]<-G4[,"Potassium"]/1000
G5[,"Calcium"]<-G4[,"Calcium"]/1000
G5[,"Phosphorus"]<-G4[,"Phosphorus"]/1000
G5[,"Iron"]<-G4[,"Iron"]/1000
G5[,"Magnesium"]<-G4[,"Magnesium"]/1000
G5[,"Copper"]<-G4[,"Copper"]/1000
G5[,"Zinc"]<-G4[,"Zinc"]/1000
G5[,"Retinol"]<-G4[,"Retinol"]/1000000
G5[,"Selenium"]<-G4[,"Selenium"]/1000000
G5[,"Carotene..alpha"]<-G4[,"Carotene..alpha"]/1000000
G5[,"Carotene..beta"]<-G4[,"Carotene..beta"]/1000000

G5[,"Vitamin.E..alpha.tocopherol."]<-G4[,"Vitamin.E..alpha.tocopherol."]/1000
G5[,"Vitamin.D..D2...D3."]<-G4[,"Vitamin.D..D2...D3."]/1000000
G5[,"Cryptoxanthin..beta"]<-G4[,"Cryptoxanthin..beta"]/1000000
G5[,"Thiamin"]<-G4[,"Thiamin"]/1000
G5[,"Riboflavin"]<-G4[,"Riboflavin"]/1000
G5[,"Niacin"]<-G4[,"Niacin"]/1000

G5[,"Vitamin.B.6"]<-G4[,"Vitamin.B.6"]/1000
G5[,"Vitamin.B.12"]<-G4[,"Vitamin.B.12"]/1000000
G5[,"Folic.acid"]<-G4[,"Folic.acid"]/1000000
G5[,"Cholesterol"]<-G4[,"Cholesterol"]/1000
G5[,"Vitamin.C"]<-G4[,"Vitamin.C"]/1000

G6<-cbind(log(G5[,1:(ncol(G5)-1)]),G5[,ncol(G5)])
for (i in 1:nrow(G6)) {
for (j in 1:ncol(G6)) {
if ((G6[i,j]<(-20))||(is.na(G6[i,j])==TRUE))
G6[i,j]<--20}}

colnames(G6)[ncol(G6)]<-"Food.code"

write.csv(G5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_ndns_rp_yr1-4a.csv")
write.csv(G6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_ndns_rp_yr1-4a.csv")



At<-read.csv("F_codes_ndns_rp_yr5-6a_foodleveldietarydata_v2.tab.csv",header=TRUE)
At<-At[,-1]
A<-At

G<-A
colnames(G)<-c("Protein","Total.Fat","Carbohydrate","Sodium","Potassium","Calcium","Magnesium","Phosphorus","Iron","Haemironmg","Nonhaemironmg","Copper","Zinc","Chloridemg","Retinol","Totalcarotenemg","Carotene..alpha","Carotene..beta","Cryptoxanthin..beta","VitaminAretinolequivalentsmg","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Folic.acid","Pantothenicacidmg","Biotinmg","Vitamin.C","Alcohol","Water","Sugars..total","Othersugarsmg","Starchmg","Glucosemg","Fructosemg","Sucrosemg","Maltosemg","Lactosemg","Nonmilkextrinsicsugarsmg","Intrinsicandmilksugarsmg","FreeSugarsmg","Fiber..total.dietary","Englystfibremg","Totalnitrogenmg","Manganesemg","Iodinemg","Selenium","Cholesterol","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Cisn6fattyacidsg","Cisn3fattyacidsg","FoodNumber")

G1<-G[,!grepl("mg",names(G))]

hu<-t(t(as.vector(G1[,"Cisn6fattyacidsg"]+G1[,"Cisn3fattyacidsg"])))

G2<-G1[!colnames(G1) %in% c("Cisn6fattyacidsg","Cisn3fattyacidsg")]
colnames(G2)[ncol(G2)]<-"Food.code"
G3<-cbind(G2,hu)
colnames(G3)[ncol(G3)]<-"Fatty.acids..total.polyunsaturated"

G4<-G3[,colnames(B1)]

G5<-G4

G5[,"Sodium"]<-G4[,"Sodium"]/1000
G5[,"Potassium"]<-G4[,"Potassium"]/1000
G5[,"Calcium"]<-G4[,"Calcium"]/1000
G5[,"Phosphorus"]<-G4[,"Phosphorus"]/1000
G5[,"Iron"]<-G4[,"Iron"]/1000
G5[,"Magnesium"]<-G4[,"Magnesium"]/1000
G5[,"Copper"]<-G4[,"Copper"]/1000
G5[,"Zinc"]<-G4[,"Zinc"]/1000
G5[,"Retinol"]<-G4[,"Retinol"]/1000000
G5[,"Selenium"]<-G4[,"Selenium"]/1000000
G5[,"Carotene..alpha"]<-G4[,"Carotene..alpha"]/1000000
G5[,"Carotene..beta"]<-G4[,"Carotene..beta"]/1000000

G5[,"Vitamin.E..alpha.tocopherol."]<-G4[,"Vitamin.E..alpha.tocopherol."]/1000
G5[,"Vitamin.D..D2...D3."]<-G4[,"Vitamin.D..D2...D3."]/1000000
G5[,"Cryptoxanthin..beta"]<-G4[,"Cryptoxanthin..beta"]/1000000
G5[,"Thiamin"]<-G4[,"Thiamin"]/1000
G5[,"Riboflavin"]<-G4[,"Riboflavin"]/1000
G5[,"Niacin"]<-G4[,"Niacin"]/1000

G5[,"Vitamin.B.6"]<-G4[,"Vitamin.B.6"]/1000
G5[,"Vitamin.B.12"]<-G4[,"Vitamin.B.12"]/1000000
G5[,"Folic.acid"]<-G4[,"Folic.acid"]/1000000
G5[,"Cholesterol"]<-G4[,"Cholesterol"]/1000
G5[,"Vitamin.C"]<-G4[,"Vitamin.C"]/1000

G6<-cbind(log(G5[,1:(ncol(G5)-1)]),G5[,ncol(G5)])
for (i in 1:nrow(G6)) {
for (j in 1:ncol(G6)) {
if ((G6[i,j]<(-20))||(is.na(G6[i,j])==TRUE))
G6[i,j]<--20}}

colnames(G6)[ncol(G6)]<-"Food.code"

write.csv(G5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_ndns_rp_yr5-6a.csv")
write.csv(G6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_ndns_rp_yr5-6a.csv")



At<-read.csv("F_codes_ndns_rp_yr7-8a_foodleveldietarydata.tab.csv",header=TRUE)
At<-At[,-1]

A<-At

G<-A
colnames(G)<-c("Protein","Total.Fat","Carbohydrate","Sodium","Potassium","Calcium","Magnesium","Phosphorus","Iron","Haemironmg","Nonhaemironmg","Copper","Zinc","Chloridemg","Retinol","Totalcarotenemg","Carotene..alpha","Carotene..beta","Cryptoxanthin..beta","VitaminAretinolequivalentsmg","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Folic.acid","Pantothenicacidmg","Biotinmg","Vitamin.C","Alcohol","Water","Sugars..total","Othersugarsmg","Starchmg","Glucosemg","Fructosemg","Sucrosemg","Maltosemg","Lactosemg","Nonmilkextrinsicsugarsmg","Intrinsicandmilksugarsmg","FreeSugarsmg","Fiber..total.dietary","Englystfibremg","Totalnitrogenmg","Manganesemg","Iodinemg","Selenium","Cholesterol","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Cisn6fattyacidsg","Cisn3fattyacidsg","FoodNumber")

G1<-G[,!grepl("mg",names(G))]

hu<-t(t(as.vector(G1[,"Cisn6fattyacidsg"]+G1[,"Cisn3fattyacidsg"])))

G2<-G1[!colnames(G1) %in% c("Cisn6fattyacidsg","Cisn3fattyacidsg")]
colnames(G2)[ncol(G2)]<-"Food.code"
G3<-cbind(G2,hu)
colnames(G3)[ncol(G3)]<-"Fatty.acids..total.polyunsaturated"

G4<-G3[,colnames(B1)]

G5<-G4

G5[,"Sodium"]<-G4[,"Sodium"]/1000
G5[,"Potassium"]<-G4[,"Potassium"]/1000
G5[,"Calcium"]<-G4[,"Calcium"]/1000
G5[,"Phosphorus"]<-G4[,"Phosphorus"]/1000
G5[,"Iron"]<-G4[,"Iron"]/1000
G5[,"Magnesium"]<-G4[,"Magnesium"]/1000
G5[,"Copper"]<-G4[,"Copper"]/1000
G5[,"Zinc"]<-G4[,"Zinc"]/1000
G5[,"Retinol"]<-G4[,"Retinol"]/1000000
G5[,"Selenium"]<-G4[,"Selenium"]/1000000
G5[,"Carotene..alpha"]<-G4[,"Carotene..alpha"]/1000000
G5[,"Carotene..beta"]<-G4[,"Carotene..beta"]/1000000

G5[,"Vitamin.E..alpha.tocopherol."]<-G4[,"Vitamin.E..alpha.tocopherol."]/1000
G5[,"Vitamin.D..D2...D3."]<-G4[,"Vitamin.D..D2...D3."]/1000000
G5[,"Cryptoxanthin..beta"]<-G4[,"Cryptoxanthin..beta"]/1000000
G5[,"Thiamin"]<-G4[,"Thiamin"]/1000
G5[,"Riboflavin"]<-G4[,"Riboflavin"]/1000
G5[,"Niacin"]<-G4[,"Niacin"]/1000

G5[,"Vitamin.B.6"]<-G4[,"Vitamin.B.6"]/1000
G5[,"Vitamin.B.12"]<-G4[,"Vitamin.B.12"]/1000000
G5[,"Folic.acid"]<-G4[,"Folic.acid"]/1000000
G5[,"Cholesterol"]<-G4[,"Cholesterol"]/1000
G5[,"Vitamin.C"]<-G4[,"Vitamin.C"]/1000

G6<-cbind(log(G5[,1:(ncol(G5)-1)]),G5[,ncol(G5)])
for (i in 1:nrow(G6)) {
for (j in 1:ncol(G6)) {
if ((G6[i,j]<(-20))||(is.na(G6[i,j])==TRUE))
G6[i,j]<--20}}

colnames(G6)[ncol(G6)]<-"Food.code"

write.csv(G5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_ndns_rp_yr7-8a.csv")
write.csv(G6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_ndns_rp_yr7-8a.csv")



At<-read.csv("F_codes_ndns_rp_yr9a_foodleveldietarydata_uk_20210831.tab.csv",header=TRUE)
At<-At[,-1]

A<-At

G<-A
colnames(G)<-c("Protein","Total.Fat","Carbohydrate","Sodium","Potassium","Calcium","Magnesium","Phosphorus","Iron","Haemironmg","Nonhaemironmg","Copper","Zinc","Chloridemg","Retinol","Totalcarotenemg","Carotene..alpha","Carotene..beta","Cryptoxanthin..beta","VitaminAretinolequivalentsmg","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Folic.acid","Pantothenicacidmg","Biotinmg","Vitamin.C","Alcohol","Water","Sugars..total","Othersugarsmg","Starchmg","Glucosemg","Fructosemg","Sucrosemg","Maltosemg","Lactosemg","Nonmilkextrinsicsugarsmg","Intrinsicandmilksugarsmg","FreeSugarsmg","Fiber..total.dietary","Englystfibremg","Totalnitrogenmg","Manganesemg","Iodinemg","Selenium","Cholesterol","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Cisn6fattyacidsg","Cisn3fattyacidsg","FoodNumber")

G1<-G[,!grepl("mg",names(G))]

hu<-t(t(as.vector(G1[,"Cisn6fattyacidsg"]+G1[,"Cisn3fattyacidsg"])))

G2<-G1[!colnames(G1) %in% c("Cisn6fattyacidsg","Cisn3fattyacidsg")]
colnames(G2)[ncol(G2)]<-"Food.code"
G3<-cbind(G2,hu)
colnames(G3)[ncol(G3)]<-"Fatty.acids..total.polyunsaturated"

G4<-G3[,colnames(B1)]

G5<-G4

G5[,"Sodium"]<-G4[,"Sodium"]/1000
G5[,"Potassium"]<-G4[,"Potassium"]/1000
G5[,"Calcium"]<-G4[,"Calcium"]/1000
G5[,"Phosphorus"]<-G4[,"Phosphorus"]/1000
G5[,"Iron"]<-G4[,"Iron"]/1000
G5[,"Magnesium"]<-G4[,"Magnesium"]/1000
G5[,"Copper"]<-G4[,"Copper"]/1000
G5[,"Zinc"]<-G4[,"Zinc"]/1000
G5[,"Retinol"]<-G4[,"Retinol"]/1000000
G5[,"Selenium"]<-G4[,"Selenium"]/1000000
G5[,"Carotene..alpha"]<-G4[,"Carotene..alpha"]/1000000
G5[,"Carotene..beta"]<-G4[,"Carotene..beta"]/1000000

G5[,"Vitamin.E..alpha.tocopherol."]<-G4[,"Vitamin.E..alpha.tocopherol."]/1000
G5[,"Vitamin.D..D2...D3."]<-G4[,"Vitamin.D..D2...D3."]/1000000
G5[,"Cryptoxanthin..beta"]<-G4[,"Cryptoxanthin..beta"]/1000000
G5[,"Thiamin"]<-G4[,"Thiamin"]/1000
G5[,"Riboflavin"]<-G4[,"Riboflavin"]/1000
G5[,"Niacin"]<-G4[,"Niacin"]/1000

G5[,"Vitamin.B.6"]<-G4[,"Vitamin.B.6"]/1000
G5[,"Vitamin.B.12"]<-G4[,"Vitamin.B.12"]/1000000
G5[,"Folic.acid"]<-G4[,"Folic.acid"]/1000000
G5[,"Cholesterol"]<-G4[,"Cholesterol"]/1000
G5[,"Vitamin.C"]<-G4[,"Vitamin.C"]/1000

G6<-cbind(log(G5[,1:(ncol(G5)-1)]),G5[,ncol(G5)])
for (i in 1:nrow(G6)) {
for (j in 1:ncol(G6)) {
if ((G6[i,j]<(-20))||(is.na(G6[i,j])==TRUE))
G6[i,j]<--20}}

colnames(G6)[ncol(G6)]<-"Food.code"

write.csv(G5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_ndns_rp_yr9a.csv")
write.csv(G6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_ndns_rp_yr9a.csv")



At<-read.csv("F_codes_ndns_rp_yr10a_foodleveldietarydata_uk_20210831.tab.csv",header=TRUE)
At<-At[,-1]

A<-At

G<-A
colnames(G)<-c("Protein","Total.Fat","Carbohydrate","Sodium","Potassium","Calcium","Magnesium","Phosphorus","Iron","Haemironmg","Nonhaemironmg","Copper","Zinc","Chloridemg","Retinol","Totalcarotenemg","Carotene..alpha","Carotene..beta","Cryptoxanthin..beta","VitaminAretinolequivalentsmg","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Folic.acid","Pantothenicacidmg","Biotinmg","Vitamin.C","Alcohol","Water","Sugars..total","Othersugarsmg","Starchmg","Glucosemg","Fructosemg","Sucrosemg","Maltosemg","Lactosemg","Nonmilkextrinsicsugarsmg","Intrinsicandmilksugarsmg","FreeSugarsmg","Fiber..total.dietary","Englystfibremg","Totalnitrogenmg","Manganesemg","Iodinemg","Selenium","Cholesterol","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Cisn6fattyacidsg","Cisn3fattyacidsg","FoodNumber")

G1<-G[,!grepl("mg",names(G))]

hu<-t(t(as.vector(G1[,"Cisn6fattyacidsg"]+G1[,"Cisn3fattyacidsg"])))

G2<-G1[!colnames(G1) %in% c("Cisn6fattyacidsg","Cisn3fattyacidsg")]
colnames(G2)[ncol(G2)]<-"Food.code"
G3<-cbind(G2,hu)
colnames(G3)[ncol(G3)]<-"Fatty.acids..total.polyunsaturated"

G4<-G3[,colnames(B1)]

G5<-G4

G5[,"Sodium"]<-G4[,"Sodium"]/1000
G5[,"Potassium"]<-G4[,"Potassium"]/1000
G5[,"Calcium"]<-G4[,"Calcium"]/1000
G5[,"Phosphorus"]<-G4[,"Phosphorus"]/1000
G5[,"Iron"]<-G4[,"Iron"]/1000
G5[,"Magnesium"]<-G4[,"Magnesium"]/1000
G5[,"Copper"]<-G4[,"Copper"]/1000
G5[,"Zinc"]<-G4[,"Zinc"]/1000
G5[,"Retinol"]<-G4[,"Retinol"]/1000000
G5[,"Selenium"]<-G4[,"Selenium"]/1000000
G5[,"Carotene..alpha"]<-G4[,"Carotene..alpha"]/1000000
G5[,"Carotene..beta"]<-G4[,"Carotene..beta"]/1000000

G5[,"Vitamin.E..alpha.tocopherol."]<-G4[,"Vitamin.E..alpha.tocopherol."]/1000
G5[,"Vitamin.D..D2...D3."]<-G4[,"Vitamin.D..D2...D3."]/1000000
G5[,"Cryptoxanthin..beta"]<-G4[,"Cryptoxanthin..beta"]/1000000
G5[,"Thiamin"]<-G4[,"Thiamin"]/1000
G5[,"Riboflavin"]<-G4[,"Riboflavin"]/1000
G5[,"Niacin"]<-G4[,"Niacin"]/1000

G5[,"Vitamin.B.6"]<-G4[,"Vitamin.B.6"]/1000
G5[,"Vitamin.B.12"]<-G4[,"Vitamin.B.12"]/1000000
G5[,"Folic.acid"]<-G4[,"Folic.acid"]/1000000
G5[,"Cholesterol"]<-G4[,"Cholesterol"]/1000
G5[,"Vitamin.C"]<-G4[,"Vitamin.C"]/1000

G6<-cbind(log(G5[,1:(ncol(G5)-1)]),G5[,ncol(G5)])
for (i in 1:nrow(G6)) {
for (j in 1:ncol(G6)) {
if ((G6[i,j]<(-20))||(is.na(G6[i,j])==TRUE))
G6[i,j]<--20}}

colnames(G6)[ncol(G6)]<-"Food.code"

write.csv(G5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_ndns_rp_yr10a.csv")
write.csv(G6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_ndns_rp_yr10a.csv")



At<-read.csv("F_codes_ndns_rp_yr11a_foodleveldietarydata_uk_20210831.tab.csv",header=TRUE)
At<-At[,-1]

A<-At

G<-A
colnames(G)<-c("Protein","Total.Fat","Carbohydrate","Sodium","Potassium","Calcium","Magnesium","Phosphorus","Iron","Haemironmg","Nonhaemironmg","Copper","Zinc","Chloridemg","Retinol","Totalcarotenemg","Carotene..alpha","Carotene..beta","Cryptoxanthin..beta","VitaminAretinolequivalentsmg","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Thiamin","Riboflavin","Niacin","Vitamin.B.6","Vitamin.B.12","Folic.acid","Pantothenicacidmg","Biotinmg","Vitamin.C","Alcohol","Water","Sugars..total","Othersugarsmg","Starchmg","Glucosemg","Fructosemg","Sucrosemg","Maltosemg","Lactosemg","Nonmilkextrinsicsugarsmg","Intrinsicandmilksugarsmg","FreeSugarsmg","Fiber..total.dietary","Englystfibremg","Totalnitrogenmg","Manganesemg","Iodinemg","Selenium","Cholesterol","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Cisn6fattyacidsg","Cisn3fattyacidsg","FoodNumber")

G1<-G[,!grepl("mg",names(G))]

hu<-t(t(as.vector(G1[,"Cisn6fattyacidsg"]+G1[,"Cisn3fattyacidsg"])))

G2<-G1[!colnames(G1) %in% c("Cisn6fattyacidsg","Cisn3fattyacidsg")]
colnames(G2)[ncol(G2)]<-"Food.code"
G3<-cbind(G2,hu)
colnames(G3)[ncol(G3)]<-"Fatty.acids..total.polyunsaturated"

G4<-G3[,colnames(B1)]

G5<-G4

G5[,"Sodium"]<-G4[,"Sodium"]/1000
G5[,"Potassium"]<-G4[,"Potassium"]/1000
G5[,"Calcium"]<-G4[,"Calcium"]/1000
G5[,"Phosphorus"]<-G4[,"Phosphorus"]/1000
G5[,"Iron"]<-G4[,"Iron"]/1000
G5[,"Magnesium"]<-G4[,"Magnesium"]/1000
G5[,"Copper"]<-G4[,"Copper"]/1000
G5[,"Zinc"]<-G4[,"Zinc"]/1000
G5[,"Retinol"]<-G4[,"Retinol"]/1000000
G5[,"Selenium"]<-G4[,"Selenium"]/1000000
G5[,"Carotene..alpha"]<-G4[,"Carotene..alpha"]/1000000
G5[,"Carotene..beta"]<-G4[,"Carotene..beta"]/1000000

G5[,"Vitamin.E..alpha.tocopherol."]<-G4[,"Vitamin.E..alpha.tocopherol."]/1000
G5[,"Vitamin.D..D2...D3."]<-G4[,"Vitamin.D..D2...D3."]/1000000
G5[,"Cryptoxanthin..beta"]<-G4[,"Cryptoxanthin..beta"]/1000000
G5[,"Thiamin"]<-G4[,"Thiamin"]/1000
G5[,"Riboflavin"]<-G4[,"Riboflavin"]/1000
G5[,"Niacin"]<-G4[,"Niacin"]/1000

G5[,"Vitamin.B.6"]<-G4[,"Vitamin.B.6"]/1000
G5[,"Vitamin.B.12"]<-G4[,"Vitamin.B.12"]/1000000
G5[,"Folic.acid"]<-G4[,"Folic.acid"]/1000000
G5[,"Cholesterol"]<-G4[,"Cholesterol"]/1000
G5[,"Vitamin.C"]<-G4[,"Vitamin.C"]/1000

G6<-cbind(log(G5[,1:(ncol(G5)-1)]),G5[,ncol(G5)])
for (i in 1:nrow(G6)) {
for (j in 1:ncol(G6)) {
if ((G6[i,j]<(-20))||(is.na(G6[i,j])==TRUE))
G6[i,j]<--20}}

colnames(G6)[ncol(G6)]<-"Food.code"

write.csv(G5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_ndns_rp_yr11a.csv")
write.csv(G6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_ndns_rp_yr11a.csv")



