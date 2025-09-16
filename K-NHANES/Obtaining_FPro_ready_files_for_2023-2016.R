
#Using F_codes_nutr_cont_per_100g files for years 2023-2016 and Train_42nutr.csv, run:


A<-read.csv("F_codes_nutr_cont_per_100g_of_hn23_24rc.csv",header=TRUE)

A<-A[,-1]
A<-na.omit(A)
B<-read.csv("Train_42nutr.csv",header=TRUE)
B<-B[,-1]
A1<-A[!colnames(A) %in% c("NF_N3","NF_N6","NF_20_0","NF_22_0","NF_24_0","NF_14_1","NF_24_1","NF_20_2","NF_20_3N6","NF_22_2","NF_VA_RAE","NF_VA")]

NF_18_3<-A1[,"NF_18_3N3"]+A1[,"NF_18_3N6"]
A2<-A1[!colnames(A1) %in% c("NF_18_3N3","NF_18_3N6")]
A3<-cbind(A2,NF_18_3)
colnames(A3)[ncol(A3)]<-"NF_18_3"

colnames(A3)<-c("Water","Protein","Total.Fat","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Fatty.acids..total.polyunsaturated","X10.00","X12.00","X14.00","X16.00","X18.00","X16.01","X18.01","X20.01","X22.01","X18.02","X18.04","X20.04","X20.5.n.3","X22.5.n.3","X22.6.n.3","Cholesterol","Carbohydrate","Fiber..total.dietary","Sugars..total","Calcium","Phosphorus","Sodium","Potassium","Magnesium","Iron","Zinc","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Carotene..beta","Retinol","Thiamin","Riboflavin","Niacin","Folic.acid","Vitamin.C","N_FCODE","X18.03")
A4<-A3[!colnames(A3) %in% c("N_FCODE")]
B1<-B[!colnames(B) %in% c("target")]
A4_reordered<-A4[, colnames(B1)]
A5<-cbind(A4_reordered, A3[,"N_FCODE"])
colnames(A5)[ncol(A5)]<-"Food.code"


A5[,"Calcium"]<-A5[,"Calcium"]/1000
A5[,"Iron"]<-A5[,"Iron"]/1000
A5[,"Magnesium"]<-A5[,"Magnesium"]/1000
A5[,"Phosphorus"]<-A5[,"Phosphorus"]/1000
A5[,"Potassium"]<-A5[,"Potassium"]/1000
A5[,"Sodium"]<-A5[,"Sodium"]/1000
A5[,"Zinc"]<-A5[,"Zinc"]/1000
A5[,"Retinol"]<-A5[,"Retinol"]/1000000
A5[,"Carotene..beta"]<-A5[,"Carotene..beta"]/1000000
A5[,"Vitamin.E..alpha.tocopherol."]<-A5[,"Vitamin.E..alpha.tocopherol."]/1000
A5[,"Vitamin.D..D2...D3."]<-A5[,"Vitamin.D..D2...D3."]/1000000
A5[,"Vitamin.C"]<-A5[,"Vitamin.C"]/1000
A5[,"Thiamin"]<-A5[,"Thiamin"]/1000
A5[,"Riboflavin"]<-A5[,"Riboflavin"]/1000
A5[,"Niacin"]<-A5[,"Niacin"]/1000
A5[,"Folic.acid"]<-A5[,"Folic.acid"]/1000000
A5[,"Cholesterol"]<-A5[,"Cholesterol"]/1000
cols_to_divide<-grep("^X", colnames(A5))
A5[,cols_to_divide]<-A5[,cols_to_divide]/1000

A6<-cbind(log(A5[,1:(ncol(A5)-1)]),A5[,ncol(A5)])
for (i in 1:nrow(A6)) {
for (j in 1:ncol(A6)) {
if (A6[i,j]<(-20))
A6[i,j]<--20}}

colnames(A6)[ncol(A6)]<-"Food.code"

write.csv(A5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_hn23_24rc.csv")
write.csv(A6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_hn23_24rc.csv")



A<-read.csv("F_codes_nutr_cont_per_100g_of_hn22_24rc.csv",header=TRUE)

A<-A[,-1]
A<-na.omit(A)
B<-read.csv("Train_42nutr.csv",header=TRUE)
B<-B[,-1]
A1<-A[!colnames(A) %in% c("NF_N3","NF_N6","NF_20_0","NF_22_0","NF_24_0","NF_14_1","NF_24_1","NF_20_2","NF_20_3N6","NF_22_2","NF_VA_RAE","NF_VA")]

NF_18_3<-A1[,"NF_18_3N3"]+A1[,"NF_18_3N6"]
A2<-A1[!colnames(A1) %in% c("NF_18_3N3","NF_18_3N6")]
A3<-cbind(A2,NF_18_3)
colnames(A3)[ncol(A3)]<-"NF_18_3"

colnames(A3)<-c("Water","Protein","Total.Fat","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Fatty.acids..total.polyunsaturated","X10.00","X12.00","X14.00","X16.00","X18.00","X16.01","X18.01","X20.01","X22.01","X18.02","X18.04","X20.04","X20.5.n.3","X22.5.n.3","X22.6.n.3","Cholesterol","Carbohydrate","Fiber..total.dietary","Sugars..total","Calcium","Phosphorus","Sodium","Potassium","Magnesium","Iron","Zinc","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Carotene..beta","Retinol","Thiamin","Riboflavin","Niacin","Folic.acid","Vitamin.C","N_FCODE","X18.03")
A4<-A3[!colnames(A3) %in% c("N_FCODE")]
B1<-B[!colnames(B) %in% c("target")]
A4_reordered<-A4[, colnames(B1)]
A5<-cbind(A4_reordered, A3[,"N_FCODE"])
colnames(A5)[ncol(A5)]<-"Food.code"


A5[,"Calcium"]<-A5[,"Calcium"]/1000
A5[,"Iron"]<-A5[,"Iron"]/1000
A5[,"Magnesium"]<-A5[,"Magnesium"]/1000
A5[,"Phosphorus"]<-A5[,"Phosphorus"]/1000
A5[,"Potassium"]<-A5[,"Potassium"]/1000
A5[,"Sodium"]<-A5[,"Sodium"]/1000
A5[,"Zinc"]<-A5[,"Zinc"]/1000
A5[,"Retinol"]<-A5[,"Retinol"]/1000000
A5[,"Carotene..beta"]<-A5[,"Carotene..beta"]/1000000
A5[,"Vitamin.E..alpha.tocopherol."]<-A5[,"Vitamin.E..alpha.tocopherol."]/1000
A5[,"Vitamin.D..D2...D3."]<-A5[,"Vitamin.D..D2...D3."]/1000000
A5[,"Vitamin.C"]<-A5[,"Vitamin.C"]/1000
A5[,"Thiamin"]<-A5[,"Thiamin"]/1000
A5[,"Riboflavin"]<-A5[,"Riboflavin"]/1000
A5[,"Niacin"]<-A5[,"Niacin"]/1000
A5[,"Folic.acid"]<-A5[,"Folic.acid"]/1000000
A5[,"Cholesterol"]<-A5[,"Cholesterol"]/1000
cols_to_divide<-grep("^X", colnames(A5))
A5[,cols_to_divide]<-A5[,cols_to_divide]/1000

A6<-cbind(log(A5[,1:(ncol(A5)-1)]),A5[,ncol(A5)])
for (i in 1:nrow(A6)) {
for (j in 1:ncol(A6)) {
if (A6[i,j]<(-20))
A6[i,j]<--20}}

colnames(A6)[ncol(A6)]<-"Food.code"

write.csv(A5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_hn22_24rc.csv")
write.csv(A6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_hn22_24rc.csv")



A<-read.csv("F_codes_nutr_cont_per_100g_of_hn21_24rc.csv",header=TRUE)

A<-A[,-1]
A<-na.omit(A)
B<-read.csv("Train_42nutr.csv",header=TRUE)
B<-B[,-1]
A1<-A[!colnames(A) %in% c("NF_N3","NF_N6","NF_20_0","NF_22_0","NF_24_0","NF_14_1","NF_24_1","NF_20_2","NF_20_3N6","NF_22_2","NF_VA_RAE","NF_VA")]

NF_18_3<-A1[,"NF_18_3N3"]+A1[,"NF_18_3N6"]
A2<-A1[!colnames(A1) %in% c("NF_18_3N3","NF_18_3N6")]
A3<-cbind(A2,NF_18_3)
colnames(A3)[ncol(A3)]<-"NF_18_3"

colnames(A3)<-c("Water","Protein","Total.Fat","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Fatty.acids..total.polyunsaturated","X10.00","X12.00","X14.00","X16.00","X18.00","X16.01","X18.01","X20.01","X22.01","X18.02","X18.04","X20.04","X20.5.n.3","X22.5.n.3","X22.6.n.3","Cholesterol","Carbohydrate","Fiber..total.dietary","Sugars..total","Calcium","Phosphorus","Sodium","Potassium","Magnesium","Iron","Zinc","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Carotene..beta","Retinol","Thiamin","Riboflavin","Niacin","Folic.acid","Vitamin.C","N_FCODE","X18.03")
A4<-A3[!colnames(A3) %in% c("N_FCODE")]
B1<-B[!colnames(B) %in% c("target")]
A4_reordered<-A4[, colnames(B1)]
A5<-cbind(A4_reordered, A3[,"N_FCODE"])
colnames(A5)[ncol(A5)]<-"Food.code"


A5[,"Calcium"]<-A5[,"Calcium"]/1000
A5[,"Iron"]<-A5[,"Iron"]/1000
A5[,"Magnesium"]<-A5[,"Magnesium"]/1000
A5[,"Phosphorus"]<-A5[,"Phosphorus"]/1000
A5[,"Potassium"]<-A5[,"Potassium"]/1000
A5[,"Sodium"]<-A5[,"Sodium"]/1000
A5[,"Zinc"]<-A5[,"Zinc"]/1000
A5[,"Retinol"]<-A5[,"Retinol"]/1000000
A5[,"Carotene..beta"]<-A5[,"Carotene..beta"]/1000000
A5[,"Vitamin.E..alpha.tocopherol."]<-A5[,"Vitamin.E..alpha.tocopherol."]/1000
A5[,"Vitamin.D..D2...D3."]<-A5[,"Vitamin.D..D2...D3."]/1000000
A5[,"Vitamin.C"]<-A5[,"Vitamin.C"]/1000
A5[,"Thiamin"]<-A5[,"Thiamin"]/1000
A5[,"Riboflavin"]<-A5[,"Riboflavin"]/1000
A5[,"Niacin"]<-A5[,"Niacin"]/1000
A5[,"Folic.acid"]<-A5[,"Folic.acid"]/1000000
A5[,"Cholesterol"]<-A5[,"Cholesterol"]/1000
cols_to_divide<-grep("^X", colnames(A5))
A5[,cols_to_divide]<-A5[,cols_to_divide]/1000

A6<-cbind(log(A5[,1:(ncol(A5)-1)]),A5[,ncol(A5)])
for (i in 1:nrow(A6)) {
for (j in 1:ncol(A6)) {
if (A6[i,j]<(-20))
A6[i,j]<--20}}

colnames(A6)[ncol(A6)]<-"Food.code"

write.csv(A5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_hn21_24rc.csv")
write.csv(A6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_hn21_24rc.csv")




A<-read.csv("F_codes_nutr_cont_per_100g_of_hn20_24rc.csv",header=TRUE)

A<-A[,-1]
A<-na.omit(A)
B<-read.csv("Train_42nutr.csv",header=TRUE)
B<-B[,-1]
A1<-A[!colnames(A) %in% c("NF_N3","NF_N6","NF_20_0","NF_22_0","NF_24_0","NF_14_1","NF_24_1","NF_20_2","NF_20_3N6","NF_22_2","NF_VA_RAE","NF_VA")]

NF_18_3<-A1[,"NF_18_3N3"]+A1[,"NF_18_3N6"]
A2<-A1[!colnames(A1) %in% c("NF_18_3N3","NF_18_3N6")]
A3<-cbind(A2,NF_18_3)
colnames(A3)[ncol(A3)]<-"NF_18_3"

colnames(A3)<-c("Water","Protein","Total.Fat","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Fatty.acids..total.polyunsaturated","X10.00","X12.00","X14.00","X16.00","X18.00","X16.01","X18.01","X20.01","X22.01","X18.02","X18.04","X20.04","X20.5.n.3","X22.5.n.3","X22.6.n.3","Cholesterol","Carbohydrate","Fiber..total.dietary","Sugars..total","Calcium","Phosphorus","Sodium","Potassium","Magnesium","Iron","Zinc","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Carotene..beta","Retinol","Thiamin","Riboflavin","Niacin","Folic.acid","Vitamin.C","N_FCODE","X18.03")
A4<-A3[!colnames(A3) %in% c("N_FCODE")]
B1<-B[!colnames(B) %in% c("target")]
A4_reordered<-A4[, colnames(B1)]
A5<-cbind(A4_reordered, A3[,"N_FCODE"])
colnames(A5)[ncol(A5)]<-"Food.code"


A5[,"Calcium"]<-A5[,"Calcium"]/1000
A5[,"Iron"]<-A5[,"Iron"]/1000
A5[,"Magnesium"]<-A5[,"Magnesium"]/1000
A5[,"Phosphorus"]<-A5[,"Phosphorus"]/1000
A5[,"Potassium"]<-A5[,"Potassium"]/1000
A5[,"Sodium"]<-A5[,"Sodium"]/1000
A5[,"Zinc"]<-A5[,"Zinc"]/1000
A5[,"Retinol"]<-A5[,"Retinol"]/1000000
A5[,"Carotene..beta"]<-A5[,"Carotene..beta"]/1000000
A5[,"Vitamin.E..alpha.tocopherol."]<-A5[,"Vitamin.E..alpha.tocopherol."]/1000
A5[,"Vitamin.D..D2...D3."]<-A5[,"Vitamin.D..D2...D3."]/1000000
A5[,"Vitamin.C"]<-A5[,"Vitamin.C"]/1000
A5[,"Thiamin"]<-A5[,"Thiamin"]/1000
A5[,"Riboflavin"]<-A5[,"Riboflavin"]/1000
A5[,"Niacin"]<-A5[,"Niacin"]/1000
A5[,"Folic.acid"]<-A5[,"Folic.acid"]/1000000
A5[,"Cholesterol"]<-A5[,"Cholesterol"]/1000
cols_to_divide<-grep("^X", colnames(A5))
A5[,cols_to_divide]<-A5[,cols_to_divide]/1000

A6<-cbind(log(A5[,1:(ncol(A5)-1)]),A5[,ncol(A5)])
for (i in 1:nrow(A6)) {
for (j in 1:ncol(A6)) {
if (A6[i,j]<(-20))
A6[i,j]<--20}}

colnames(A6)[ncol(A6)]<-"Food.code"

write.csv(A5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_hn20_24rc.csv")
write.csv(A6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_hn20_24rc.csv")




A<-read.csv("F_codes_nutr_cont_per_100g_of_hn19_24rc.csv",header=TRUE)

A<-A[,-1]
A<-na.omit(A)
B<-read.csv("Train_42nutr.csv",header=TRUE)
B<-B[,-1]
A1<-A[!colnames(A) %in% c("NF_N3","NF_N6","NF_20_0","NF_22_0","NF_24_0","NF_14_1","NF_24_1","NF_20_2","NF_20_3N6","NF_22_2","NF_VA_RAE","NF_VA")]

NF_18_3<-A1[,"NF_18_3N3"]+A1[,"NF_18_3N6"]
A2<-A1[!colnames(A1) %in% c("NF_18_3N3","NF_18_3N6")]
A3<-cbind(A2,NF_18_3)
colnames(A3)[ncol(A3)]<-"NF_18_3"

colnames(A3)<-c("Water","Protein","Total.Fat","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Fatty.acids..total.polyunsaturated","X10.00","X12.00","X14.00","X16.00","X18.00","X16.01","X18.01","X20.01","X22.01","X18.02","X18.04","X20.04","X20.5.n.3","X22.5.n.3","X22.6.n.3","Cholesterol","Carbohydrate","Fiber..total.dietary","Sugars..total","Calcium","Phosphorus","Sodium","Potassium","Magnesium","Iron","Zinc","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Carotene..beta","Retinol","Thiamin","Riboflavin","Niacin","Folic.acid","Vitamin.C","N_FCODE","X18.03")
A4<-A3[!colnames(A3) %in% c("N_FCODE")]
B1<-B[!colnames(B) %in% c("target")]
A4_reordered<-A4[, colnames(B1)]
A5<-cbind(A4_reordered, A3[,"N_FCODE"])
colnames(A5)[ncol(A5)]<-"Food.code"


A5[,"Calcium"]<-A5[,"Calcium"]/1000
A5[,"Iron"]<-A5[,"Iron"]/1000
A5[,"Magnesium"]<-A5[,"Magnesium"]/1000
A5[,"Phosphorus"]<-A5[,"Phosphorus"]/1000
A5[,"Potassium"]<-A5[,"Potassium"]/1000
A5[,"Sodium"]<-A5[,"Sodium"]/1000
A5[,"Zinc"]<-A5[,"Zinc"]/1000
A5[,"Retinol"]<-A5[,"Retinol"]/1000000
A5[,"Carotene..beta"]<-A5[,"Carotene..beta"]/1000000
A5[,"Vitamin.E..alpha.tocopherol."]<-A5[,"Vitamin.E..alpha.tocopherol."]/1000
A5[,"Vitamin.D..D2...D3."]<-A5[,"Vitamin.D..D2...D3."]/1000000
A5[,"Vitamin.C"]<-A5[,"Vitamin.C"]/1000
A5[,"Thiamin"]<-A5[,"Thiamin"]/1000
A5[,"Riboflavin"]<-A5[,"Riboflavin"]/1000
A5[,"Niacin"]<-A5[,"Niacin"]/1000
A5[,"Folic.acid"]<-A5[,"Folic.acid"]/1000000
A5[,"Cholesterol"]<-A5[,"Cholesterol"]/1000
cols_to_divide<-grep("^X", colnames(A5))
A5[,cols_to_divide]<-A5[,cols_to_divide]/1000

A6<-cbind(log(A5[,1:(ncol(A5)-1)]),A5[,ncol(A5)])
for (i in 1:nrow(A6)) {
for (j in 1:ncol(A6)) {
if (A6[i,j]<(-20))
A6[i,j]<--20}}

colnames(A6)[ncol(A6)]<-"Food.code"

write.csv(A5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_hn19_24rc.csv")
write.csv(A6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_hn19_24rc.csv")



A<-read.csv("F_codes_nutr_cont_per_100g_of_hn18_24rc.csv",header=TRUE)

A<-A[,-1]
A<-na.omit(A)
B<-read.csv("Train_42nutr.csv",header=TRUE)
B<-B[,-1]
A1<-A[!colnames(A) %in% c("NF_N3","NF_N6","NF_20_0","NF_22_0","NF_24_0","NF_14_1","NF_24_1","NF_20_2","NF_20_3N6","NF_22_2","NF_VA_RAE","NF_VA")]

NF_18_3<-A1[,"NF_18_3N3"]+A1[,"NF_18_3N6"]
A2<-A1[!colnames(A1) %in% c("NF_18_3N3","NF_18_3N6")]
A3<-cbind(A2,NF_18_3)
colnames(A3)[ncol(A3)]<-"NF_18_3"

colnames(A3)<-c("Water","Protein","Total.Fat","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Fatty.acids..total.polyunsaturated","X10.00","X12.00","X14.00","X16.00","X18.00","X16.01","X18.01","X20.01","X22.01","X18.02","X18.04","X20.04","X20.5.n.3","X22.5.n.3","X22.6.n.3","Cholesterol","Carbohydrate","Fiber..total.dietary","Sugars..total","Calcium","Phosphorus","Sodium","Potassium","Magnesium","Iron","Zinc","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Carotene..beta","Retinol","Thiamin","Riboflavin","Niacin","Folic.acid","Vitamin.C","N_FCODE","X18.03")
A4<-A3[!colnames(A3) %in% c("N_FCODE")]
B1<-B[!colnames(B) %in% c("target")]
A4_reordered<-A4[, colnames(B1)]
A5<-cbind(A4_reordered, A3[,"N_FCODE"])
colnames(A5)[ncol(A5)]<-"Food.code"


A5[,"Calcium"]<-A5[,"Calcium"]/1000
A5[,"Iron"]<-A5[,"Iron"]/1000
A5[,"Magnesium"]<-A5[,"Magnesium"]/1000
A5[,"Phosphorus"]<-A5[,"Phosphorus"]/1000
A5[,"Potassium"]<-A5[,"Potassium"]/1000
A5[,"Sodium"]<-A5[,"Sodium"]/1000
A5[,"Zinc"]<-A5[,"Zinc"]/1000
A5[,"Retinol"]<-A5[,"Retinol"]/1000000
A5[,"Carotene..beta"]<-A5[,"Carotene..beta"]/1000000
A5[,"Vitamin.E..alpha.tocopherol."]<-A5[,"Vitamin.E..alpha.tocopherol."]/1000
A5[,"Vitamin.D..D2...D3."]<-A5[,"Vitamin.D..D2...D3."]/1000000
A5[,"Vitamin.C"]<-A5[,"Vitamin.C"]/1000
A5[,"Thiamin"]<-A5[,"Thiamin"]/1000
A5[,"Riboflavin"]<-A5[,"Riboflavin"]/1000
A5[,"Niacin"]<-A5[,"Niacin"]/1000
A5[,"Folic.acid"]<-A5[,"Folic.acid"]/1000000
A5[,"Cholesterol"]<-A5[,"Cholesterol"]/1000
cols_to_divide<-grep("^X", colnames(A5))
A5[,cols_to_divide]<-A5[,cols_to_divide]/1000

A6<-cbind(log(A5[,1:(ncol(A5)-1)]),A5[,ncol(A5)])
for (i in 1:nrow(A6)) {
for (j in 1:ncol(A6)) {
if (A6[i,j]<(-20))
A6[i,j]<--20}}

colnames(A6)[ncol(A6)]<-"Food.code"

write.csv(A5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_hn18_24rc.csv")
write.csv(A6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_hn18_24rc.csv")




A<-read.csv("F_codes_nutr_cont_per_100g_of_hn17_24rc.csv",header=TRUE)

A<-A[,-1]
A<-na.omit(A)
B<-read.csv("Train_42nutr.csv",header=TRUE)
B<-B[,-1]
A1<-A[!colnames(A) %in% c("NF_N3","NF_N6","NF_20_0","NF_22_0","NF_24_0","NF_14_1","NF_24_1","NF_20_2","NF_20_3N6","NF_22_2","NF_VA_RAE","NF_VA")]

NF_18_3<-A1[,"NF_18_3N3"]+A1[,"NF_18_3N6"]
A2<-A1[!colnames(A1) %in% c("NF_18_3N3","NF_18_3N6")]
A3<-cbind(A2,NF_18_3)
colnames(A3)[ncol(A3)]<-"NF_18_3"

colnames(A3)<-c("Water","Protein","Total.Fat","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Fatty.acids..total.polyunsaturated","X10.00","X12.00","X14.00","X16.00","X18.00","X16.01","X18.01","X20.01","X22.01","X18.02","X18.04","X20.04","X20.5.n.3","X22.5.n.3","X22.6.n.3","Cholesterol","Carbohydrate","Fiber..total.dietary","Sugars..total","Calcium","Phosphorus","Sodium","Potassium","Magnesium","Iron","Zinc","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Carotene..beta","Retinol","Thiamin","Riboflavin","Niacin","Folic.acid","Vitamin.C","N_FCODE","X18.03")
A4<-A3[!colnames(A3) %in% c("N_FCODE")]
B1<-B[!colnames(B) %in% c("target")]
A4_reordered<-A4[, colnames(B1)]
A5<-cbind(A4_reordered, A3[,"N_FCODE"])
colnames(A5)[ncol(A5)]<-"Food.code"


A5[,"Calcium"]<-A5[,"Calcium"]/1000
A5[,"Iron"]<-A5[,"Iron"]/1000
A5[,"Magnesium"]<-A5[,"Magnesium"]/1000
A5[,"Phosphorus"]<-A5[,"Phosphorus"]/1000
A5[,"Potassium"]<-A5[,"Potassium"]/1000
A5[,"Sodium"]<-A5[,"Sodium"]/1000
A5[,"Zinc"]<-A5[,"Zinc"]/1000
A5[,"Retinol"]<-A5[,"Retinol"]/1000000
A5[,"Carotene..beta"]<-A5[,"Carotene..beta"]/1000000
A5[,"Vitamin.E..alpha.tocopherol."]<-A5[,"Vitamin.E..alpha.tocopherol."]/1000
A5[,"Vitamin.D..D2...D3."]<-A5[,"Vitamin.D..D2...D3."]/1000000
A5[,"Vitamin.C"]<-A5[,"Vitamin.C"]/1000
A5[,"Thiamin"]<-A5[,"Thiamin"]/1000
A5[,"Riboflavin"]<-A5[,"Riboflavin"]/1000
A5[,"Niacin"]<-A5[,"Niacin"]/1000
A5[,"Folic.acid"]<-A5[,"Folic.acid"]/1000000
A5[,"Cholesterol"]<-A5[,"Cholesterol"]/1000
cols_to_divide<-grep("^X", colnames(A5))
A5[,cols_to_divide]<-A5[,cols_to_divide]/1000

A6<-cbind(log(A5[,1:(ncol(A5)-1)]),A5[,ncol(A5)])
for (i in 1:nrow(A6)) {
for (j in 1:ncol(A6)) {
if (A6[i,j]<(-20))
A6[i,j]<--20}}

colnames(A6)[ncol(A6)]<-"Food.code"

write.csv(A5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_hn17_24rc.csv")
write.csv(A6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_hn17_24rc.csv")



A<-read.csv("F_codes_nutr_cont_per_100g_of_hn16_24rc.csv",header=TRUE)

A<-A[,-1]
A<-na.omit(A)
B<-read.csv("Train_42nutr.csv",header=TRUE)
B<-B[,-1]
A1<-A[!colnames(A) %in% c("NF_N3","NF_N6","NF_20_0","NF_22_0","NF_24_0","NF_14_1","NF_24_1","NF_20_2","NF_20_3N6","NF_22_2","NF_VA_RAE","NF_VA")]

NF_18_3<-A1[,"NF_18_3N3"]+A1[,"NF_18_3N6"]
A2<-A1[!colnames(A1) %in% c("NF_18_3N3","NF_18_3N6")]
A3<-cbind(A2,NF_18_3)
colnames(A3)[ncol(A3)]<-"NF_18_3"

colnames(A3)<-c("Water","Protein","Total.Fat","Fatty.acids..total.saturated","Fatty.acids..total.monounsaturated","Fatty.acids..total.polyunsaturated","X10.00","X12.00","X14.00","X16.00","X18.00","X16.01","X18.01","X20.01","X22.01","X18.02","X18.04","X20.04","X20.5.n.3","X22.5.n.3","X22.6.n.3","Cholesterol","Carbohydrate","Fiber..total.dietary","Sugars..total","Calcium","Phosphorus","Sodium","Potassium","Magnesium","Iron","Zinc","Vitamin.D..D2...D3.","Vitamin.E..alpha.tocopherol.","Carotene..beta","Retinol","Thiamin","Riboflavin","Niacin","Folic.acid","Vitamin.C","N_FCODE","X18.03")
A4<-A3[!colnames(A3) %in% c("N_FCODE")]
B1<-B[!colnames(B) %in% c("target")]
A4_reordered<-A4[, colnames(B1)]
A5<-cbind(A4_reordered, A3[,"N_FCODE"])
colnames(A5)[ncol(A5)]<-"Food.code"


A5[,"Calcium"]<-A5[,"Calcium"]/1000
A5[,"Iron"]<-A5[,"Iron"]/1000
A5[,"Magnesium"]<-A5[,"Magnesium"]/1000
A5[,"Phosphorus"]<-A5[,"Phosphorus"]/1000
A5[,"Potassium"]<-A5[,"Potassium"]/1000
A5[,"Sodium"]<-A5[,"Sodium"]/1000
A5[,"Zinc"]<-A5[,"Zinc"]/1000
A5[,"Retinol"]<-A5[,"Retinol"]/1000000
A5[,"Carotene..beta"]<-A5[,"Carotene..beta"]/1000000
A5[,"Vitamin.E..alpha.tocopherol."]<-A5[,"Vitamin.E..alpha.tocopherol."]/1000
A5[,"Vitamin.D..D2...D3."]<-A5[,"Vitamin.D..D2...D3."]/1000000
A5[,"Vitamin.C"]<-A5[,"Vitamin.C"]/1000
A5[,"Thiamin"]<-A5[,"Thiamin"]/1000
A5[,"Riboflavin"]<-A5[,"Riboflavin"]/1000
A5[,"Niacin"]<-A5[,"Niacin"]/1000
A5[,"Folic.acid"]<-A5[,"Folic.acid"]/1000000
A5[,"Cholesterol"]<-A5[,"Cholesterol"]/1000
cols_to_divide<-grep("^X", colnames(A5))
A5[,cols_to_divide]<-A5[,cols_to_divide]/1000

A6<-cbind(log(A5[,1:(ncol(A5)-1)]),A5[,ncol(A5)])
for (i in 1:nrow(A6)) {
for (j in 1:ncol(A6)) {
if (A6[i,j]<(-20))
A6[i,j]<--20}}

colnames(A6)[ncol(A6)]<-"Food.code"

write.csv(A5,"Adjusted_F_codes_nutr_cont_in_gram_per_100g_of_hn16_24rc.csv")
write.csv(A6,"log_ie_FPro-ready_F_codes_nutr_cont­_of_hn16_24rc.csv")



















