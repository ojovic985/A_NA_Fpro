
##F15+

A<-read.csv("ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"Age"])==FALSE)&&(A[i,"Age"]>14) ) { #&&(A[i,"Age"]<50) #for years 1-8a without "R" in AgeR, years 9-11 with "R"
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")

write.csv(Mat,"ndns_rp_yr1-4a_Total_and_Aval_iron_for_AllF15plus.csv")









A<-read.csv("ndns_rp_yr5-6a_foodleveldietarydata_v2.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"Age"])==FALSE)&&(A[i,"Age"]>14) ) { #&&(A[i,"Age"]<50) #for years 1-8a without "R" in AgeR, years 9-11 with "R"
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")

write.csv(Mat,"ndns_rp_yr5-6a_Total_and_Aval_iron_for_AllF15plus.csv")







A<-read.csv("ndns_rp_yr7-8a_foodleveldietarydata.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"Age"])==FALSE)&&(A[i,"Age"]>14) ) { #&&(A[i,"Age"]<50) #for years 1-8a without "R" in AgeR, years 9-11 with "R" 
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")

write.csv(Mat,"ndns_rp_yr7-8a_Total_and_Aval_iron_for_AllF15plus.csv")







A<-read.csv("ndns_rp_yr9a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"AgeR"])==FALSE)&&(A[i,"AgeR"]>14) ) { #&&(A[i,"Age"]<50) #for years 1-8a without "R" in AgeR, years 9-11 with "R" 
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")

write.csv(Mat,"ndns_rp_yr9a_Total_and_Aval_iron_for_AllF15plus.csv")








A<-read.csv("ndns_rp_yr10a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"AgeR"])==FALSE)&&(A[i,"AgeR"]>14) ) { #&&(A[i,"Age"]<50) #for years 1-8a without "R" in AgeR, years 9-11 with "R" 
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")



write.csv(Mat,"ndns_rp_yr10a_Total_and_Aval_iron_for_AllF15plus.csv")









A<-read.csv("ndns_rp_yr11a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"AgeR"])==FALSE)&&(A[i,"AgeR"]>14) ) { #&&(A[i,"Age"]<50) #for years 1-8a without "R" in AgeR, years 9-11 with "R"
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")


write.csv(Mat,"ndns_rp_yr11a_Total_and_Aval_iron_for_AllF15plus.csv")



















###F15-49


A<-read.csv("ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"Age"])==FALSE)&&(A[i,"Age"]>14)&&(A[i,"Age"]<50) ) { # #for years 1-8a without "R" in AgeR, years 9-11 with "R"
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")

write.csv(Mat,"ndns_rp_yr1-4a_Total_and_Aval_iron_for_F15-49.csv")







A<-read.csv("ndns_rp_yr5-6a_foodleveldietarydata_v2.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"Age"])==FALSE)&&(A[i,"Age"]>14)&&(A[i,"Age"]<50) ) { # #for years 1-8a without "R" in AgeR, years 9-11 with "R"
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")

write.csv(Mat,"ndns_rp_yr5-6a_Total_and_Aval_iron_for_F15-49.csv")







A<-read.csv("ndns_rp_yr7-8a_foodleveldietarydata.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"Age"])==FALSE)&&(A[i,"Age"]>14)&&(A[i,"Age"]<50) ) { # #for years 1-8a without "R" in AgeR, years 9-11 with "R"
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")

write.csv(Mat,"ndns_rp_yr7-8a_Total_and_Aval_iron_for_F15-49.csv")







A<-read.csv("ndns_rp_yr9a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"AgeR"])==FALSE)&&(A[i,"AgeR"]>14)&&(A[i,"AgeR"]<50) ) { # #for years 1-8a without "R" in AgeR, years 9-11 with "R"
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")

write.csv(Mat,"ndns_rp_yr9a_Total_and_Aval_iron_for_F15-49.csv")








A<-read.csv("ndns_rp_yr10a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"AgeR"])==FALSE)&&(A[i,"AgeR"]>14)&&(A[i,"AgeR"]<50) ) { # #for years 1-8a without "R" in AgeR, years 9-11 with "R"
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")



write.csv(Mat,"ndns_rp_yr10a_Total_and_Aval_iron_for_F15-49.csv")









A<-read.csv("ndns_rp_yr11a_foodleveldietarydata_uk_20210831.tab",sep="	",header=TRUE)


fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"AgeR"])==FALSE)&&(A[i,"AgeR"]>14)&&(A[i,"AgeR"]<50) ) { # #for years 1-8a without "R" in AgeR, years 9-11 with "R"
fe<-c(fe,c(i))}} 

A1<-A[fe,]

Ser1<-unique(A1[,"seriali"])
days<-unique(A1[,"DayNo"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"seriali"),]

for (j in 1:length(days)) {
G1<-G[grepl(days[j],G$"DayNo"),]

Meals<-unique(G1[,"MealTime"])
if (length(Meals)>0) {
for (k in 1:length(Meals)) {
G2<-G1[grepl(Meals[k],G1$"MealTime"),]

HI<-sum(G2[,"Haemironmg"])
NHI<-sum(G2[,"Nonhaemironmg"])
FE<-sum(G2[,"VitaminCmg"])
Tot_iron<-sum(G2[,"Ironmg"])

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3 + 8.93*log((FE+100)/100))/100 }

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI
Total_iron[i]<-Total_iron[i]+Tot_iron
}}}
Aval_iron[i]<-Aval_iron[i]/length(unique(G[,"DayNo"]))
Total_iron[i]<-Total_iron[i]/length(unique(G[,"DayNo"]))
}

Mat<-cbind(t(t(Ser1)),t(t(Total_iron)),t(t(Aval_iron)))
colnames(Mat)<-c("seriali","Total_iron","Aval_iron")


write.csv(Mat,"ndns_rp_yr11a_Total_and_Aval_iron_for_F15-49.csv")

























########################## F15+ #################################


B1<-read.csv("ndns_rp_yr1-4a_Total_and_Aval_iron_for_AllF15plus.csv",header=TRUE)
B2<-read.csv("ndns_rp_yr5-6a_Total_and_Aval_iron_for_AllF15plus.csv",header=TRUE)
B3<-read.csv("ndns_rp_yr7-8a_Total_and_Aval_iron_for_AllF15plus.csv",header=TRUE)
B4<-read.csv("ndns_rp_yr9a_Total_and_Aval_iron_for_AllF15plus.csv",header=TRUE)
B5<-read.csv("ndns_rp_yr10a_Total_and_Aval_iron_for_AllF15plus.csv",header=TRUE)
B6<-read.csv("ndns_rp_yr11a_Total_and_Aval_iron_for_AllF15plus.csv",header=TRUE)

Buk<-rbind(B1,B2,B3,B4,B5,B6)
Buk<-Buk[,-1]



A<-read.csv("ndns_rp_yr1-4a_indiv_uk.tab",sep="	",header=TRUE)
A2<-read.csv("ndns_rp_yr5-6a_indiv.tab",sep="	",header=TRUE)
A3<-read.csv("ndns_rp_yr7-8a_indiv.tab",sep="	",header=TRUE)
A4<-read.csv("ndns_rp_yr9-11a_indiv_20211020.tab",sep="	",header=TRUE)

Aq<-A[colnames(A) %in% c("seriali","area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_UKY1234")]

A2q<-A2[colnames(A2) %in% c("seriali","area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_Y56")]

A3q<-A3[colnames(A3) %in% c("seriali","area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_Y78")]

A4q<-A4[colnames(A4) %in% c("seriali","Area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_Y911")]

wti_UKY1234r = Aq[,"wti_UKY1234"] * (15655 / 6828) * (4/11)
wti_UKY56r = A2q[,"wti_Y56"] * (15655 / 2546) * (2/11)
wti_UKY78r = A3q[,"wti_Y78"] * (15655 / 2723) * (2/11)
wti_UKY911r = A4q[,"wti_Y911"] * (15655 / 3558) * (3/11)

Aq_2<-cbind(Aq,t(t(wti_UKY1234r)))
A2q_2<-cbind(A2q,t(t(wti_UKY56r)))
A3q_2<-cbind(A3q,t(t(wti_UKY78r)))
A4q_2<-cbind(A4q,t(t(wti_UKY911r)))
colnames(Aq_2)[ncol( Aq_2)]<-"Comb_weights"
colnames(A2q_2)[ncol( A2q_2)]<-"Comb_weights"
colnames(A3q_2)[ncol( A3q_2)]<-"Comb_weights"
colnames(A4q_2)[ncol( A4q_2)]<-"Comb_weights"
colnames(A4q_2)<-gsub("Area","area",colnames(A4q_2))

Aq_2<- Aq_2[!colnames(Aq_2) %in% c("wti_UKY1234")]
A2q_2<- A2q_2[!colnames(A2q_2) %in% c("wti_Y56")]
A3q_2<- A3q_2[!colnames(A3q_2) %in% c("wti_Y78")]
A4q_2<- A4q_2[!colnames(A4q_2) %in% c("wti_Y911")]

Aq_2<-Aq_2[,colnames(A2q_2)]

colnames(Aq_2)==colnames(A2q_2)
colnames(Aq_2)==colnames(A3q_2)
colnames(Aq_2)==colnames(A4q_2)

Auk<-rbind(Aq_2, A2q_2, A3q_2, A4q_2)

G1<-cbind(A[,"seriali"],A[,"Hb"],A[,"bmiok"])
colnames(G1)<-c("seriali","Hb","bmiok")
G2<-cbind(A2[,"seriali"],A2[,"Hb"],A2[,"bmiok"])
colnames(G2)<-c("seriali","Hb","bmiok")
G3<-cbind(A3[,"seriali"],A3[,"Hb"],A3[,"bmiok"])
colnames(G3)<-c("seriali","Hb","bmiok")
G4<-cbind(A4[,"seriali"],A4[,"Hb"],A4[,"bmiok"])
colnames(G4)<-c("seriali","Hb","bmiok")

Guk<-rbind(G1,G2,G3,G4)

ce<-c()
for (j in 1:nrow(Guk)) {
if (Guk[j,"bmiok"]==-90) {
ce[j]<-cbind(ce,c(j))}
}
length(ce)
#[1] 0

anaemia<-rep(0,nrow(Guk))

for (j in 1:nrow(Guk)) {
if ((is.na(Guk[j,"Hb"])==FALSE)&&(Guk[j,"Hb"]<12)&&(Guk[j,"Hb"]!=-1)) {
anaemia[j]<-1}
}
Guk2<-cbind(Guk,t(t(anaemia)))
colnames(Guk2)[ncol(Guk2)]<-"anaemia"
Guk2<-as.data.frame(Guk2)
Guk3<-Guk2[!colnames(Guk2) %in% c("Hb","bmiok")]

M1<-merge(Guk3,Buk,by=c("seriali"))

M2<-merge(M1,Auk,by=c("seriali"))

library(survey)

data=M2

ndns_design<-svydesign(id=~area,strata=~astrata1,weights=~Comb_weights,data=data,nest=TRUE)

check_strata_validity<-function(strata_var) {
design<-svydesign(
id = ~ area,
strata = as.formula(paste0("~",strata_var)),weights=~ Comb_weights,data=data,nest=TRUE)

n_strata<-length(unique(data[[strata_var]]))
psu_per_stratum<-table(data[[strata_var]],data$area)
strata_with_1_psu<-sum(rowSums(psu_per_stratum>0)==1)

list( strata=strata_var, single_psu_strata=strata_with_1_psu, total_strata=n_strata, design=design) }

strata_results<-lapply(paste0("astrata",1:5),check_strata_validity)

sapply(strata_results, function(x) x$single_psu_strata)


strata_vars<-paste0("astrata",1:5)
check_strata_validity<-function(strata_var) {
psu_per_stratum<-table(data[[strata_var]],data$area)
strata_with_1_psu<-sum(rowSums(psu_per_stratum>0)==1)
return(strata_with_1_psu) }

single_psu_counts<-sapply(strata_vars, check_strata_validity)
best_strata<-strata_vars[which.min(single_psu_counts)]
cat("Using:",best_strata,"\n")

strata_formula=as.formula(paste0("~",best_strata))

ndns_design<-svydesign(id=~area, strata=strata_formula,weights=~ Comb_weights,data=data,nest=TRUE)

#ndns_design<-svydesign(id=~area, strata=~astrata4,weights=~ Comb_weights,data=data,nest=TRUE)

options(survey.lonely.psu="adjust")

#svyglm(formula = anaemia ~ FPro, design = ndns_design, family = quasibinomial())

Total_iron<-svymean(~Total_iron,design = ndns_design,na.rm = TRUE)
Aval_iron<-svymean(~Aval_iron,design = ndns_design,na.rm = TRUE)



###Results for AllF15plus###
Total_iron<-svymean(~Total_iron,design = ndns_design,na.rm = TRUE)
Total_iron
coef(Total_iron)
SE(Total_iron)
confint(Total_iron)
#             mean     SE
#Total_iron 11.217 0.1861
#Total_iron 
#  11.21654 
#           Total_iron
#Total_iron   0.186098
#              2.5 %   97.5 %
#Total_iron 10.85179 11.58128

Aval_iron<-svymean(~Aval_iron,design = ndns_design,na.rm = TRUE)
Aval_iron
coef(Aval_iron)
SE(Aval_iron)
confint(Aval_iron)
#             mean     SE
#Aval_iron 0.65567 0.0103
#Aval_iron 
#0.6556698 
#          Aval_iron
#Aval_iron 0.0102887
#              2.5 %    97.5 %
#Aval_iron 0.6355043 0.6758352

dim(data)
#[1] 5559   11


###F15+ Cohort
mean(data[,"Aval_iron"])
#[1] 0.6307961
dim(data)
#[1] 5559   11
sd(data[,"Aval_iron"])*1.96/sqrt(dim(data)[1])
#[1] 0.01609514
mean(data[,"Total_iron"])
#[1] 10.90193
sd(data[,"Total_iron"])*1.96/sqrt(dim(data)[1])
#[1] 0.311374 ### F15+ Cohort

###Results for AllF15plus###
















########################## F15-49 #################################



B1<-read.csv("ndns_rp_yr1-4a_Total_and_Aval_iron_for_F15-49.csv",header=TRUE)
B2<-read.csv("ndns_rp_yr5-6a_Total_and_Aval_iron_for_F15-49.csv",header=TRUE)
B3<-read.csv("ndns_rp_yr7-8a_Total_and_Aval_iron_for_F15-49.csv",header=TRUE)
B4<-read.csv("ndns_rp_yr9a_Total_and_Aval_iron_for_F15-49.csv",header=TRUE)
B5<-read.csv("ndns_rp_yr10a_Total_and_Aval_iron_for_F15-49.csv",header=TRUE)
B6<-read.csv("ndns_rp_yr11a_Total_and_Aval_iron_for_F15-49.csv",header=TRUE)

Buk<-rbind(B1,B2,B3,B4,B5,B6)
Buk<-Buk[,-1]



A<-read.csv("ndns_rp_yr1-4a_indiv_uk.tab",sep="	",header=TRUE)
A2<-read.csv("ndns_rp_yr5-6a_indiv.tab",sep="	",header=TRUE)
A3<-read.csv("ndns_rp_yr7-8a_indiv.tab",sep="	",header=TRUE)
A4<-read.csv("ndns_rp_yr9-11a_indiv_20211020.tab",sep="	",header=TRUE)

Aq<-A[colnames(A) %in% c("seriali","area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_UKY1234")]

A2q<-A2[colnames(A2) %in% c("seriali","area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_Y56")]

A3q<-A3[colnames(A3) %in% c("seriali","area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_Y78")]

A4q<-A4[colnames(A4) %in% c("seriali","Area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_Y911")]

wti_UKY1234r = Aq[,"wti_UKY1234"] * (15655 / 6828) * (4/11)
wti_UKY56r = A2q[,"wti_Y56"] * (15655 / 2546) * (2/11)
wti_UKY78r = A3q[,"wti_Y78"] * (15655 / 2723) * (2/11)
wti_UKY911r = A4q[,"wti_Y911"] * (15655 / 3558) * (3/11)

Aq_2<-cbind(Aq,t(t(wti_UKY1234r)))
A2q_2<-cbind(A2q,t(t(wti_UKY56r)))
A3q_2<-cbind(A3q,t(t(wti_UKY78r)))
A4q_2<-cbind(A4q,t(t(wti_UKY911r)))
colnames(Aq_2)[ncol( Aq_2)]<-"Comb_weights"
colnames(A2q_2)[ncol( A2q_2)]<-"Comb_weights"
colnames(A3q_2)[ncol( A3q_2)]<-"Comb_weights"
colnames(A4q_2)[ncol( A4q_2)]<-"Comb_weights"
colnames(A4q_2)<-gsub("Area","area",colnames(A4q_2))

Aq_2<- Aq_2[!colnames(Aq_2) %in% c("wti_UKY1234")]
A2q_2<- A2q_2[!colnames(A2q_2) %in% c("wti_Y56")]
A3q_2<- A3q_2[!colnames(A3q_2) %in% c("wti_Y78")]
A4q_2<- A4q_2[!colnames(A4q_2) %in% c("wti_Y911")]

Aq_2<-Aq_2[,colnames(A2q_2)]

colnames(Aq_2)==colnames(A2q_2)
colnames(Aq_2)==colnames(A3q_2)
colnames(Aq_2)==colnames(A4q_2)

Auk<-rbind(Aq_2, A2q_2, A3q_2, A4q_2)

G1<-cbind(A[,"seriali"],A[,"Hb"],A[,"bmiok"])
colnames(G1)<-c("seriali","Hb","bmiok")
G2<-cbind(A2[,"seriali"],A2[,"Hb"],A2[,"bmiok"])
colnames(G2)<-c("seriali","Hb","bmiok")
G3<-cbind(A3[,"seriali"],A3[,"Hb"],A3[,"bmiok"])
colnames(G3)<-c("seriali","Hb","bmiok")
G4<-cbind(A4[,"seriali"],A4[,"Hb"],A4[,"bmiok"])
colnames(G4)<-c("seriali","Hb","bmiok")

Guk<-rbind(G1,G2,G3,G4)

ce<-c()
for (j in 1:nrow(Guk)) {
if (Guk[j,"bmiok"]==-90) {
ce[j]<-cbind(ce,c(j))}
}
length(ce)
#[1] 0

anaemia<-rep(0,nrow(Guk))

for (j in 1:nrow(Guk)) {
if ((is.na(Guk[j,"Hb"])==FALSE)&&(Guk[j,"Hb"]<12)&&(Guk[j,"Hb"]!=-1)) {
anaemia[j]<-1}
}
Guk2<-cbind(Guk,t(t(anaemia)))
colnames(Guk2)[ncol(Guk2)]<-"anaemia"
Guk2<-as.data.frame(Guk2)
Guk3<-Guk2[!colnames(Guk2) %in% c("Hb","bmiok")]

M1<-merge(Guk3,Buk,by=c("seriali"))

M2<-merge(M1,Auk,by=c("seriali"))

library(survey)

data=M2

ndns_design<-svydesign(id=~area,strata=~astrata1,weights=~Comb_weights,data=data,nest=TRUE)

check_strata_validity<-function(strata_var) {
design<-svydesign(
id = ~ area,
strata = as.formula(paste0("~",strata_var)),weights=~ Comb_weights,data=data,nest=TRUE)

n_strata<-length(unique(data[[strata_var]]))
psu_per_stratum<-table(data[[strata_var]],data$area)
strata_with_1_psu<-sum(rowSums(psu_per_stratum>0)==1)

list( strata=strata_var, single_psu_strata=strata_with_1_psu, total_strata=n_strata, design=design) }

strata_results<-lapply(paste0("astrata",1:5),check_strata_validity)

sapply(strata_results, function(x) x$single_psu_strata)


strata_vars<-paste0("astrata",1:5)
check_strata_validity<-function(strata_var) {
psu_per_stratum<-table(data[[strata_var]],data$area)
strata_with_1_psu<-sum(rowSums(psu_per_stratum>0)==1)
return(strata_with_1_psu) }

single_psu_counts<-sapply(strata_vars, check_strata_validity)
best_strata<-strata_vars[which.min(single_psu_counts)]
cat("Using:",best_strata,"\n")

strata_formula=as.formula(paste0("~",best_strata))

ndns_design<-svydesign(id=~area, strata=strata_formula,weights=~ Comb_weights,data=data,nest=TRUE)

#ndns_design<-svydesign(id=~area, strata=~astrata4,weights=~ Comb_weights,data=data,nest=TRUE)

options(survey.lonely.psu="adjust")

#svyglm(formula = anaemia ~ FPro, design = ndns_design, family = quasibinomial())

Total_iron<-svymean(~Total_iron,design = ndns_design,na.rm = TRUE)
Aval_iron<-svymean(~Aval_iron,design = ndns_design,na.rm = TRUE)



###Results for AllF15-49###
Total_iron
coef(Total_iron)
SE(Total_iron)
confint(Total_iron)
#            mean     SE
#Total_iron 10.96 0.2425
#Total_iron 
#  10.95959 
#           Total_iron
#Total_iron  0.2425297
#              2.5 %   97.5 %
#Total_iron 10.48424 11.43494

Aval_iron
coef(Aval_iron)
SE(Aval_iron)
confint(Aval_iron)
#             mean     SE
#Aval_iron 0.63153 0.0142
#Aval_iron 
#0.6315289 
#           Aval_iron
#Aval_iron 0.01416003
#              2.5 %   97.5 %
#Aval_iron 0.6037757 0.659282
dim(data)
#[1] 3343   11
###Results for AllF15-49###









