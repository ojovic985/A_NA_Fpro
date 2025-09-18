A1<-read.csv("FPro_predicted_plus_food_Finland_15_nutr.csv",header=TRUE)
A1<-A1[,-1]
A1<-as.data.frame(cbind(rep("Finland",dim(A1)[1]),A1))
colnames(A1)[1]<-"Survey.s.country"

A2<-read.csv("FPro_predicted_plus_food_United.Kingdom_15_nutr.csv",header=TRUE)
A2<-A2[,-1]
A2<-as.data.frame(cbind(rep("United Kingdom",dim(A2)[1]),A2))
colnames(A2)[1]<-"Survey.s.country"

A3<-read.csv("FPro_predicted_plus_food_France_15_nutr.csv",header=TRUE)
A3<-A3[,-1]
A3<-as.data.frame(cbind(rep("France",dim(A3)[1]),A3))
colnames(A3)[1]<-"Survey.s.country"

A4<-read.csv("FPro_predicted_plus_food_Sweden_15_nutr.csv",header=TRUE)
A4<-A4[,-1]
A4<-as.data.frame(cbind(rep("Sweden",dim(A4)[1]),A4))
colnames(A4)[1]<-"Survey.s.country"

A5<-read.csv("FPro_predicted_plus_food_Italy_15_nutr.csv",header=TRUE)
A5<-A5[,-1]
A5<-as.data.frame(cbind(rep("Italy",dim(A5)[1]),A5))
colnames(A5)[1]<-"Survey.s.country"


A6<-read.csv("FPro_predicted_plus_food_Germany_15_nutr.csv",header=TRUE)
A6<-A6[,-1]
A6<-as.data.frame(cbind(rep("Germany",dim(A6)[1]),A6))
colnames(A6)[1]<-"Survey.s.country"


A7<-read.csv("FPro_predicted_plus_food_Netherlands_15_nutr.csv",header=TRUE)
A7<-A7[,-1]
A7<-as.data.frame(cbind(rep("Netherlands",dim(A7)[1]),A7))
colnames(A7)[1]<-"Survey.s.country"

A<-rbind(A1,A2,A3,A4,A5,A6,A7)
colnames(A)[ncol(A)]="FPro"

dat1<-read.csv("Foodex_2_L7_dashboard_part_1.csv",header=TRUE)
dat2<-read.csv("Foodex_2_L7_dashboard_part_2.csv",header=TRUE)
dat3<-read.csv("Foodex_2_L7_dashboard_part_3.csv",header=TRUE)
dat4<-read.csv("Foodex_2_L7_dashboard_part_4.csv",header=TRUE)
dat5<-read.csv("Foodex_2_L7_dashboard_part_5.csv",header=TRUE)
dat6<-read.csv("Foodex_2_L7_dashboard_part_6.csv",header=TRUE)
dat7<-read.csv("Foodex_2_L7_dashboard_part_7.csv",header=TRUE)
dat8<-read.csv("Foodex_2_L7_dashboard_part_8.csv",header=TRUE)

dat<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8)
dat<-dat[,-1]

Gen<-dat[grepl("Female", dat$"Gender"), ]
E<-Gen[grepl("Adults", Gen$"Population.Group..L2."), ]
E1<-Gen[grepl("Adolescents", Gen$"Population.Group..L2."), ]
F<-rbind(E,E1)


colnames(F)[which(colnames(F)=="Exposure.hierarchy..L6.")]<-colnames(A)[2]

#Obtaining Country&Years statistics

M<-merge(F,A,by=c(colnames(A)[2],"Survey.s.country"))

G<-M
G[,"Number.of.subjects"]<-as.numeric(gsub(",", "", G[,"Number.of.subjects"]))
G[,"Mean"]<-as.numeric(gsub(",", "", G[,"Mean"]))
Countries<-unique(G[,"Survey.s.country"])
Years<-unique(G[,"Survey.start.year"])
Years<-sort(Years)
Mat<-matrix(rep(NA,length(Countries)*length(Years)),nrow=length(Countries))
Mat1<-data.frame(cbind(Countries,Mat))
colnames(Mat1)[2:ncol(Mat1)]<-Years
Mat2<-data.frame(cbind(Countries,Mat))
colnames(Mat1)[2:ncol(Mat1)]<-Years

for (i in 1:length(Countries)) {
G1<-G[grepl(Countries[i], G$"Survey.s.country"), ]
Survey_years<-unique(G1[,"Survey.start.year"])

for (j in 1:length(Survey_years)) {
G2<-G1[grepl(Survey_years[j], G1$"Survey.start.year"), ]


PROD<-0
Weight<-0
for (k in 1:nrow(G2)) {

if ((G2[k,"Exposure.hierarchy..L1."]!="Water and water-based beverages")&& (G2[k,"Exposure.hierarchy..L1."]!="Alcoholic beverages")) {
#if (G2[k,"Exposure.hierarchy..L1."]!="Water and water-based beverages")  {
PROD<-PROD+as.numeric(G2[k,"Number.of.subjects"])*as.numeric(G2[k,"Mean"])*G2[k,"FPro"] 
Weight<-Weight+as.numeric(G2[k,"Number.of.subjects"])*as.numeric(G2[k,"Mean"])
}
}
Mat1[i,which(colnames(Mat1)==Survey_years[j])]<-PROD/Weight
Mat2[i,which(colnames(Mat1)==Survey_years[j])]<-PROD/Weight
}}

Set<-c()
for (i in 1:nrow(Mat1)) {
for (j in 2:ncol(Mat1)) {
if (is.na(Mat1[i,j])==FALSE) {
Set<-rbind(Set,cbind(Mat1[i,1],colnames(Mat1)[j],Mat1[i,j]))}
}}

Mat2<-as.data.frame(Set)

write.csv(Mat2,"FPro_Food_wise_withCoutries_Hierarchy_6_calculated_without_Water and water-based beverages_and_without_alcoholic_beverages.csv")


G<-M
G[,"Number.of.subjects"]<-as.numeric(gsub(",", "", G[,"Number.of.subjects"]))
G[,"Mean"]<-as.numeric(gsub(",", "", G[,"Mean"]))
Countries<-unique(G[,"Survey.s.country"])
Years<-unique(G[,"Survey.start.year"])
Years<-sort(Years)
Mat<-matrix(rep(NA,length(Countries)*length(Years)),nrow=length(Countries))
Mat1<-data.frame(cbind(Countries,Mat))
colnames(Mat1)[2:ncol(Mat1)]<-Years
Mat2<-data.frame(cbind(Countries,Mat))
colnames(Mat2)[2:ncol(Mat2)]<-Years

for (i in 1:length(Countries)) {
G1<-G[grepl(Countries[i], G$"Survey.s.country"), ]
Survey_years<-unique(G1[,"Survey.start.year"])

for (j in 1:length(Survey_years)) {
G2<-G1[grepl(Survey_years[j], G1$"Survey.start.year"), ]

x_vals <- c()
w_vals <- c()
for (k in 1:nrow(G2)) {
  if ((G2[k,"Exposure.hierarchy..L1."] != "Water and water-based beverages") &&
      (G2[k,"Exposure.hierarchy..L1."] != "Alcoholic beverages")) {

    w <- as.numeric(G2[k,"Number.of.subjects"]) * as.numeric(G2[k,"Mean"])
    x <- as.numeric(G2[k,"FPro"])

    w_vals <- c(w_vals, w)
    x_vals <- c(x_vals, x)
  }
}
weighted_mean <- sum(w_vals * x_vals) / sum(w_vals)
weighted_var <- sum(w_vals * (x_vals - weighted_mean)^2) / sum(w_vals)
n_eff <- (sum(w_vals))^2 / sum(w_vals^2)
SE <- sqrt(weighted_var / n_eff)
CI_low <- weighted_mean - 1.96 * SE
CI_high <- weighted_mean + 1.96 * SE
Mat1[i, which(colnames(Mat1) == Survey_years[j])] <- weighted_mean
Mat2[i, which(colnames(Mat2) == Survey_years[j])] <- CI_high - weighted_mean
}}

Set<-c()
for (i in 1:nrow(Mat1)) {
for (j in 2:ncol(Mat1)) {
if (is.na(Mat1[i,j])==FALSE) {
Set<-rbind(Set,cbind(Mat1[i,1],colnames(Mat1)[j],Mat1[i,j]))}
}}

Mat1_2<-as.data.frame(Set)
colnames(Mat1_2)<-c("Country","Year","FPro_Weighted_Average")

Set2<-c()
for (i in 1:nrow(Mat2)) {
for (j in 2:ncol(Mat2)) {
if (is.na(Mat2[i,j])==FALSE) {
Set2<-rbind(Set2,cbind(Mat2[i,1],colnames(Mat2)[j],Mat2[i,j]))}
}}

Mat2_2<-as.data.frame(Set2)
colnames(Mat2_2)<-c("Country","Year","95%_CI_half-margin")

write.csv(Mat1_2,"Weighted_Average_FPro_Food_wise_withCoutries_Hierarchy_6_calculated_without_Water and water-based beverages_and_without_alcoholic_beverages.csv")

write.csv(Mat2_2,"95CI_half-margin_of_Weighted_Average_FPro_Food_wise_withCoutries_Hierarchy_6_calculated_without_Water and water-based beverages_and_without_alcoholic_beverages.csv")

G<-M
G[,"Number.of.subjects"]<-as.numeric(gsub(",", "", G[,"Number.of.subjects"]))
G[,"Mean"]<-as.numeric(gsub(",", "", G[,"Mean"]))
Countries<-unique(G[,"Survey.s.country"])
Mat<-array(rep(NA,length(Countries)))
Mat1<-data.frame(cbind(Countries,Mat))
colnames(Mat1)<-c("Country","FPro")
Mat2<-data.frame(cbind(Countries,Mat))
colnames(Mat2)<-c("Country","95CI_FPro_half-width")

for (i in 1:length(Countries)) {
G1<-G[grepl(Countries[i], G$"Survey.s.country"), ]
G2<-G1

x_vals <- c()
w_vals <- c()
for (k in 1:nrow(G2)) {
  if ((G2[k,"Exposure.hierarchy..L1."] != "Water and water-based beverages") &&
      (G2[k,"Exposure.hierarchy..L1."] != "Alcoholic beverages")) {

    w <- as.numeric(G2[k,"Number.of.subjects"]) * as.numeric(G2[k,"Mean"])
    x <- as.numeric(G2[k,"FPro"])

    w_vals <- c(w_vals, w)
    x_vals <- c(x_vals, x)
  }
}
weighted_mean <- sum(w_vals * x_vals) / sum(w_vals)
weighted_var <- sum(w_vals * (x_vals - weighted_mean)^2) / sum(w_vals)
n_eff <- (sum(w_vals))^2 / sum(w_vals^2)
SE <- sqrt(weighted_var / n_eff)
CI_low <- weighted_mean - 1.96 * SE
CI_high <- weighted_mean + 1.96 * SE
Mat1[i,2] <- weighted_mean
Mat2[i,2] <- CI_high - weighted_mean
}

Mat3<-merge(Mat1,Mat2,by="Country")

write.csv(Mat3,"Weighted_Average_FPro_Food_wise_for_Coutries_of_merged_Years_Hierarchy_6_calculated_without_Water and water-based beverages_and_without_alcoholic_beverages.csv")



A<-read.csv("Weighted_Average_FPro_Food_wise_withCoutries_Hierarchy_6_calculated_without_Water and water-based beverages_and_without_alcoholic_beverages.csv",header=TRUE)
A<-A[,-1]
A2<-read.csv("Other_secondary_source_IHME_GLOBAL_ANEMIA_2000_2019_ALL_ANEMIA_PREV_ADMIN_0_Y2020M08D31.CSV",header=TRUE)
A2<-A2[,-1] #Anemia prevalence is 
Anaemia<-cbind(A2[,"ADM0_NAME"],A2[,"year"],A2[,"mean"])
colnames(Anaemia)<-c("Country","Year","An_prev")

Y<-merge(A,Anaemia,by=c("Country","Year"))
Sweden1997<-cbind(A[9,],0.0656); colnames(Sweden1997)[ncol(Sweden1997)]<-"An_prev"
Y1<-data.frame(rbind(Y,Sweden1997))

B<-read.csv("Population_and_other.csv") # source: https://www.census.gov/
B1<-B[,1:4]
B1<-cbind(B1,B[,ncol(B)]); colnames(B1)[ncol(B1)]<-"Female_age"
B2<-B1[,-2]
Sweden1997_pop<-cbind(Y1[nrow(Y1),],8846062,37.84) 
colnames(Sweden1997_pop)[ncol(Sweden1997_pop)-1]<-"Total.Population"
colnames(Sweden1997_pop)[ncol(Sweden1997_pop)]<-"Female_age"
# based on https://fred.stlouisfed.org/data/POPTOTSEA647NWDB?utm_source=chatgpt.com

Y1a<-merge(Y1,B2,by=c("Country","Year"))
Y1b<-Y1a
Y1b[,ncol(Y1b)-1]<-gsub(",","",Y1a[,ncol(Y1a)-1])

Y2<-data.frame(rbind(Y1b,Sweden1997_pop))

Q1<-read.csv("BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_1.csv",header=TRUE)
Q2<-read.csv("BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_1.csv",header=TRUE)
Q3<-read.csv("BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_1.csv",header=TRUE)
Q<-rbind(Q1,Q2,Q3)
C<-Q[,-1]
C1<-C[,1:5]
C1<-C1[,-2]
colnames(C1)[2]<-"Country"
C1<-C1[!colnames(C1) %in% c("ISO")]
C2<-cbind(C1,C[,"Prevalence.of.BMI..30.kg.m...obesity."])
colnames(C2)[ncol(C2)]<-"Obesity"


Y2a<-Y2[order(Y2[,c("Country")]),]

Y2b<-Y2a[grepl("Sweden", Y2a$"Country"), ]
Y2c<-Y2b[order(Y2b[,c("Year")]),]
Y2d<-Y2a[-grep("Sweden", Y2a$"Country"), ]

Y2e<-rbind(Y2d,Y2c)
Y2f<-Y2e[order(Y2e[,c("Country")]),]

C3<-merge(Y2f,C2,by=c("Country","Year"))

G<-C3

Countries<-unique(G[,"Country"])
Years<-unique(G[,"Year"])
Years<-sort(Years)
Mat<-matrix(rep(0,length(Countries)*length(Years)),nrow=length(Countries))
Mat1<-data.frame(cbind(Countries,as.matrix(Mat)))
colnames(Mat1)[2:ncol(Mat1)]<-Years

for (i in 1:length(Countries)) {
G1<-G[grepl(Countries[i], G$"Country"), ]

Survey_years<-unique(G1[,"Year"])

for (k in 1:length(Survey_years)) {

G2<-G1[grepl(Survey_years[k], G1$"Year"), ]
w<-0
br<-0
for (j in 1:nrow(G2)) {
if (G2$"Age.group"[j]=="18-19") {
Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])]<-as.numeric(Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])])+2*G2$"Obesity"[j]
w<-w+2
br<-br+1}
if (G2$"Age.group"[j]%in%c("20-24","25-29","30-34","35-39","40-44","45-49")) {
Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])]<-as.numeric(Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])])+5*G2$"Obesity"[j]
w<-w+5
br<-br+1}
}
Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])]<-as.numeric(Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])])/w
}}

Set<-c()
for (i in 1:nrow(Mat1)) {
for (j in 2:ncol(Mat1)) {
if (as.numeric(Mat1[i,j])!=0) {
Set<-rbind(Set,cbind(Mat1[i,1],colnames(Mat1)[j],Mat1[i,j]))}
}}

Mat2<-as.data.frame(Set)
colnames(Mat2)<-c("Country","Year","Obesity")

Y3<-merge(Y2f,Mat2,by=c("Country","Year"))

D<-read.csv("GDP_per_capita_API_NY.GDP.PCAP.CD_DS2_en_csv_v2_127149_header_solved.csv",header=TRUE)
colnames(D)<-gsub("X","",colnames(D))
D1<-D[,-c(2,3,4)]

Mat<-matrix(rep(0,length(Countries)*length(Years)),nrow=length(Countries))
Mat1<-data.frame(cbind(Countries,as.matrix(Mat)))
colnames(Mat1)[2:ncol(Mat1)]<-Years

for (i in 1: length(Countries)) {

H<-D1[grepl(Countries[i], D1$"Country"), ]

H1<-Y3[grepl(Countries[i], Y3$"Country"), ]

Survey_years<-unique(H1[,"Year"])

for (j in 1:length(Survey_years)) {
Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[j])] <-H[match(Survey_years[j],colnames(H))]
}
}

Set<-c()
for (i in 1:nrow(Mat1)) {
for (j in 2:ncol(Mat1)) {
if (as.numeric(Mat1[i,j])!=0) {
Set<-rbind(Set,cbind(Mat1[i,1],colnames(Mat1)[j],Mat1[i,j]))}
}}

Mat2<-as.data.frame(Set)
colnames(Mat2)<-c("Country","Year","GDP_pc")

Y4<-merge(Y3,Mat2,by=c("Country","Year"))
colnames(Y4)<-gsub("An_prev","Sec_Source_An_prev",colnames(Y4))

write.csv(Y4,"Country_Year_FPro_Anaemia_Pop_Obes_GDP_F-Age.csv")



A1<-read.csv("FPro_predicted_plus_food_Finland_15_nutr.csv",header=TRUE)
A1<-A1[,-1]
A1<-as.data.frame(cbind(rep("Finland",dim(A1)[1]),A1))
colnames(A1)[1]<-"Survey.s.country"

A2<-read.csv("FPro_predicted_plus_food_United.Kingdom_15_nutr.csv",header=TRUE)
A2<-A2[,-1]
A2<-as.data.frame(cbind(rep("United Kingdom",dim(A2)[1]),A2))
colnames(A2)[1]<-"Survey.s.country"

A3<-read.csv("FPro_predicted_plus_food_France_15_nutr.csv",header=TRUE)
A3<-A3[,-1]
A3<-as.data.frame(cbind(rep("France",dim(A3)[1]),A3))
colnames(A3)[1]<-"Survey.s.country"

A4<-read.csv("FPro_predicted_plus_food_Sweden_15_nutr.csv",header=TRUE)
A4<-A4[,-1]
A4<-as.data.frame(cbind(rep("Sweden",dim(A4)[1]),A4))
colnames(A4)[1]<-"Survey.s.country"

A5<-read.csv("FPro_predicted_plus_food_Italy_15_nutr.csv",header=TRUE)
A5<-A5[,-1]
A5<-as.data.frame(cbind(rep("Italy",dim(A5)[1]),A5))
colnames(A5)[1]<-"Survey.s.country"


A6<-read.csv("FPro_predicted_plus_food_Germany_15_nutr.csv",header=TRUE)
A6<-A6[,-1]
A6<-as.data.frame(cbind(rep("Germany",dim(A6)[1]),A6))
colnames(A6)[1]<-"Survey.s.country"


A7<-read.csv("FPro_predicted_plus_food_Netherlands_15_nutr.csv",header=TRUE)
A7<-A7[,-1]
A7<-as.data.frame(cbind(rep("Netherlands",dim(A7)[1]),A7))
colnames(A7)[1]<-"Survey.s.country"

A<-rbind(A1,A2,A3,A4,A5,A6,A7)
colnames(A)[ncol(A)]="FPro"

dat1<-read.csv("Foodex_2_L7_dashboard_part_1.csv",header=TRUE)
dat2<-read.csv("Foodex_2_L7_dashboard_part_2.csv",header=TRUE)
dat3<-read.csv("Foodex_2_L7_dashboard_part_3.csv",header=TRUE)
dat4<-read.csv("Foodex_2_L7_dashboard_part_4.csv",header=TRUE)
dat5<-read.csv("Foodex_2_L7_dashboard_part_5.csv",header=TRUE)
dat6<-read.csv("Foodex_2_L7_dashboard_part_6.csv",header=TRUE)
dat7<-read.csv("Foodex_2_L7_dashboard_part_7.csv",header=TRUE)
dat8<-read.csv("Foodex_2_L7_dashboard_part_8.csv",header=TRUE)

dat<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8)
dat<-dat[,-1]

Gen<-dat[grepl("Female", dat$"Gender"), ]
E<-Gen[grepl("Adults", Gen$"Population.Group..L2."), ]
E1<-Gen[grepl("Adolescents", Gen$"Population.Group..L2."), ]
F<-rbind(E,E1)


colnames(F)[which(colnames(F)=="Exposure.hierarchy..L6.")]<-colnames(A)[2]

Foods<-unique(A[,2])
counts<-table(A[,2])
repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,2]==as.character(x)))
names(positions)<-repeated_vals

Torrs<-sort(Foods)

FPro<-array(length(Torrs))
t_score_FPro<-rep(NA,length(Torrs))
crit_t_FPro<-rep(NA,length(Torrs))
get_t_critical <- function(n, conf_level = 0.999) {
  df <- n - 1
  alpha <- 1 - conf_level
  qt(1 - alpha/2, df)
}

for (j in 1:length(Torrs)) {
Cer<-A[positions[[as.character(Torrs[j])]][1:length(positions[[as.character(Torrs[j])]])],]
FPro[j]<-median(Cer[,ncol(Cer)])
if (nrow(Cer)>1) {
t_score_FPro[j]<-mean(Cer[,ncol(Cer)])/sd(Cer[,ncol(Cer)])
crit_t_FPro[j]<-get_t_critical(nrow(Cer))}}

Other_Country_Food_FPro<-as.data.frame(cbind(Torrs,FPro))
colnames(Other_Country_Food_FPro)<-c(colnames(A)[2],"Median_FPro")

M0<-merge(F,Other_Country_Food_FPro,by=colnames(A)[2])
Countries0<-unique(A[,"Survey.s.country"])
M1<-M0[!M0[,"Survey.s.country"] %in%Countries0,]
Countries<-unique(M1[,"Survey.s.country"])

G<-M1
G[,"Number.of.subjects"]<-as.numeric(gsub(",", "", G[,"Number.of.subjects"]))
G[,"Mean"]<-as.numeric(gsub(",", "", G[,"Mean"]))

Mat<-array(rep(NA,length(Countries)))
Mat1<-data.frame(cbind(Countries,Mat))
colnames(Mat1)<-c("Country","FPro")
Mat2<-data.frame(cbind(Countries,Mat))
colnames(Mat2)<-c("Country","95CI_FPro_half-width")

for (i in 1:length(Countries)) {
G1<-G[grepl(Countries[i], G$"Survey.s.country"), ]
G2<-G1

x_vals <- c()
w_vals <- c()
for (k in 1:nrow(G2)) {
  if ((G2[k,"Exposure.hierarchy..L1."] != "Water and water-based beverages") &&
      (G2[k,"Exposure.hierarchy..L1."] != "Alcoholic beverages")) {

    w <- as.numeric(G2[k,"Number.of.subjects"]) * as.numeric(G2[k,"Mean"])
    x <- as.numeric(G2[k,"Median_FPro"])

    w_vals <- c(w_vals, w)
    x_vals <- c(x_vals, x)
  }
}
weighted_mean <- sum(w_vals * x_vals) / sum(w_vals)
weighted_var <- sum(w_vals * (x_vals - weighted_mean)^2) / sum(w_vals)
n_eff <- (sum(w_vals))^2 / sum(w_vals^2)
SE <- sqrt(weighted_var / n_eff)
CI_low <- weighted_mean - 1.96 * SE
CI_high <- weighted_mean + 1.96 * SE
Mat1[i,2] <- weighted_mean
Mat2[i,2] <- CI_high - weighted_mean
}

Mat3<-merge(Mat1,Mat2,by="Country")

write.csv(Mat3,"Weighted_Average_FPro_Food_wise_for_the_rest_19_Coutries_of_merged_Years_Hierarchy_6.csv")

G<-M1
G[,"Number.of.subjects"]<-as.numeric(gsub(",", "", G[,"Number.of.subjects"]))
G[,"Mean"]<-as.numeric(gsub(",", "", G[,"Mean"]))
Countries<-unique(G[,"Survey.s.country"])
Years<-unique(G[,"Survey.start.year"])
Years<-sort(Years)
Mat<-matrix(rep(NA,length(Countries)*length(Years)),nrow=length(Countries))
Mat1<-data.frame(cbind(Countries,Mat))
colnames(Mat1)[2:ncol(Mat1)]<-Years
Mat2<-data.frame(cbind(Countries,Mat))
colnames(Mat2)[2:ncol(Mat2)]<-Years


for (i in 1:length(Countries)) {
G1<-G[grepl(Countries[i], G$"Survey.s.country"), ]
Survey_years<-unique(G1[,"Survey.start.year"])

for (j in 1:length(Survey_years)) {
G2<-G1[grepl(Survey_years[j], G1$"Survey.start.year"), ]

x_vals <- c()
w_vals <- c()
for (k in 1:nrow(G2)) {
  if ((G2[k,"Exposure.hierarchy..L1."] != "Water and water-based beverages") &&
      (G2[k,"Exposure.hierarchy..L1."] != "Alcoholic beverages")) {

    w <- as.numeric(G2[k,"Number.of.subjects"]) * as.numeric(G2[k,"Mean"])
    x <- as.numeric(G2[k,"Median_FPro"])

    w_vals <- c(w_vals, w)
    x_vals <- c(x_vals, x)
  }
}
weighted_mean <- sum(w_vals * x_vals) / sum(w_vals)
weighted_var <- sum(w_vals * (x_vals - weighted_mean)^2) / sum(w_vals)
n_eff <- (sum(w_vals))^2 / sum(w_vals^2)
SE <- sqrt(weighted_var / n_eff)
CI_low <- weighted_mean - 1.96 * SE
CI_high <- weighted_mean + 1.96 * SE
Mat1[i, which(colnames(Mat1) == Survey_years[j])] <- weighted_mean
Mat2[i, which(colnames(Mat2) == Survey_years[j])] <- CI_high - weighted_mean
}}

Set<-c()
for (i in 1:nrow(Mat1)) {
for (j in 2:ncol(Mat1)) {
if (is.na(Mat1[i,j])==FALSE) {
Set<-rbind(Set,cbind(Mat1[i,1],colnames(Mat1)[j],Mat1[i,j]))}
}}

Mat1_2<-as.data.frame(Set)
colnames(Mat1_2)<-c("Country","Year","FPro_Weighted_Average")

Set2<-c()
for (i in 1:nrow(Mat2)) {
for (j in 2:ncol(Mat2)) {
if (is.na(Mat2[i,j])==FALSE) {
Set2<-rbind(Set2,cbind(Mat2[i,1],colnames(Mat2)[j],Mat2[i,j]))}
}}

Mat2_2<-as.data.frame(Set2)
colnames(Mat2_2)<-c("Country","Year","95%_CI_half-margin")

Mat3<-merge(Mat1_2,Mat2_2,by=c("Country","Year"))

write.csv(Mat3,"Country_and_Year_FPro_Food_wise_for_the_rest_19_Coutries.csv")

A<-read.csv("Country_and_Year_FPro_Food_wise_for_the_rest_19_Coutries.csv",header=TRUE)
A<-A[,-1]
A2<-read.csv("Other_secondary_source_IHME_GLOBAL_ANEMIA_2000_2019_ALL_ANEMIA_PREV_ADMIN_0_Y2020M08D31.CSV",header=TRUE)
A2<-A2[,-1] #Anemia prevalence is 
Anaemia<-cbind(A2[,"ADM0_NAME"],A2[,"year"],A2[,"mean"])
colnames(Anaemia)<-c("Country","Year","An_prev")

Y<-merge(A,Anaemia,by=c("Country","Year"))
Croatia2021<-cbind(A[8,],0.11082); colnames(Croatia2021)[ncol(Croatia2021)]<-"An_prev"
Spain1998<-cbind(A[29,],0.11883); colnames(Spain1998)[ncol(Spain1998)]<-"An_prev"
Spain1999<-cbind(A[30,],0.11635); colnames(Spain1999)[ncol(Spain1999)]<-"An_prev"
Ireland1997<-cbind(A[18,],0.09186); colnames(Ireland1997)[ncol(Ireland1997)]<-"An_prev"

Y1<-data.frame(rbind(Y[1:7,],Croatia2021,Y[8:16,],Ireland1997,Y[17:26,],Spain1998,Spain1999,Y[27:30,]))

B<-read.csv("Population_and_other.csv")
B1<-B[,1:4]
B1<-cbind(B1,B[,ncol(B)]); colnames(B1)[ncol(B1)]<-"Female_age"
B2<-B1[,-2]
Croatia2021_pop<-cbind(Y1[8,],3878981,41.7) 
colnames(Croatia2021_pop)[ncol(Croatia2021_pop)-1]<-"Total.Population"
colnames(Croatia2021_pop)[ncol(Croatia2021_pop)]<-"Female_age"
Spain1998_pop<-cbind(Y1[29,],40223509,36.1) #approx from 2000
colnames(Spain1998_pop)[ncol(Spain1998_pop)-1]<-"Total.Population"
colnames(Spain1998_pop)[ncol(Spain1998_pop)]<-"Female_age"
Spain1999_pop<-cbind(Y1[30,],40386875,36.1) #approx from 2000 
colnames(Spain1999_pop)[ncol(Spain1999_pop)-1]<-"Total.Population"
colnames(Spain1999_pop)[ncol(Spain1999_pop)]<-"Female_age"
Ireland1997_pop<-cbind(Y1[18,],3674171,32) 
colnames(Ireland1997_pop)[ncol(Ireland1997_pop)-1]<-"Total.Population"
colnames(Ireland1997_pop)[ncol(Ireland1997_pop)]<-"Female_age"
# based on https://fred.stlouisfed.org/data/POPTOTSEA647NWDB?utm_source=chatgpt.com
# https://www.cso.ie/en/releasesandpublications/ep/p-cpini/irelandandnorthernireland-ajointcensuspublication2021-2022/populationandage/?utm_source=chatgpt.com

Y1a<-merge(Y1,B2,by=c("Country","Year"))
Y1b<-Y1a
Y1b[,ncol(Y1b)-1]<-gsub(",","",Y1a[,ncol(Y1a)-1])

Y2<-data.frame(rbind(Y1b[1:7,],Croatia2021_pop,Y1b[8:16,],Ireland1997_pop,Y1b[17:26,],Spain1998_pop,Spain1999_pop,Y1b[27:30,]))

Y2a<-Y2[!colnames(Y2) %in% c("X95._CI_half.margin") ]

Q1<-read.csv("BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_1.csv",header=TRUE)
Q2<-read.csv("BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_1.csv",header=TRUE)
Q3<-read.csv("BMI_NCD_RisC_Lancet_2024_BMI_female_age_specific_country_part_1.csv",header=TRUE)
Q<-rbind(Q1,Q2,Q3)
C<-Q[,-1]

C1<-C[,1:5]
C1<-C1[,-2]
colnames(C1)[2]<-"Country"
C1<-C1[!colnames(C1) %in% c("ISO")]
C2<-cbind(C1,C[,"Prevalence.of.BMI..30.kg.m...obesity."])
colnames(C2)[ncol(C2)]<-"Obesity"

C3<-merge(Y2a,C2,by=c("Country","Year"))




G<-C3

Countries<-unique(G[,"Country"])
Years<-unique(G[,"Year"])
Years<-sort(Years)
Mat<-matrix(rep(0,length(Countries)*length(Years)),nrow=length(Countries))
Mat1<-data.frame(cbind(Countries,as.matrix(Mat)))
colnames(Mat1)[2:ncol(Mat1)]<-Years

for (i in 1:length(Countries)) {
G1<-G[grepl(Countries[i], G$"Country"), ]

Survey_years<-unique(G1[,"Year"])

for (k in 1:length(Survey_years)) {

G2<-G1[grepl(Survey_years[k], G1$"Year"), ]
w<-0
br<-0
for (j in 1:nrow(G2)) {
if (G2$"Age.group"[j]=="18-19") {
Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])]<-as.numeric(Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])])+2*G2$"Obesity"[j]
w<-w+2
br<-br+1}
if (G2$"Age.group"[j]%in%c("20-24","25-29","30-34","35-39","40-44","45-49")) {
Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])]<-as.numeric(Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])])+5*G2$"Obesity"[j]
w<-w+5
br<-br+1}
}
Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])]<-as.numeric(Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[k])])/w
}}

Set<-c()
for (i in 1:nrow(Mat1)) {
for (j in 2:ncol(Mat1)) {
if (as.numeric(Mat1[i,j])!=0) {
Set<-rbind(Set,cbind(Mat1[i,1],colnames(Mat1)[j],Mat1[i,j]))}
}}

Mat2<-as.data.frame(Set)
colnames(Mat2)<-c("Country","Year","Obesity")

Y3<-merge(Y2a,Mat2,by=c("Country","Year"))

D<-read.csv("GDP_per_capita_API_NY.GDP.PCAP.CD_DS2_en_csv_v2_127149_header_solved.csv",header=TRUE)
colnames(D)<-gsub("X","",colnames(D))
D1<-D[,-c(2,3,4)]

Mat<-matrix(rep(0,length(Countries)*length(Years)),nrow=length(Countries))
Mat1<-data.frame(cbind(Countries,as.matrix(Mat)))
colnames(Mat1)[2:ncol(Mat1)]<-Years

for (i in 1: length(Countries)) {

H<-D1[grepl(Countries[i], D1$"Country"), ]

H1<-Y3[grepl(Countries[i], Y3$"Country"), ]

Survey_years<-unique(H1[,"Year"])

for (j in 1:length(Survey_years)) {
Mat1[i,1+which(as.numeric(colnames(Mat1)[2:ncol(Mat1)])==Survey_years[j])] <-H[match(Survey_years[j],colnames(H))]
}
}

Set<-c()
for (i in 1:nrow(Mat1)) {
for (j in 2:ncol(Mat1)) {
if (as.numeric(Mat1[i,j])!=0) {
Set<-rbind(Set,cbind(Mat1[i,1],colnames(Mat1)[j],Mat1[i,j]))}
}}

Mat2<-as.data.frame(Set)
colnames(Mat2)<-c("Country","Year","GDP_pc")

Y4<-merge(Y3,Mat2,by=c("Country","Year"))

A<-read.csv("Country_Year_FPro_Anaemia_Pop_Obes_GDP_F-Age.csv",header=TRUE)
A<-A[,-1]

colnames(Y4)<-colnames(A)

write.csv(Y4,"19_rest_Countries_Year_FPro_Anaemia_Pop_Obes_GDP_F-Age.csv")

source("Sol_27_08_2025.R")

write.csv(data3,"All_Var&Confounder_data.csv")










