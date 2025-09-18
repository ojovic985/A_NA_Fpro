
QQ1<-read.csv("fsd-full-export-2025-07-15_part_1.csv")
QQ2<-read.csv("fsd-full-export-2025-07-15_part_2.csv")
QQ3<-read.csv("fsd-full-export-2025-07-15_part_3.csv")
QQ4<-read.csv("fsd-full-export-2025-07-15_part_4.csv")

A1<-rbind(QQ1,QQ2,QQ3,QQ4)

E<-unique(A1[,1])
GH<-A1[grepl("Anemia in women 15-49 years \\(SDG 2.2.3\\)",A1$"Indicator"),]

GA<-A1[grepl("Total population",A1$"Indicator"),]
GB<-A1[grepl("Underweight in women 15-49 years",A1$"Indicator"),]
GC<-A1[grepl("Annual growth in GNI per capita",A1$"Indicator"),]
GD<-A1[grepl("Annual population growth",A1$"Indicator"),]
GE<-A1[grepl("Adult diabetes prevalence",A1$"Indicator"),] # data for only 39 out of 52 countries
GF<-A1[grepl("Adult overweight",A1$"Indicator"),]
#GG<-A1[grepl("Adults: Consumption of at least one animal-source food",A1$"Indicator"),] too few info
#GJ<-A1[grepl("Adults: Nuts or seeds consumption",A1$"Indicator"),]
#GI<-A1[grepl("Adults: Processed meat consumption",A1$"Indicator"),]
#GK<-A1[grepl("Adults: Salty packaged snacks, instant noodles, or fast food consumption",A1$"Indicator"),]
GL<-A1[grepl("Adult raised blood pressure",A1$"Indicator"),] # data for only 37 out of 52 countries
#GO<-A1[grepl("Adults: Sweet foods consumption",A1$"Indicator"),]
#GP<-A1[grepl("Adults: Zero fruit or vegetable consumption",A1$"Indicator"),]
#GR<-A1[grepl("Adults: Unprocessed red meat consumption",A1$"Indicator"),]
#GS<-A1[grepl("Adults: Whole grains consumption",A1$"Indicator"),]

#> unique(GH[,"Unit"])
#[1] "%"
#> T<-GH[,"End.Year"] - GH[,"Start.Year"]
#> unique(T)
#[1] 0
#> unique(GH[,"Source"])
#[1] "WHO"
#> cor(data3$Tot_Pop,data3$Population)
#[1] 0.9999427

GA2<-GA[colnames(GA) %in% c("Start.Year","Country","Value")]
GA2$Country<-gsub("Netherlands \\(Kingdom of the\\)","Netherlands",GA2$"Country")
GA2$Country<-gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom",GA2$"Country")
colnames(GA2)[2]<-"Year"
colnames(GA2)[3]<-"Tot_Pop"

GB2<-GB[colnames(GB) %in% c("Start.Year","Country","Value")]
GB2$Country<-gsub("Netherlands \\(Kingdom of the\\)","Netherlands",GB2$"Country")
GB2$Country<-gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom",GB2$"Country")
colnames(GB2)[2]<-"Year"
colnames(GB2)[3]<-"Underweight"

GC2<-GC[colnames(GC) %in% c("Start.Year","Country","Value")]
GC2$Country<-gsub("Netherlands \\(Kingdom of the\\)","Netherlands",GC2$"Country")
GC2$Country<-gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom",GC2$"Country")
colnames(GC2)[2]<-"Year"
colnames(GC2)[3]<-"Growth_GNI"

GD2<-GD[colnames(GD) %in% c("Start.Year","Country","Value")]
GD2$Country<-gsub("Netherlands \\(Kingdom of the\\)","Netherlands",GD2$"Country")
GD2$Country<-gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom",GD2$"Country")
colnames(GD2)[2]<-"Year"
colnames(GD2)[3]<-"Pop_Growth"

GE2<-GE[colnames(GE) %in% c("Start.Year","Country","Value")]
GE2$Country<-gsub("Netherlands \\(Kingdom of the\\)","Netherlands",GE2$"Country")
GE2$Country<-gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom",GE2$"Country")
colnames(GE2)[2]<-"Year"
colnames(GE2)[3]<-"Diabetes"

GF2<-GF[colnames(GF) %in% c("Start.Year","Country","Value")]
GF2$Country<-gsub("Netherlands \\(Kingdom of the\\)","Netherlands",GF2$"Country")
GF2$Country<-gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom",GF2$"Country")
colnames(GF2)[2]<-"Year"
colnames(GF2)[3]<-"Overweight"

GH2<-GH[colnames(GH) %in% c("Start.Year","Country","Value")]
GH2$Country<-gsub("Netherlands \\(Kingdom of the\\)","Netherlands",GH2$"Country")
GH2$Country<-gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom",GH2$"Country")
colnames(GH2)[2]<-"Year"

A<-read.csv("Country_Year_FPro_Anaemia_Pop_Obes_GDP_F-Age.csv",header=TRUE)
A<-A[,-1]

library(stats)

B<-read.csv("19_rest_Countries_Year_FPro_Anaemia_Pop_Obes_GDP_F-Age.csv",header=TRUE)
B<-B[,-1]

C<-rbind(A,B)

library(stats)

data<-as.data.frame(C)
colnames(data)<-c("Country","Year","FPro","Sec_Source_An_prev","Population","Age","Obesity","GDP")
data_pop<-merge(data,GA2,by=c("Country","Year"))
data_pop2<-merge(data_pop,GB2,by=c("Country","Year"))
data_pop3<-merge(data_pop2,GC2,by=c("Country","Year"))
data_pop4<-merge(data_pop3,GD2,by=c("Country","Year"))
data_pop5<-merge(data_pop4,GF2,by=c("Country","Year"))

data_pop2<-data_pop5

data2<-merge(data_pop2,GH2,by=c("Country","Year"))
unmatched <- data_pop2[!paste(data_pop2$Country, data_pop2$Year) %in% 
                    paste(GH2$Country, GH2$Year), ]

#> unmatched
#   Country Year      FPro       B Population   Age    Obesity      GDP  Tot_Pop
#8  Croatia 2021 0.6096315 0.11082    3878981 41.70 0.18663250 17789.93  3878981
#25 Ireland 1997 0.6283520 0.09186    3674171 32.00 0.14032927 22551.11  3674171
#41   Spain 1998 0.6861935 0.11883   40223509 36.10 0.11624325 15382.33 40223509
#42   Spain 1999 0.6273044 0.11635   40386875 36.10 0.11772209 15707.95 40386875
#47  Sweden 1997 0.6798569 0.06560    8846062 37.84 0.08872111 30324.45  8846062
 

#Value<-c(12.6,21,11,11.5,11.5) #Variant 1 as used as closest country information from sheets e.g. 
#Spain1998 as Spain2000 or Ireland1997 as Ireland2000

Value<-c(21.1,11.3,11.6,11.5,13.1) #Variant 2 from linear regression on 27_08_2025_using five closest country information from sheets e.g. Spain1998 as predicted from Spain2000-2004

unmatched2<-cbind(unmatched,Value)
colnames(unmatched2)[ncol(unmatched2)]<-"Value"
data3<-rbind(data2, unmatched2)
data3$Tot_Pop <- as.numeric(data3$Tot_Pop)
data3$Underweight <- as.numeric(data3$Underweight)
data3$Growth_GNI <- as.numeric(data3$Growth_GNI)
data3$Pop_Growth <- as.numeric(data3$Pop_Growth)
data3$Overweight <- as.numeric(data3$Overweight)

