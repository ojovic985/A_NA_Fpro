Q<-read.csv("F15-49_ready_for_glm_2023-2013_kindg1_n21&22.csv",header=TRUE)
Q<-Q[,-1]


A<-read.csv('hn13_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn13.csv",row.names=FALSE)





A<-read.csv('hn14_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn14.csv",row.names=FALSE)







A<-read.csv('hn15_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn15.csv",row.names=FALSE)






A<-read.csv('hn16_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn16.csv",row.names=FALSE)






A<-read.csv('hn17_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn17.csv",row.names=FALSE)








A<-read.csv('hn18_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn18.csv",row.names=FALSE)







A<-read.csv('hn19_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn19.csv",row.names=FALSE)








A<-read.csv('hn20_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn20.csv",row.names=FALSE)







A<-read.csv('hn21_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn21.csv",row.names=FALSE)










A<-read.csv('hn22_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn22.csv",row.names=FALSE)











A<-read.csv('hn23_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")


write.csv(Mat,"Aval_and_Tot_Iron_F15-49_Serie_hn23.csv",row.names=FALSE)










Q<-read.csv("F50_plus_ready_for_glm_2023-2013_kindg1_n21&22.csv",header=TRUE)
Q<-Q[,-1]



A<-read.csv('hn13_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn13.csv",row.names=FALSE)





A<-read.csv('hn14_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn14.csv",row.names=FALSE)







A<-read.csv('hn15_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn15.csv",row.names=FALSE)






A<-read.csv('hn16_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn16.csv",row.names=FALSE)







A<-read.csv('hn17_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn17.csv",row.names=FALSE)








A<-read.csv('hn18_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn18.csv",row.names=FALSE)







A<-read.csv('hn19_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn19.csv",row.names=FALSE)






A<-read.csv('hn20_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn20.csv",row.names=FALSE)








A<-read.csv('hn21_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn21.csv",row.names=FALSE)








A<-read.csv('hn22_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn22.csv",row.names=FALSE)







A<-read.csv('hn23_24rc.csv',header=TRUE)


colnames(Q)[1]<-"ID"
Q1<-Q[,1:(which(grepl("N",colnames(Q)))[1]-1)]
A1<-merge(A,Q1,by="ID")

Ser1<-unique(A1[,"ID"])
Aval_iron<-rep(0,length(Ser1))
Total_iron<-rep(0,length(Ser1))

for (i in 1:length(Ser1)) {
G<-A1[grepl(Ser1[i],A1$"ID"),]
G1<-G
Meals<-unique(G1[,"N_MEAL_T"])
FE<-0
if (length(Meals)>0) {
for (k in 1:length(Meals)) {

G2<-G1[grepl(Meals[k],G1$"N_MEAL_T"),]
Tot_iron<-sum(G2[,"NF_FE"],na.rm=TRUE)
FE<-sum(G2[,"NF_VITC"],na.rm=TRUE)
if (is.na(FE)==TRUE) {
FE<-0}
NHI<-0
HI<-0
for (m in 1:nrow(G2)) {

WWEIA_class<-G2[m,"N_KINDG1"]

if ( (is.na(WWEIA_class)==FALSE)&&(is.na(G2[m,"NF_FE"])==FALSE)&&((WWEIA_class==15)||(WWEIA_class==17)) ) {
HI<-HI+0.4*G2[m,"NF_FE"]
NHI<-NHI+0.6*G2[m,"NF_FE"]
}
else {

if (is.na(G2[m,"NF_FE"])==FALSE) {
NHI<-NHI+G2[m,"NF_FE"]
}}
}

if (FE>75) {
Abs_rate_NHI<-0.08}
else {
Abs_rate_NHI<-(3+8.93*log((FE+100)/100))/100}

Aval_iron[i]<-Aval_iron[i]+Abs_rate_NHI*NHI +0.25*HI ### 
Total_iron[i]<-Total_iron[i]+Tot_iron  ## in mg!!!!!!!!!!!!
}}


} 

Mat<-data.frame(cbind(t(t(Ser1)), t(t(Aval_iron)),t(t(Total_iron))))
colnames(Mat)<-c("ID","Aval_iron","Total_iron")

write.csv(Mat,"Aval_and_Tot_Iron_F50_plus_Serie_hn23.csv",row.names=FALSE)







##### COMPLEX SURVEY DESIGN ANALYSIS #####



### Goes with Q<-read.csv("F50_plus_ready_for_glm_2023-2013_kindg1_n21&22.csv",header=TRUE) ###

B1<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn13.csv",header=TRUE)
B2<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn14.csv",header=TRUE)
B3<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn15.csv",header=TRUE)
B4<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn16.csv",header=TRUE)
B5<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn17.csv",header=TRUE)
B6<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn18.csv",header=TRUE)
B7<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn19.csv",header=TRUE)
B8<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn20.csv",header=TRUE)
B9<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn21.csv",header=TRUE)
B10<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn22.csv",header=TRUE)
B11<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn23.csv",header=TRUE)

### Goes with Q<-read.csv("F50_plus_ready_for_glm_2023-2013_kindg1_n21&22.csv",header=TRUE) ###




### Goes with Q<-read.csv("F15-49_ready_for_glm_2023-2013_kindg1_n21&22.csv",header=TRUE) ###

B1<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn13.csv",header=TRUE)
B2<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn14.csv",header=TRUE)
B3<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn15.csv",header=TRUE)
B4<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn16.csv",header=TRUE)
B5<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn17.csv",header=TRUE)
B6<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn18.csv",header=TRUE)
B7<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn19.csv",header=TRUE)
B8<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn20.csv",header=TRUE)
B9<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn21.csv",header=TRUE)
B10<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn22.csv",header=TRUE)
B11<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn23.csv",header=TRUE)

### Goes with Q<-read.csv("F15-49_ready_for_glm_2023-2013_kindg1_n21&22.csv",header=TRUE) ###


Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11)

Q<-read.csv("F15-49_ready_for_glm_2023-2013_kindg1_n21&22.csv",header=TRUE)
Q<-Q[,-1]

M1<-merge(Q,Buk,by="ID")


M2<- M1

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"Anaemia"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2

data<-data_1

library(survey)
options(survey.lonely.psu="adjust")

design_nhanes<-svydesign(id=~psu, strata=~kstrata,weights=~wt_tot,data=data,nest=TRUE)


est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)



#> coef(est) # Anem F15-49
#coef(est)
#SE(est)
#confint(est)
#Aval_iron 
#0.7140158 
#           Aval_iron
#Aval_iron 0.01484062
#              2.5 %    97.5 %
#Aval_iron 0.6849287 0.7431029


est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#> est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
#coef(est)
#SE(est)
#confint(est)
#Total_iron 
#  11.14987 
#           Total_iron
#Total_iron  0.1864807
#              2.5 %   97.5 %
#Total_iron 10.78437 11.51536 # Anem F15-49



data<-data_2

library(survey)
options(survey.lonely.psu="adjust")

design_nhanes<-svydesign(id=~psu, strata=~kstrata,weights=~wt_tot,data=data,nest=TRUE)

est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)



#> est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE) # NonAnem F15-49
#coef(est)
#SE(est)
#confint(est)
#Aval_iron 
# 0.737801 
#            Aval_iron
#Aval_iron 0.007636446
#              2.5 %    97.5 %
#Aval_iron 0.7228338 0.7527681

#> est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
#coef(est)
#SE(est)
#confint(est)
#Total_iron 
#  11.20336 See Figure S18, row 1, m(Non-A, Tot/Av-Fe) first value (before "/")
#           Total_iron
#Total_iron  0.0882939
#             2.5 %   97.5 %
#Total_iron 11.0303 11.37641 # NonAnem F15-49

data=data_3

library(survey)

data$Anaemia <- relevel(factor(data$Anaemia, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")

design_nhanes<-svydesign(id=~psu, strata=~kstrata,weights=~wt_tot,data=data,nest=TRUE)
options(survey.lonely.psu="adjust")

model <- svyglm(Aval_iron ~ Anaemia, design = design_nhanes)
summary(model)

#Call:
#svyglm(formula = Aval_iron ~ Anaemia, design = design_nhanes)

#Survey design:
#svydesign(id = ~psu, strata = ~kstrata, weights = ~wt_tot, data = data, 
#    nest = TRUE)

#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     0.71402    0.01504  47.470   <2e-16 ***
#AnaemiaNonAnem  0.02379    0.01647   1.444    0.149    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.5147827)

#Number of Fisher Scoring iterations: 2

confint(model)

#                      2.5 %    97.5 %
#(Intercept)     0.684515765 0.7435159
#AnaemiaNonAnem -0.008512277 0.0560826

summary(model)$coefficients[2,1]
#[1] 0.02378516
#> summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.03229744

svyttest(formula = Total_iron ~ Anaemia, design_nhanes)$p.value
#AnaemiaNonAnem 
#     0.7945039  ### F15-49 See Figure S18

dim(data_1)
#[1] 2122   69

dim(data_2)
#[1] 12177    69







B1<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn13.csv",header=TRUE)
B2<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn14.csv",header=TRUE)
B3<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn15.csv",header=TRUE)
B4<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn16.csv",header=TRUE)
B5<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn17.csv",header=TRUE)
B6<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn18.csv",header=TRUE)
B7<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn19.csv",header=TRUE)
B8<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn20.csv",header=TRUE)
B9<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn21.csv",header=TRUE)
B10<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn22.csv",header=TRUE)
B11<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn23.csv",header=TRUE)



Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11)

Q<-read.csv("F50_plus_ready_for_glm_2023-2013_kindg1_n21&22.csv",header=TRUE)
Q<-Q[,-1]

M1<-merge(Q,Buk,by="ID")


M2<- M1

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"Anaemia"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2

data<-data_1

library(survey)
options(survey.lonely.psu="adjust")

design_nhanes<-svydesign(id=~psu, strata=~kstrata,weights=~wt_tot,data=data,nest=TRUE)


est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)

#> est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE) # F50+ Anem
#coef(est)
#SE(est)
#confint(est)
#Aval_iron 
#0.5744667 
#           Aval_iron
#Aval_iron 0.01494516
#              2.5 %    97.5 %
#Aval_iron 0.5451747 0.6037586

#> est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
#coef(est)
#SE(est)
#confint(est)
#Total_iron 
#  9.745435 
#           Total_iron
#Total_iron  0.1725251
#              2.5 %   97.5 %
#Total_iron 9.407292 10.08358 # F50+ Anem


data<-data_2

library(survey)
options(survey.lonely.psu="adjust")

design_nhanes<-svydesign(id=~psu, strata=~kstrata,weights=~wt_tot,data=data,nest=TRUE)


est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE)
coef(est)
SE(est)
confint(est)


#> est <- svymean(~Aval_iron, design_nhanes, na.rm = TRUE) # F50+ NonAnem
#coef(est)
#SE(est)
#confint(est)
#Aval_iron 
#0.6967512 
#            Aval_iron
#Aval_iron 0.006720924
#              2.5 %   97.5 %
#Aval_iron 0.6835784 0.709924

#> est <- svymean(~Total_iron, design_nhanes, na.rm = TRUE)
#coef(est)
#SE(est)
#confint(est)
#Total_iron 
#  11.57056 
           Total_iron
#Total_iron 0.09326276
#              2.5 %   97.5 %
#Total_iron 11.38777 11.75335 # F50+ NonAnem



data=data_3

library(survey)

data$Anaemia <- relevel(factor(data$Anaemia, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")

design_nhanes<-svydesign(id=~psu, strata=~kstrata,weights=~wt_tot,data=data,nest=TRUE)
options(survey.lonely.psu="adjust")

model <- svyglm(Aval_iron ~ Anaemia, design = design_nhanes)
summary(model)

#Call:
#svyglm(formula = Aval_iron ~ Anaemia, design = design_nhanes)

#Survey design:
#svydesign(id = ~psu, strata = ~kstrata, weights = ~wt_tot, data = data, 
#    nest = TRUE)

#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     0.57447    0.01509  38.068  < 2e-16 ***
#AnaemiaNonAnem  0.12228    0.01604   7.623 3.87e-14 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.442311)

#Number of Fisher Scoring iterations: 2


confint(model)
#                    2.5 %    97.5 %
#(Intercept)    0.54487087 0.6040624
#AnaemiaNonAnem 0.09082285 0.1537462
#> summary(model)$coefficients[2,1]
#[1] 0.1222845
#> summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.03146169

svyttest(formula = Total_iron ~ Anaemia, design_nhanes)$p.value
#AnaemiaNonAnem 
#1.592016e-20 # F50+ 










##### COHORT ANALYSIS #####


B1<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn13.csv",header=TRUE)
B2<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn14.csv",header=TRUE)
B3<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn15.csv",header=TRUE)
B4<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn16.csv",header=TRUE)
B5<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn17.csv",header=TRUE)
B6<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn18.csv",header=TRUE)
B7<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn19.csv",header=TRUE)
B8<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn20.csv",header=TRUE)
B9<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn21.csv",header=TRUE)
B10<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn22.csv",header=TRUE)
B11<-read.csv("Aval_and_Tot_Iron_F15-49_Serie_hn23.csv",header=TRUE)

Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11)

Q<-read.csv("F15-49_ready_for_glm_2023-2013_kindg1_n21&22.csv",header=TRUE)
Q<-Q[,-1]


M1<-merge(Q,Buk,by="ID")


M2<- M1

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"Anaemia"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2

data<-data_1



est <- mean(data_1[,"Aval_iron"], na.rm = TRUE) # F15-49, Anem
est
#[1] 0.7212692 
est <- mean(data_1[,"Total_iron"], na.rm = TRUE) 
est
#[1] 11.30923 # F15-49, Anem

est <- mean(data_2[,"Aval_iron"], na.rm = TRUE) # F15-49, NonAnem
est
#[1] 0.7545631
est <- mean(data_2[,"Total_iron"], na.rm = TRUE) 
est
#[1] 11.48831 # F15-49, NonAnem



data=data_3 # F15-49

data$Anaemia <- relevel(factor(data$Anaemia, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")

model <- glm(Aval_iron ~ Anaemia, data=data)
summary(model)

#Call:
#glm(formula = Aval_iron ~ Anaemia, data = data)

#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     0.72127    0.01580  45.645   <2e-16 ***
#AnaemiaNonAnem  0.03329    0.01712   1.944   0.0519 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.5298405)

#    Null deviance: 7577.1  on 14298  degrees of freedom
#Residual deviance: 7575.1  on 14297  degrees of freedom
#AIC: 31500

#Number of Fisher Scoring iterations: 2

confint(model)
#Waiting for profiling to be done...
#                       2.5 %     97.5 %
#(Intercept)     0.6902987541 0.75223972
#AnaemiaNonAnem -0.0002668242 0.06685452

summary(model)$coefficients[2,1]
#[1] 0.03329385

summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.03356067

t.test(formula = Aval_iron ~ Anaemia, data=data,var.equal=TRUE)$p.value
#[1] 0.05186908

t.test(formula = Total_iron ~ Anaemia, data=data,var.equal=TRUE)$p.value
#[1] 0.4099287 # F15-49











B1<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn13.csv",header=TRUE)
B2<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn14.csv",header=TRUE)
B3<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn15.csv",header=TRUE)
B4<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn16.csv",header=TRUE)
B5<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn17.csv",header=TRUE)
B6<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn18.csv",header=TRUE)
B7<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn19.csv",header=TRUE)
B8<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn20.csv",header=TRUE)
B9<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn21.csv",header=TRUE)
B10<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn22.csv",header=TRUE)
B11<-read.csv("Aval_and_Tot_Iron_F50_plus_Serie_hn23.csv",header=TRUE)

Buk<-rbind(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11)

Q<-read.csv("F50_plus_ready_for_glm_2023-2013_kindg1_n21&22.csv",header=TRUE)
Q<-Q[,-1]

M1<-merge(Q,Buk,by="ID")


M2<- M1

fe<-c()
for(i in 1:nrow(M2)) {
if (M2[i,"Anaemia"]==1)  {
fe<-cbind(fe,i)}
}
M_anem<-M2[fe,]
M_Nonanem<-M2[-fe,]

data_1<-M_anem

data_2<-M_Nonanem

data_3<-M2

data<-data_1

est <- mean(data_1[,"Aval_iron"], na.rm = TRUE) # F50+, Anem
est
#[1] 0.5680105
est <- mean(data_1[,"Total_iron"], na.rm = TRUE) 
est
#[1] 9.745339 # F50+, Anem


est <- mean(data_2[,"Aval_iron"], na.rm = TRUE) # F50+, NonAnem
est
#[1] 0.6928749
est <- mean(data_2[,"Total_iron"], na.rm = TRUE)
est
#[1] 11.59359 # F50+, NonAnem See Table S18, row 4 (F50+, C) m(Non-A, Tot/Av-Fe) value before "/"


data=data_3 # F15-49
data$Anaemia <- relevel(factor(data$Anaemia, levels = c(0, 1),
                         labels = c("NonAnem", "Anem")), ref = "Anem")
model <- glm(Aval_iron ~ Anaemia, data=data)
summary(model)

#Call:
#glm(formula = Aval_iron ~ Anaemia, data = data)

#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     0.56801    0.01516  37.470  < 2e-16 ***
#AnaemiaNonAnem  0.12486    0.01613   7.743 1.03e-14 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.4568327)

#    Null deviance: 7827.3  on 17075  degrees of freedom
#Residual deviance: 7800.0  on 17074  degrees of freedom
#AIC: 35086

#Number of Fisher Scoring iterations: 2


confint(model)
#Waiting for profiling to be done...
#                    2.5 %    97.5 %
#(Intercept)    0.53829946 0.5977216
#AnaemiaNonAnem 0.09325643 0.1564722

summary(model)$coefficients[2,1]
#[1] 0.1248643

summary(model)$coefficients[2,1]-confint(model)[2,1]
#[1] 0.03160791

t.test(formula = Aval_iron ~ Anaemia, data=data,var.equal=TRUE)$p.value
#[1] 1.027765e-14 # F50+, C See Table S18, row 4, p-value(Tot/Av) value after "/"

t.test(formula = Total_iron ~ Anaemia, data=data,var.equal=TRUE)$p.value
#[1] 2.29328e-16 # F50+, C See Table S18, row 4, p-value(Tot/Av) value before "/"


