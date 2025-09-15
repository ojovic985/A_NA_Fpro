A<-read.csv("ndns_rp_yr1-4a_indiv_uk.tab",sep="	",header=TRUE)

fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"age"])==FALSE)&&(A[i,"age"]>14)&&(A[i,"Hb"]>0)&&(is.na(A[i,"Hb"])==FALSE)&&(A[i,"bmiok"]==1)&&(is.na(A[i,"bmiok"])==FALSE) ) { # with &&(A[i,"age"]<50) -> F15-49, without -> F15+
fe<-c(fe,c(i))}}

A1<-A[fe,]
B1<-read.csv("ID_and_Weigh_FPro_for_ndns_rp_yr1-4a.csv",header=TRUE)

B1<-B1[,-1]
colnames(B1)<-c("seriali","FPro")
C1<-merge(A1,B1,by="seriali")

D1<-cbind(C1[,"seriali"], C1[,"age"], C1[,"Hb"], C1[,"FPro"],C1[,"bmival"],C1[,"cigsta3"], C1[,"dnoft3"], C1[,"ethgr2"], C1[,"qual7"],C1[,"GenHelf"])

colnames(D1)<-c("seriali","age","Hb","FPro","bmival","cigsta3","dnoft3","ethgr2","qual7","GenHelf")

F1<-D1[-which(D1[,"cigsta3"]==-1),]
G1<-F1[-which(F1[,"qual7"]==-1),]

H1<-G1
anaemia<-rep(0,nrow(H1))
smoking<-rep(0,nrow(H1))
alcohol<-rep(0,nrow(H1))
degree<-rep(0,nrow(H1))
Imperf_health<-rep(0,nrow(H1))

for (j in 1:nrow(G1)) {

if (G1[j,"Hb"]<12) {
anaemia[j]<-1}

if (G1[j,"cigsta3"]==1) {
smoking[j]<-1}

if (G1[j,"dnoft3"]==1) {
alcohol[j]<-1}

if (G1[j,"qual7"]==1) {
degree[j]<-1}

if (G1[j,"GenHelf"]!=1) {
Imperf_health[j]<-1}
}

G1<-as.data.frame(G1)
H1<-G1[!colnames(G1) %in% c("Hb","cigsta3","dnoft3","qual7","GenHelf")]

H1<-cbind(H1,t(t(anaemia)),t(t(smoking)),t(t(alcohol)),t(t(degree)),t(t(Imperf_health)))
colnames(H1)[(ncol(H1)-4):ncol(H1)]<-c("anaemia","smoking","alcohol","degree","Imperf_health")

write.csv(H1,"Conf_set_ndns_rp_yr1-4a_AllF15_plus_ImpfHeal.csv")



A2<-read.csv("ndns_rp_yr5-6a_indiv.tab",sep="	",header=TRUE)
A<-A2
fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"age"])==FALSE)&&(A[i,"age"]>14)&&(A[i,"Hb"]>0)&&(is.na(A[i,"Hb"])==FALSE)&&(A[i,"bmiok"]==1)&&(is.na(A[i,"bmiok"])==FALSE) ) { # with &&(A[i,"age"]<50) -> F15-49, without -> F15+
fe<-c(fe,c(i))}}
A2<-A[fe,]

A1<-A2
B1<-read.csv("ID_and_Weigh_FPro_for_ndns_rp_yr5-6a.csv",header=TRUE)

B1<-B1[,-1]
colnames(B1)<-c("seriali","FPro")
C1<-merge(A1,B1,by="seriali")

D1<-cbind(C1[,"seriali"], C1[,"age"], C1[,"Hb"], C1[,"FPro"],C1[,"bmival"],C1[,"cigsta3"], C1[,"dnoft3"], C1[,"ethgrp2"], C1[,"qual7"],C1[,"GenHelf"])
colnames(D1)<-c("seriali","age","Hb","FPro","bmival","cigsta3","dnoft3","ethgr2","qual7","GenHelf")

F1<-D1[-which(D1[,"cigsta3"]==-1),]
G1<-F1[-which(F1[,"qual7"]==-1),]

H1<-G1
anaemia<-rep(0,nrow(H1))
smoking<-rep(0,nrow(H1))
alcohol<-rep(0,nrow(H1))
degree<-rep(0,nrow(H1))
Imperf_health<-rep(0,nrow(H1))

for (j in 1:nrow(G1)) {

if (G1[j,"Hb"]<12) {
anaemia[j]<-1}

if (G1[j,"cigsta3"]==1) {
smoking[j]<-1}

if (G1[j,"dnoft3"]==1) {
alcohol[j]<-1}

if (G1[j,"qual7"]==1) {
degree[j]<-1}

if (G1[j,"GenHelf"]!=1) {
Imperf_health[j]<-1}
}

G1<-as.data.frame(G1)
H1<-G1[!colnames(G1) %in% c("Hb","cigsta3","dnoft3","qual7","GenHelf")]
H1<-cbind(H1,t(t(anaemia)),t(t(smoking)),t(t(alcohol)),t(t(degree)),t(t(Imperf_health)))
colnames(H1)[(ncol(H1)-4):ncol(H1)]<-c("anaemia","smoking","alcohol","degree","Imperf_health")

write.csv(H1,"Conf_set_ndns_rp_yr5-6a_AllF15_plus_ImpfHeal.csv")




A3<-read.csv("ndns_rp_yr7-8a_indiv.tab",sep="	",header=TRUE)
A<-A3
fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"age"])==FALSE)&&(A[i,"age"]>14)&&(A[i,"Hb"]>0)&&(is.na(A[i,"Hb"])==FALSE)&&(A[i,"bmiok"]==1)&&(is.na(A[i,"bmiok"])==FALSE) ) { # with &&(A[i,"age"]<50) -> F15-49, without -> F15+
fe<-c(fe,c(i))}}
A3<-A[fe,]
A1<-A3
B1<-read.csv("ID_and_Weigh_FPro_for_ndns_rp_yr7-8a.csv",header=TRUE)

B1<-B1[,-1]
colnames(B1)<-c("seriali","FPro")
C1<-merge(A1,B1,by="seriali")

D1<-cbind(C1[,"seriali"], C1[,"age"], C1[,"Hb"], C1[,"FPro"],C1[,"bmival"],C1[,"cigsta3"], C1[,"dnoft3"], C1[,"ethgrp2"], C1[,"qual7"],C1[,"GenHelf"])
colnames(D1)<-c("seriali","age","Hb","FPro","bmival","cigsta3","dnoft3","ethgr2","qual7","GenHelf")

F1<-D1[-which(D1[,"cigsta3"]==-1),]
G1<-F1[-which(F1[,"qual7"]==-1),]

H1<-G1
anaemia<-rep(0,nrow(H1))
smoking<-rep(0,nrow(H1))
alcohol<-rep(0,nrow(H1))
degree<-rep(0,nrow(H1))
Imperf_health<-rep(0,nrow(H1))

for (j in 1:nrow(G1)) {

if (G1[j,"Hb"]<12) {
anaemia[j]<-1}

if (G1[j,"cigsta3"]==1) {
smoking[j]<-1}

if (G1[j,"dnoft3"]==1) {
alcohol[j]<-1}

if (G1[j,"qual7"]==1) {
degree[j]<-1}

if (G1[j,"GenHelf"]!=1) {
Imperf_health[j]<-1}
}

G1<-as.data.frame(G1)

H1<-G1[!colnames(G1) %in% c("Hb","cigsta3","dnoft3","qual7","GenHelf")]

H1<-cbind(H1,t(t(anaemia)),t(t(smoking)),t(t(alcohol)),t(t(degree)),t(t(Imperf_health)))
colnames(H1)[(ncol(H1)-4):ncol(H1)]<-c("anaemia","smoking","alcohol","degree","Imperf_health")

write.csv(H1,"Conf_set_ndns_rp_yr7-8a_AllF15_plus_ImpfHeal.csv")



A4<-read.csv("ndns_rp_yr9-11a_indiv_20211020.tab",sep="	",header=TRUE)
A<-A4
fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"AgeR"])==FALSE)&&(A[i,"AgeR"]>14)&&(A[i,"Hb"]>0)&&(is.na(A[i,"Hb"])==FALSE)&&(A[i,"bmiok"]==1)&&(is.na(A[i,"bmiok"])==FALSE) ) { # with &&(A[i,"age"]<50) -> F15-49, without -> F15+
fe<-c(fe,c(i))}}
A4<-A[fe,]
A1<-A4
B1<-read.csv("ID_and_Weigh_FPro_for_ndns_rp_yr9a.csv",header=TRUE)

B1<-B1[,-1]
colnames(B1)<-c("seriali","FPro")
C1<-merge(A1,B1,by="seriali")

D1<-cbind(C1[,"seriali"], C1[,"AgeR"], C1[,"Hb"], C1[,"FPro"],C1[,"bmival2"], C1[,"cigsta3"], C1[,"dnoft3"], C1[,"ethgrp2"], C1[,"qual7"], C1[,"GenHelf"])
colnames(D1)<-c("seriali","age","Hb","FPro","bmival","cigsta3","dnoft3","ethgr2","qual7","GenHelf")

F1<-D1[-which(D1[,"cigsta3"]==-1),]
G1<-F1[-which(F1[,"qual7"]==-1),]

H1<-G1

anaemia<-rep(0,nrow(H1))
smoking<-rep(0,nrow(H1))
alcohol<-rep(0,nrow(H1))
degree<-rep(0,nrow(H1))
Imperf_health<-rep(0,nrow(H1))

for (j in 1:nrow(G1)) {

if (G1[j,"Hb"]<12) {
anaemia[j]<-1}

if (G1[j,"cigsta3"]==1) {  
smoking[j]<-1}

if (G1[j,"dnoft3"]==1) { 
alcohol[j]<-1}

if (G1[j,"qual7"]==1) {
degree[j]<-1}

if (G1[j,"GenHelf"]!=1) {
Imperf_health[j]<-1}
}

G1<-as.data.frame(G1)
H1<-G1[!colnames(G1) %in% c("Hb","cigsta3","dnoft3","qual7","GenHelf")]

H1<-cbind(H1,t(t(anaemia)),t(t(smoking)),t(t(alcohol)),t(t(degree)),t(t(Imperf_health)))
colnames(H1)[(ncol(H1)-4):ncol(H1)]<-c("anaemia","smoking","alcohol","degree","Imperf_health")

write.csv(H1,"Conf_set_ndns_rp_yr9a_AllF15_plus_ImpfHeal.csv")


A5<-read.csv("ndns_rp_yr9-11a_indiv_20211020.tab",sep="	",header=TRUE)
A<-A5
fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"AgeR"])==FALSE)&&(A[i,"AgeR"]>14)&&(A[i,"Hb"]>0)&&(is.na(A[i,"Hb"])==FALSE)&&(A[i,"bmiok"]==1)&&(is.na(A[i,"bmiok"])==FALSE) ) { # with &&(A[i,"age"]<50) -> F15-49, without -> F15+
fe<-c(fe,c(i))}}
A5<-A[fe,]
A1<-A5
B1<-read.csv("ID_and_Weigh_FPro_for_ndns_rp_yr10a.csv",header=TRUE)

B1<-B1[,-1]
colnames(B1)<-c("seriali","FPro")
C1<-merge(A1,B1,by="seriali")

D1<-cbind(C1[,"seriali"], C1[,"AgeR"], C1[,"Hb"], C1[,"FPro"],C1[,"bmival2"], C1[,"cigsta3"], C1[,"dnoft3"], C1[,"ethgrp2"], C1[,"qual7"], C1[,"GenHelf"])
colnames(D1)<-c("seriali","age","Hb","FPro","bmival","cigsta3","dnoft3","ethgr2","qual7","GenHelf")

F1<-D1[-which(D1[,"cigsta3"]==-1),]
G1<-F1

H1<-G1

anaemia<-rep(0,nrow(H1))
smoking<-rep(0,nrow(H1))
alcohol<-rep(0,nrow(H1))
degree<-rep(0,nrow(H1))
Imperf_health<-rep(0,nrow(H1))

for (j in 1:nrow(G1)) {

if (G1[j,"Hb"]<12) {
anaemia[j]<-1}

if (G1[j,"cigsta3"]==1) {  ## valid for years9&10 but not for 11
smoking[j]<-1}

if (G1[j,"dnoft3"]==1) { ## valid for years9&10 but not for 11
alcohol[j]<-1}

if (G1[j,"qual7"]==1) {
degree[j]<-1}

if (G1[j,"GenHelf"]!=1) {
Imperf_health[j]<-1}

}

G1<-as.data.frame(G1)
H1<-G1[!colnames(G1) %in% c("Hb","cigsta3","dnoft3","qual7","GenHelf")]

H1<-cbind(H1,t(t(anaemia)),t(t(smoking)),t(t(alcohol)),t(t(degree)),t(t(Imperf_health)))
colnames(H1)[(ncol(H1)-4):ncol(H1)]<-c("anaemia","smoking","alcohol","degree","Imperf_health")

write.csv(H1,"Conf_set_ndns_rp_yr10a_AllF15_plus_ImpfHeal.csv")


A6<-read.csv("ndns_rp_yr9-11a_indiv_20211020.tab",sep="	",header=TRUE)
A<-A6
fe<-c()
for (i in 1:nrow(A)) {
if ( (A[i,"Sex"]==2)&&(is.na(A[i,"Sex"])==FALSE)&&(is.na(A[i,"AgeR"])==FALSE)&&(A[i,"AgeR"]>14)&&(A[i,"Hb"]>0)&&(is.na(A[i,"Hb"])==FALSE)&&(A[i,"bmiok"]==1)&&(is.na(A[i,"bmiok"])==FALSE) ) { # with &&(A[i,"age"]<50) -> F15-49, without -> F15+
fe<-c(fe,c(i))}}
A6<-A[fe,]
A1<-A6
B1<-read.csv("ID_and_Weigh_FPro_for_ndns_rp_yr11a.csv",header=TRUE)

B1<-B1[,-1]
colnames(B1)<-c("seriali","FPro")
C1<-merge(A1,B1,by="seriali")

D1<-cbind(C1[,"seriali"], C1[,"AgeR"], C1[,"Hb"], C1[,"FPro"],C1[,"bmival2"], C1[,"cigsta3"], C1[,"dnoft3"], C1[,"ethgrp2"], C1[,"qual7"], C1[,"GenHelf"]) # or "ethgr2" "ethgrp2"
colnames(D1)<-c("seriali","age","Hb","FPro","bmival","cigsta3","dnoft3","ethgr2","qual7","GenHelf")

G1<-D1[-which(D1[,"qual7"]==-1),]

H1<-G1

anaemia<-rep(0,nrow(H1))
smoking<-rep(NA,nrow(H1))
alcohol<-rep(NA,nrow(H1))
degree<-rep(0,nrow(H1))
Imperf_health<-rep(0,nrow(H1))

for (j in 1:nrow(G1)) {

if (G1[j,"Hb"]<12) {
anaemia[j]<-1}

if (G1[j,"qual7"]==1) {
degree[j]<-1}

if (G1[j,"GenHelf"]!=1) {
Imperf_health[j]<-1}

}

G1<-as.data.frame(G1)
H1<-G1[!colnames(G1) %in% c("Hb","cigsta3","dnoft3","qual7","GenHelf")]

H1<-cbind(H1,t(t(anaemia)),t(t(smoking)),t(t(alcohol)),t(t(degree)),t(t(Imperf_health)))
colnames(H1)[(ncol(H1)-4):ncol(H1)]<-c("anaemia","smoking","alcohol","degree","Imperf_health")

write.csv(H1,"Conf_set_ndns_rp_yr11a_AllF15_plus_ImpfHeal.csv")
















