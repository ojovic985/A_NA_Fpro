#Open R in folder FNDDS_Fpro_calc_prepared and copy-paste code below!


Mer2<-function(X1,X2){

X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {
if (is.na(match(X1["Food code"][i,1],X2["Food code"][,1]))==FALSE) {
I<-cbind(I,i)
J<-cbind(J,match(X1["Food code"][i,1],X2["Food code"][,1]))}}
X1X2<-cbind(X1[I,],X2[J,])
return(X1X2)}

setwd(getwd()) 

files <- list.files()
f <- list()
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}

for (j in 1:length(files)) {
E<-f[[j]]
#E<-as.numeric(unlist(E))
E<-E[,-1]
E1<-E
E2<-cbind(exp(E1[,1:62]),
E1[,c("min_FPro","max_p","class")],E1[,ncol(E1)])
colnames(E2)[ncol(E2)]<-"Food code"
if (j==1) {
gh<-E2[,"Food code"]
X<-E2}
else {
X<-Mer2(X,E2)
gh<-X[,"Food code"]}
}

XY<-matrix(rep(0,dim(X)[1]*63),nrow=dim(X)[1])
for (i in 1:63) {
for (j in 1:dim(X)[1]) {
XY[j,i]<-median(as.numeric(X[j,][seq(i,594,by=66)]))
}}

XX<-cbind(XY,X[,592:594])
colnames(XX)<-colnames(X)[1:66]

write.csv(XX,"Data_3099_food_codes_g_per_100g.csv")

