#Open R in folder FNDDS_Fpro_calc_prepared and copy-paste code below!


Disjunkcija2<-function(X1,X2){

X1X2<-c()
I<-c()
J<-c()
for (i in 1:dim(X1)[1]) {
if (is.na(match(X1["Food code"][i,1],X2["Food code"][,1]))==TRUE) {
I<-cbind(I,i)}}
X1X2<-cbind(X1[I,])
return(X1X2)}

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

A<-read.csv("../Data_3099_food_codes_g_per_100g.csv",header=TRUE)
A<-A[,-1]

colnames(A)[ncol(A)]<-"Food code"
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
E2<-cbind(exp(E1[,1:62]),E1[,c("min_FPro","max_p","class")],E1[,ncol(E1)])
colnames(E2)[ncol(E2)]<-"Food code"
if (j==1) {
X<-Disjunkcija2(E2,A)
Z<-X[,ncol(X)]}
else {
X1<-Disjunkcija2(E2,A)
Z1<-X1[,ncol(X1)]
Z<-unique(as.vector(cbind(t(Z),t(Z1))))}
}

XY<-matrix(rep(0,length(Z)*9),nrow=length(Z))
XY<-cbind(t(t(Z)),XY)


for (j in 1:length(files)) {
E<-f[[j]]
E<-E[,-1]
E1<-E
E2<-cbind(exp(E1[,1:62]),E1[,c("min_FPro","max_p","class")],E1[,ncol(E1)])
colnames(E2)[ncol(E2)]<-"Food code"
for (i in 1:length(Z)) {
if (Z[i]%in%E2[,ncol(E2)])
XY[i,j+1]<-1
}
if (j==1) {
X<-E2
fes<-nrow(E2)}
else {
X<-rbind(X,E2)
fes<-cbind(fes,nrow(E2))}
}

X<-X[,-64]


X<-as.matrix(X)
Res<-matrix(rep(0,dim(XY)[1]*65),nrow=dim(XY)[1])
Fes<- cumsum(fes)
tes<-Fes-fes+1
for (i in 1:nrow(XY)) {
br<-0
J<-c()
for (j in 2:ncol(XY)) {
if (XY[i,j]==1) {
if (br==0) {
br<-br+1
J<-tes[j-1]+match(XY[i,1],X[tes[j-1]:Fes[j-1],][,ncol(X)])-1}
else {
br<-br+1
J<-cbind(J,tes[j-1]+match(XY[i,1],X[tes[j-1]:Fes[j-1],][,ncol(X)])-1)}}}
Gh<-X[J,]
if (br==1) {
Res[i,]<-Gh}
else {
for (k in 1:63) {
Res[i,k]<-median(Gh[,k])}
Res[i,64:65]<-Gh[nrow(Gh),64:65]}}

colnames(Res)<-colnames(X)

dim(Res)
max_p<-array(dim(Res)[1])
for (i in 1:nrow(Res)) {
if (Res[i,"class"]==0) {
max_p[i]<-"p1"}
else {
if (Res[i,"class"]==1) {
max_p[i]<-"p2"}
else {
if (Res[i,"class"]==2) {
max_p[i]<-"p3"}
else {
if (Res[i,"class"]==3) {
max_p[i]<-"p4"}}}}}
max_p<-as.factor(max_p)
Res2<-cbind(Res[,1:63],max_p,Res[,64:65])

hes<-matrix(rep(0,length(Z)*2),nrow=length(Z))
hes<-cbind(t(t(Z)),hes)
hes<-hes[,-ncol(hes)]
for (i in 1:length(Z)) {
hes[i,2]<-sum(XY[i,2:ncol(XY)])
}
I<-c()
for (i in 1:length(Z)) {
if (hes[i,2]>2)
I<-cbind(I,i)
}
XXY<-Res2[I,]

write.csv(rbind(XXY,A),"Data_9450_food_codes_g_per_100g.csv")

