#R

#Working directory in R must be '/A_NA_FPro' (the same with which prior code MAIN_github_code_Approach_1_and_2_results_for_each_p_value_3.R was launched)

#EXTREMELY IMPORTANT: FIRST RUN the command: source("MAIN_github_code_Approach_1_2_and_3_results_for_each_p_value_3.R") -THEN and only then proceed to sourcing this file!


for (ki in 1:1) {
setwd("p_0.025_")
files <- list.files()
f <- list()
if (length(files)<25) {
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}
for (j in 1:length(files)) {
E<-f[[j]]
if (j==1) {
G<-E[,2]}
else {
G<-cbind(G,E[,2])}}
Z<-data.frame(t(G))
colnames(Z)<-E[,1]
write.csv(Z,"Result_0.025.csv") }
setwd("../")}

for (ki in 1:1) {
setwd("p_0.05_")
files <- list.files()
f <- list()
if (length(files)<25) {
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}
for (j in 1:length(files)) {
E<-f[[j]]
if (j==1) {
G<-E[,2]}
else {
G<-cbind(G,E[,2])}}
Z<-data.frame(t(G))
colnames(Z)<-E[,1]
write.csv(Z,"Result_0.05.csv") }
setwd("../") }

for (ki in 1:1) {
setwd("p_0.1_")
files <- list.files()
f <- list()
if (length(files)<25) {
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}
for (j in 1:length(files)) {
E<-f[[j]]
if (j==1) {
G<-E[,2]}
else {
G<-cbind(G,E[,2])}}
Z<-data.frame(t(G))
colnames(Z)<-E[,1]
write.csv(Z,"Result_0.1.csv") }
setwd("../") }

for (ki in 1:1) {
setwd("p_0.2_")
files <- list.files()
f <- list()
if (length(files)<25) {
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}
for (j in 1:length(files)) {
E<-f[[j]]
if (j==1) {
G<-E[,2]}
else {
G<-cbind(G,E[,2])}}
Z<-data.frame(t(G))
colnames(Z)<-E[,1]
write.csv(Z,"Result_0.2.csv") }
setwd("../") }

for (ki in 1:1) {
setwd("p_0.3_")
files <- list.files()
f <- list()
if (length(files)<25) {
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}
for (j in 1:length(files)) {
E<-f[[j]]
if (j==1) {
G<-E[,2]}
else {
G<-cbind(G,E[,2])}}
Z<-data.frame(t(G))
colnames(Z)<-E[,1]
write.csv(Z,"Result_0.3.csv") }
setwd("../") }

for (ki in 1:1) {
setwd("p_0.5_")
files <- list.files()
f <- list()
if (length(files)<25) {
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}
for (j in 1:length(files)) {
E<-f[[j]]
if (j==1) {
G<-E[,2]}
else {
G<-cbind(G,E[,2])}}
Z<-data.frame(t(G))
colnames(Z)<-E[,1]
write.csv(Z,"Result_0.5.csv") }
setwd("../") }


for (ki in 1:1) {
setwd("p_0.7_")
files <- list.files()
f <- list()
if (length(files)<25) {
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}
for (j in 1:length(files)) {
E<-f[[j]]
if (j==1) {
G<-E[,2]}
else {
G<-cbind(G,E[,2])}}
Z<-data.frame(t(G))
colnames(Z)<-E[,1]
write.csv(Z,"Result_0.7.csv") }
setwd("../") }


for (ki in 1:1) {
setwd("p_0.8_")
files <- list.files()
f <- list()
if (length(files)<25) {
for (i in 1:length(files)) {  
f[[i]] <- read.csv(files[i], header = TRUE, sep = ",")
}
for (j in 1:length(files)) {
E<-f[[j]]
if (j==1) {
G<-E[,2]}
else {
G<-cbind(G,E[,2])}}
Z<-data.frame(t(G))
colnames(Z)<-E[,1]
write.csv(Z,"Result_0.8.csv") }
setwd("../") }


A1<-read.csv("p_0.025_/Result_0.025.csv",header=TRUE)
A1<-A1[,-1]

A2<-read.csv("p_0.05_/Result_0.05.csv",header=TRUE)
A2<-A2[,-1]

A3<-read.csv("p_0.1_/Result_0.1.csv",header=TRUE)
A3<-A3[,-1]

A4<-read.csv("p_0.2_/Result_0.2.csv",header=TRUE)
A4<-A4[,-1]

A5<-read.csv("p_0.3_/Result_0.3.csv",header=TRUE)
A5<-A5[,-1]

A6<-read.csv("p_0.5_/Result_0.5.csv",header=TRUE)
A6<-A6[,-1]

A7<-read.csv("p_0.7_/Result_0.7.csv",header=TRUE)
A7<-A7[,-1]

A8<-read.csv("p_0.8_/Result_0.8.csv",header=TRUE)
A8<-A8[,-1]

F1<-rbind(A1,A2,A3,A4,A5,A6,A7,A8)

F2<-F1[,66:128] # This selects Cohen's d method. Alternatives would be F2<- F1[,66:128] - which is Stan_diff_means (Approach 2) and F2<- F1[,129:191] - logit(p) (Approach 3), HOWEVER you cannot just simply and only do that here, as in that case, the whole y-axis would have to rescaled (in case of Approach 2, by using factor of 5) because purple dashed line starts then at 1, not at 0.2, but then for loop (below) at y-parameters would contain "2*" instead of "*10" - Still, by doing very consistent code changes and with only a slight effort - Figures S12 and S14 could be also reproduced.
T<-F2


Zu<-as.vector(t(colMeans(T)))
EM<-array(ncol(T))
medi<-array(ncol(T))
mini<-array(ncol(T))
maxi<-array(ncol(T))
quart1<-array(ncol(T))
quart3<-array(ncol(T))

for (i in 1:ncol(T)) {
EM[i]<-1.973*sd(T[,i])/sqrt(nrow(T))
medi[i]<-median(T[,i])
mini[i]<-quantile(T[,i])[1]
quart1[i]<-quantile(T[,i])[2]
quart3[i]<-quantile(T[,i])[4]
maxi[i]<-quantile(T[,i])[5]
}


FG<-c("prot","fat","carb","alco","mois","caff","theo","sugr","fibe","calc","iron","magn","phos","pota","sodi","zinc","copp","sele","reti","bcar","acar","atoc","vd","cryp","lyco","lz","vc","vb1","vb2","niac","vb6","fola","vb12","tchl","vk","fa","tff","atoa","b12a","chol","sfat","s040","s060","s080","s100","s120","s140","s160","s180","m181","p182","p183","p204","p226","m161","p184","m201","p205","m221","p225","mfat","pfat","FPro")
color<-c("red","darkgreen")

FG2<-c("Protein","Total fat","Carbohydrate","Alcohol","Water", "Caffeine", "Theobromine","Total sugars","Dietary fiber", "Calcium", "Iron", "Magnesium", "Phosphorus", "Potassium", "Sodium", "Zinc","Copper","Selenium","Retinol","β-carotene", "α-carotene", "Vitamin E", "Vitamin D", "β-cryptoxanthin", "Lycopene", "Lut.+zexanthin","Vitamin C","Vitamin B1","Vitamin B2", "Vitamin B3", "Vitamin B6","Total folate", "Vitamin B12", "Choline","Vitamin K", "Folic acid", "Food folate", "Add. vit. E","Add. vit. B12", "Cholesterol", "Total SFA", "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 18:1", "PFA 18:2", "PFA 18:3", "PFA 20:4","PFA 22:6", "MFA 16:1", "PFA 18:4","MFA 20:1","PFA 20:5","MFA 22:1", "PFA 22:5","Total MFA","Total PFA","FPro")

mdif<-Zu

png("Figure_2.png", width = 6600, height = 4100, res = 400)
par(mar = c(0.6, 0, 0, 1),cex=1.13)
#dev.new(width=15.86, height=10.2, unit="in")
frame()
plot.window(xlim=c(0,20), ylim=c(0,16))

text(10,16.25,expression("Cohen's " * italic(d) * " between " * italic("Non") * "-" * italic("A") * " and "  * italic(A) * " through all datasets"),cex=1.4)
#text(0,16,"(a)",cex=1.6)


segments(x0=0.8,y0=10,x1=20,y1=10)
text(-0.1, 10,
     expression("Cohen's " * italic(d) * ", i.e. [" * italic(m) * "(" * italic("Non") * "-" * italic("A") * ") - " * 
               italic(m) * "(" * italic(A) * ")] / " * italic(s) * "(pooled)"),
     cex = 1.3, srt = 90, xpd = TRUE)
segments(x0=0.8,y0=1.9,x1=0.8,y1=16)
segments(x0=0.7,y0=10,x1=0.8,y1=10)
segments(x0=0.7,y0=12,x1=0.8,y1=12)
segments(x0=0.7,y0=14,x1=0.8,y1=14)
segments(x0=0.7,y0=16,x1=0.8,y1=16)
segments(x0=0.7,y0=8,x1=0.8,y1=8)
segments(x0=0.7,y0=6,x1=0.8,y1=6)
segments(x0=0.7,y0=4,x1=0.8,y1=4)
segments(x0=0.7,y0=2,x1=20,y1=2)
segments(x0=0.8,y0=8,x1=20,y1=8,lty="dashed",col="purple")
segments(x0=0.8,y0=12,x1=20,y1=12,lty="dashed",col="purple")
text(0.42,16,"0.6",cex=1.3)
text(0.42,14,"0.4",cex=1.3)
text(0.42,12,"0.2",cex=1.3)
text(0.48,10,"0",cex=1.3)
text(0.42,8,"-0.2",cex=1.3)
text(0.42,6,"-0.4",cex=1.3)
text(0.42,4,"-0.6",cex=1.3)
text(0.42,2,"-0.8",cex=1.3)
tuf<-c(63)
color<-c("red","darkgreen")

for (i in 1:63) {
if (i%in%tuf) {
colo<-color[2]}
else {
colo<-color[1]}
points(0.9+0.3*i,10+10*mdif[i],pch=16,col="blue",cex=1.2)
#text(0.9+0.3*i,1.25,paste(FG[i]),cex=1.2,srt=90,xpd=TRUE)
text(1.05+0.3*i,1.77,paste(FG2[i]),cex=1.2,srt=90,xpd=TRUE,pos=2)
segments(x0=0.9+0.3*i,y0=1.9,x1=0.9+0.3*i,y1=2)
segments(x0=0.9+0.3*i,y0=10+10*mini[i],x1=0.9+0.3*i,y1=10+10*quart1[i],col=colo,lwd=2)
segments(x0=0.9+0.3*i,y0=10+10*quart3[i],x1=0.9+0.3*i,y1=10+10*maxi[i],col=colo,lwd=2)
segments(x0=0.9+0.3*i+0.1,y0=10+10*quart1[i],x1=0.9+0.3*i+0.1,y1=10+10*quart3[i],col=colo,lwd=2)
segments(x0=0.9+0.3*i-0.1,y0=10+10*quart1[i],x1=0.9+0.3*i-0.1,y1=10+10*quart3[i],col=colo,lwd=2)

segments(x0=0.9+0.3*i-0.1,y0=10+10*quart1[i],x1=0.9+0.3*i+0.1,y1=10+10*quart1[i],col=colo,lwd=2)

segments(x0=0.9+0.3*i-0.1,y0=10+10*quart3[i],x1=0.9+0.3*i+0.1,y1=10+10*quart3[i],col=colo,lwd=2)
segments(x0=0.9+0.3*i-0.1,y0=10+10*medi[i],x1=0.9+0.3*i+0.1,y1=10+10*medi[i],col=colo,lwd=2)
segments(x0=0.9+0.3*i,y0=2,x1=0.9+0.3*i,y1=10+10*mini[i],col="gray",lwd=1,lty="dashed")

segments(x0=0.9+0.3*i-0.1,y0=10+10*(mdif[i]-EM[i]),x1=0.9+0.3*i+0.1,y1=10+10*(mdif[i]-EM[i]),col="black",lwd=2)
segments(x0=0.9+0.3*i-0.1,y0=10+10*(mdif[i]+EM[i]),x1=0.9+0.3*i+0.1,y1=10+10*(mdif[i]+EM[i]),col="black",lwd=2)
}

text(3,5,expression( italic(m) * "(" * italic("Non") * "-" * italic("A") * ")<" * italic(m) * "(" * italic(A) * ")" ),cex=1.3,col="brown")
text(3,15,expression(italic(m) * "(" * italic("Non") * "-" * italic("A") * ")>" * italic(m) * "(" * italic(A) * ")"),cex=1.3,col="brown")
segments(x0=2.8,y0=5.3,x1=2.8,y1=6.8,lty="dashed",col="brown")
segments(x0=2.8,y0=13.3,x1=2.8,y1=14.8,lty="dashed",col="brown")
segments(x0=2.65,y0=5.8,x1=2.8,y1=5.3,lty="dashed",col="brown")
segments(x0=2.95,y0=5.8,x1=2.8,y1=5.3,lty="dashed",col="brown")
segments(x0=2.65,y0=14.3,x1=2.8,y1=14.8,lty="dashed",col="brown")
segments(x0=2.95,y0=14.3,x1=2.8,y1=14.8,lty="dashed",col="brown")

dev.off()

