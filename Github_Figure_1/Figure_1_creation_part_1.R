#### US NHANES ####

library(plotrix)


A<-read.csv("Max&Min_p_val_&coef_final_statistics_with_US-NHANES_AllF-Nutrient-svyglm.csv",header=TRUE)
A<-A[,-1]

B<-read.csv("NonNut_Max&Min_p_val_&coef_final_statistics_with_US-NHANES_AllF-nonNutrient-svyglm.csv",header=TRUE)
B<-B[,-1]

T1_Nut<--log10(as.numeric(A[1,]))
T2_Nut<--log10(as.numeric(A[2,]))

S1_Nut<-as.numeric(A[3,])
S2_Nut<-as.numeric(A[4,])

T1_NonNut<--log10(as.numeric(B[1,]))
T2_NonNut<--log10(as.numeric(B[2,]))

S1_NonNut<-as.numeric(B[3,])
S2_NonNut<-as.numeric(B[4,])

T_Nut<-rbind(T1_Nut,T2_Nut)
S_Nut<-rbind(S1_Nut,S2_Nut)
T_NonNut<-rbind(T1_NonNut,T2_NonNut)
S_NonNut<-rbind(S1_NonNut,S2_NonNut)

FG<-c("Protein","Total fat","Carbohydrate","Alcohol","Water", "Caffeine", "Theobromine","Total sugars","Dietary fiber", "Calcium", "Iron", "Magnesium", "Phosphorus", "Potassium", "Sodium", "Zinc","Copper","Selenium","Retinol","β-carotene", "α-carotene", "Vitamin E", "Vitamin D", "β-cryptoxanthin", "Lycopene", "Lut.+zexanthin","Vitamin C","Vitamin B1","Vitamin B2", "Vitamin B3", "Vitamin B6","Total folate", "Vitamin B12", "Choline","Vitamin K", "Folic acid", "Food folate", "Add. vit. E","Add. vit. B12", "Cholesterol", "Total SFA", "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MUFA 18:1", "PUFA 18:2", "PUFA 18:3", "PUFA 20:4","PUFA 22:6", "MUFA 16:1", "PUFA 18:4","MUFA 20:1","PUFA 20:5","MUFA 22:1", "PUFA 22:5","Total MUFA","Total PUFA","FPro")

facr<-0.7

#dev.new(width=15.86, height=7.93, unit="in")
#frame()
#plot.window(xlim=c(0,20), ylim=c(0,4+12*facr))

png("US-NHANES.png", width = 6600, height = 3300, res = 400)
par(mar = c(0, 0, 0, 0),cex=1.13)
frame()
plot.window(xlim=c(0,20), ylim=c(0,4+12*facr))




for (i in 1:dim(A)[2]) {
if (i %% 2==0) {
rect(0.66+0.3*i, 0.85, 0.97+0.3*i, 4+12*facr, col = "lightgray", border = NA)}}

segments(x0=0.8,y0=4,x1=20,y1=4)
text(-0.1,4+6*facr,"-log(p-value)",cex=1.3,srt=90,xpd=TRUE)
segments(x0=0.8,y0=3.9,x1=0.8,y1=4+12*facr)

segments(x0=0.8,y0=3.9,x1=0.8,y1=4+12*facr)

segments(x0=0.7,y0=4+6*facr,x1=0.8,y1=4+6*facr)
segments(x0=0.7,y0=4+9*facr,x1=0.8,y1=4+9*facr)
segments(x0=0.7,y0=4+12*facr,x1=0.8,y1=4+12*facr)
segments(x0=0.7,y0=4+3*facr,x1=0.8,y1=4+3*facr)
segments(x0=0.7,y0=4,x1=0.8,y1=4)
text(0.38,4+12*facr,"10",cex=1.3)
text(0.42,4+9*facr,"7.5",cex=1.3)
text(0.42,4+6*facr,"5.0",cex=1.3)
text(0.42,4+3*facr,"2.5",cex=1.3)
text(0.48,4,"0",cex=1.3)

for (i in 1:dim(A)[2]) {
text(0.99+0.3*i,3.77,paste(FG[i]),cex=1.2,srt=90,xpd=TRUE,pos=2)

segments(x0=0.69+0.3*i, y0=4+facr*1.2*T_Nut[1,i], x1=0.81+0.3*i, y1=4+facr*1.2*T_Nut[1,i],col="red",lwd=2)
segments(x0=0.69+0.3*i, y0=4+facr*1.2*T_Nut[2,i], x1=0.81+0.3*i, y1=4+facr*1.2*T_Nut[2,i],col="red",lwd=2)
segments(x0=0.75+0.3*i, y0=4+facr*1.2*T_Nut[1,i], x1=0.75+0.3*i, y1=4+facr*1.2*T_Nut[2,i],col="red",lwd=2)



segments(x0=0.82+0.3*i, y0=4+facr*1.2*T_NonNut[1,i], x1=0.94+0.3*i, y1=4+facr*1.2*T_NonNut[1,i],col="blue",lwd=2)
segments(x0=0.82+0.3*i, y0=4+facr*1.2*T_NonNut[2,i], x1=0.94+0.3*i, y1=4+facr*1.2*T_NonNut[2,i],col="blue",lwd=2)
segments(x0=0.88+0.3*i, y0=4+facr*1.2*T_NonNut[1,i], x1=0.88+0.3*i, y1=4+facr*1.2*T_NonNut[2,i],col="blue",lwd=2)

if ((S_NonNut[1,i]>0)&&(S_NonNut[2,i]>0)&&(S_Nut[1,i]>0)&&(S_Nut[2,i]>0)) {
text(0.82+0.3*i,max(c(4+facr*1.2*T_Nut[2,i],4+facr*1.2*T_NonNut[2,i]))+0.5,"+",cex=1.5)
}
else {
if ((S_NonNut[1,i]<0)&&(S_NonNut[2,i]<0)&&(S_Nut[1,i]<0)&&(S_Nut[2,i]<0)) {
text(0.82+0.3*i,max(c(4+facr*1.2*T_Nut[2,i],4+facr*1.2*T_NonNut[2,i]))+0.5,"−",cex=1.5)}
else {
text(0.82+0.3*i,max(c(4+facr*1.2*T_Nut[2,i],4+facr*1.2*T_NonNut[2,i]))+0.5,"x",cex=1.5)
}}
}

segments(x0=0.8,y0=4-log10(0.05)*1.2*facr,x1=20,y1=4-log10(0.05)*1.2*facr,lty="dashed",col="purple",lwd=3)
text(0.45, 4.2-log10(0.05)*1.2*facr,"95%",cex=1.2,col="purple")
text(0.45, 3.7-log10(0.05)*1.2*facr,"conf.",cex=1.2,col="purple")

rectFill(x1=8.4, y1=4+8*facr, x2=14.5, y2=4+12*facr, fg="white", bg = "white",pch=0,pch.col="white")
segments(x0=8.4, y0=4+11.8*facr, x1=8.4, y1=4+12*facr,col="red",lwd=2)
segments(x0=10, y0=4+11.8*facr, x1=10, y1=4+12*facr,col="red",lwd=2)
segments(x0=8.4, y0=4+11.9*facr, x1=10, y1=4+11.9*facr,col="red",lwd=2)
segments(x0=8.4, y0=4+11.0*facr, x1=8.4, y1=4+11.2*facr,col="blue",lwd=2)
segments(x0=10, y0=4+11.0*facr, x1=10, y1=4+11.2*facr,col="blue",lwd=2)
segments(x0=8.4, y0=4+11.1*facr, x1=10, y1=4+11.1*facr,col="blue",lwd=2)


text(10,4+11.9*facr,"Nutrient confounders added",pos=4,cex=1.2)
text(10,4+11.1*facr,"Only Non-nutrient confounders",pos=4,cex=1.2)
text(8.2,4+10.3*facr,"+ : only positive Coefficients obtained",pos=4,cex=1.2)
text(8.2,4+9.5*facr,"− : only negative Coefficients obtained",pos=4,cex=1.2)
text(8.2,4+8.7*facr,"x : inconsistent signs of Coefficients",pos=4,cex=1.2)

dev.off()

#### US NHANES ####


#### K NHANES ####

A<-read.csv("Max&Min_p_val_&coef_final_statistics_with_KNHANES_AllF-Nutrient-svyglm.csv",header=TRUE)
A<-A[,-1]

B<-read.csv("NonNut_Max&Min_p_val_&coef_final_statistics_with_KNHANES_AllF-nonNutrient-svyglm.csv",header=TRUE)

B<-B[,-1]

T1_Nut<--log10(as.numeric(A[1,]))
T2_Nut<--log10(as.numeric(A[2,]))

S1_Nut<-as.numeric(A[3,])
S2_Nut<-as.numeric(A[4,])

T1_NonNut<--log10(as.numeric(B[1,]))
T2_NonNut<--log10(as.numeric(B[2,]))

S1_NonNut<-as.numeric(B[3,])
S2_NonNut<-as.numeric(B[4,])

T_Nut<-rbind(T1_Nut,T2_Nut)
S_Nut<-rbind(S1_Nut,S2_Nut)
T_NonNut<-rbind(T1_NonNut,T2_NonNut)
S_NonNut<-rbind(S1_NonNut,S2_NonNut)


FG<-c("Water","Protein","Total fat", "SFA", "MUFA","PUFA", "PUFA n-3", "PUFA n-6","SFA 10:0","SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "SFA 20:0", "SFA 22:0", "SFA 24:0", "MUFA 14:1", "MUFA 16:1", "MUFA 18:1", "MUFA 20:1", "MUFA 22:1", "MUFA 24:1", "PUFA 18:2 n-6", "PUFA 18:3 n-3", "PUFA 18:3 n-6", "PUFA 18:4", "PUFA 20:2", "PUFA 20:3 n-6", "PUFA 20:4 n-6", "PUFA 20:5 n-3", "PUFA 22:2", "PUFA 22:5", "PUFA 22:6 n-3", "Cholesterol", "Carbohydrate", "Dietary fiber", "Calcium", "Phosphorus", "Sodium", "Potassium", "Iron", "β-carotene", "Retinol", "Vitamin B1","Vitamin B2", "Vitamin B3", "Vitamin C","FPro")


f2<-63/length(FG)

facr<-0.7

#dev.new(width=15.86, height=7.93, unit="in")
#frame()
#plot.window(xlim=c(0,20), ylim=c(0,4+12*facr))

png("test_KHANES.png", width = 6600, height = 3300, res = 400)
par(mar = c(0, 0, 0, 0),cex=1.13)
frame()
plot.window(xlim=c(0,20), ylim=c(0,4+12*facr))



for (i in 1:dim(A)[2]) {
if (i %% 2==0) {
rect((0.66+0.3*i)*f2, 0.85, (0.97+0.3*i)*f2, 4+12*facr, col = "lightgray", border = NA)}}

segments(x0=0.8,y0=4,x1=20.2,y1=4)
text(-0.1,4+6*facr,"-log(p-value)",cex=1.3,srt=90,xpd=TRUE)
segments(x0=0.8,y0=3.9,x1=0.8,y1=4+12*facr)

segments(x0=0.8,y0=3.9,x1=0.8,y1=4+12*facr)

segments(x0=0.7,y0=4+6*facr,x1=0.8,y1=4+6*facr)
segments(x0=0.7,y0=4+9*facr,x1=0.8,y1=4+9*facr)
segments(x0=0.7,y0=4+12*facr,x1=0.8,y1=4+12*facr)
segments(x0=0.7,y0=4+3*facr,x1=0.8,y1=4+3*facr)
segments(x0=0.7,y0=4,x1=0.8,y1=4)
text(0.38,4+12*facr,"10",cex=1.3)
text(0.42,4+9*facr,"7.5",cex=1.3)
text(0.42,4+6*facr,"5.0",cex=1.3)
text(0.42,4+3*facr,"2.5",cex=1.3)
text(0.48,4,"0",cex=1.3)

for (i in 1:dim(A)[2]) {
text((0.99+0.3*i)*f2-0.03,3.77,paste(FG[i]),cex=1.2,srt=90,xpd=TRUE,pos=2)

segments(x0=(0.69+0.3*i)*f2, y0=4+facr*1.2*T_Nut[1,i], x1=(0.81+0.3*i)*f2, y1=4+facr*1.2*T_Nut[1,i],col="red",lwd=2)
segments(x0=(0.69+0.3*i)*f2, y0=4+facr*1.2*T_Nut[2,i], x1=(0.81+0.3*i)*f2, y1=4+facr*1.2*T_Nut[2,i],col="red",lwd=2)
segments(x0=(0.75+0.3*i)*f2, y0=4+facr*1.2*T_Nut[1,i], x1=(0.75+0.3*i)*f2, y1=4+facr*1.2*T_Nut[2,i],col="red",lwd=2)



segments(x0=(0.82+0.3*i)*f2, y0=4+facr*1.2*T_NonNut[1,i], x1=(0.94+0.3*i)*f2, y1=4+facr*1.2*T_NonNut[1,i],col="blue",lwd=2)
segments(x0=(0.82+0.3*i)*f2, y0=4+facr*1.2*T_NonNut[2,i], x1=(0.94+0.3*i)*f2, y1=4+facr*1.2*T_NonNut[2,i],col="blue",lwd=2)
segments(x0=(0.88+0.3*i)*f2, y0=4+facr*1.2*T_NonNut[1,i], x1=(0.88+0.3*i)*f2, y1=4+facr*1.2*T_NonNut[2,i],col="blue",lwd=2)

if ((S_NonNut[1,i]>0)&&(S_NonNut[2,i]>0)&&(S_Nut[1,i]>0)&&(S_Nut[2,i]>0)) {
text((0.82+0.3*i)*f2,max(c(4+facr*1.2*T_Nut[2,i],4+facr*1.2*T_NonNut[2,i]))+0.5,"+",cex=1.5)
}
else {
if ((S_NonNut[1,i]<0)&&(S_NonNut[2,i]<0)&&(S_Nut[1,i]<0)&&(S_Nut[2,i]<0)) {
text((0.82+0.3*i)*f2,max(c(4+facr*1.2*T_Nut[2,i],4+facr*1.2*T_NonNut[2,i]))+0.5,"−",cex=1.5)}
else {
text((0.82+0.3*i)*f2,max(c(4+facr*1.2*T_Nut[2,i],4+facr*1.2*T_NonNut[2,i]))+0.5,"x",cex=1.5)
}}
}

segments(x0=0.8,y0=4-log10(0.05)*facr*1.2,x1=20.3,y1=4-log10(0.05)*facr*1.2,lty="dashed",col="purple",lwd=3)
text(0.45, 4.2-log10(0.05)*facr*1.2,"95%",cex=1.2,col="purple")
text(0.45, 3.7-log10(0.05)*facr*1.2,"conf.",cex=1.2,col="purple")

library(plotrix)

rectFill(x1=8.4, y1=4+8*facr, x2=14.5, y2=4+12*facr, fg="white", bg = "white",pch=0,pch.col="white")
segments(x0=8.6, y0=4+11.8*facr, x1=8.6, y1=4+12*facr,col="red",lwd=2)
segments(x0=10.2, y0=4+11.8*facr, x1=10.2, y1=4+12*facr,col="red",lwd=2)
segments(x0=8.6, y0=4+11.9*facr, x1=10.2, y1=4+11.9*facr,col="red",lwd=2)
segments(x0=8.6, y0=4+11.0*facr, x1=8.6, y1=4+11.2*facr,col="blue",lwd=2)
segments(x0=10.2, y0=4+11.0*facr, x1=10.2, y1=4+11.2*facr,col="blue",lwd=2)
segments(x0=8.6, y0=4+11.1*facr, x1=10.2, y1=4+11.1*facr,col="blue",lwd=2)


text(10.2,4+11.9*facr,"Nutrient confounders added",pos=4,cex=1.2)
text(10.2,4+11.1*facr,"Only Non-nutrient confounders",pos=4,cex=1.2)
text(8.4,4+10.3*facr,"+ : only positive Coefficients obtained",pos=4,cex=1.2)
text(8.4,4+9.5*facr,"− : only negative Coefficients obtained",pos=4,cex=1.2)
text(8.4,4+8.7*facr,"x : inconsistent signs of Coefficients",pos=4,cex=1.2)

dev.off()


#### K NHANES ####


####UK NDNS #####

library(plotrix)

A<-read.csv("Max&Min_p_val_&coef_final_statistics_with_UK-NDNS_AllF-Nutrient-svyglm_max_obs_confs.csv",header=TRUE)
A<-A[,-1]

B<-read.csv("NonNut_Max&Min_p_val_&coef_final_statistics_with_UK-NDNS_AllF-nonNutrient-svyglm.csv",header=TRUE)

B<-B[,-1]

T1_Nut<--log10(as.numeric(A[1,]))
T2_Nut<--log10(as.numeric(A[2,]))

S1_Nut<-as.numeric(A[3,])
S2_Nut<-as.numeric(A[4,])

T1_NonNut<--log10(as.numeric(B[1,]))
T2_NonNut<--log10(as.numeric(B[2,]))

S1_NonNut<-as.numeric(B[3,])
S2_NonNut<-as.numeric(B[4,])

T_Nut<-rbind(T1_Nut,T2_Nut)
S_Nut<-rbind(S1_Nut,S2_Nut)
T_NonNut<-rbind(T1_NonNut,T2_NonNut)
S_NonNut<-rbind(S1_NonNut,S2_NonNut)


FG<-c("Protein","Total fat", "Carbohydrate", "Sodium", "Potassium", "Calcium", "Magnesium", "Phosphorus", "Iron", "Haemiron", "Nonhaemiron", "Copper", "Zinc", "Chloride", "Retinol", "Totalcarotene", "Alphacarotene", "Betacarotene", "β-cryptoxanthin",  "Vitamin A ret.eq.", "Vitamin D", "Vitamin E", "Vitamin B1","Vitamin B2", "Vitamin B3", "Vitamin B6", "Vitamin B12", "Folate", "Vitamin B5", "Biotin", "Vitamin C", "Alcohol", "Water", "Total sugar", "Other sugar", "Starch", "Glucose", "Fructose", "Sucrose", "Maltose", "Lactose", "Nonmilk ex. sugar", "Intr. & milk sugar", "Free Sugar", "Dietary fiber","Englyst fiber", "Total nitrogen", "Manganese", "Iodine", "Selenium", "Cholesterol", "SFA", "Cis-MUFA","Cis-PUFA n-6", "Cis-PUFA n-3", "Trans-FA", "FPro")


f2<-63/length(FG)


facr<-0.7

#dev.new(width=15.86, height=7.93, unit="in")
#frame()
#plot.window(xlim=c(0,20), ylim=c(0,4+12*facr))

png("test_UKNDNS.png", width = 6600, height = 3300, res = 400)
par(mar = c(0, 0, 0, 0),cex=1.13)
frame()
plot.window(xlim=c(0,20), ylim=c(0,4+12*facr))

for (i in 1:dim(A)[2]) {
if (i %% 2==0) {
rect((0.66+0.3*i)*f2, 0.85, (0.97+0.3*i)*f2, 4+12*facr, col = "lightgray", border = NA)}}

f=0.8

segments(x0=0.8,y0=4,x1=4+20.2,y1=4)
text(-0.1,4+6*facr,"-log(p-value)",cex=1.3,srt=90,xpd=TRUE)
segments(x0=0.8,y0=3.9,x1=0.8,y1=4+12*facr)

segments(x0=0.8,y0=3.9,x1=0.8,y1=4+12*facr)

segments(x0=0.7,y0=4+6*facr,x1=0.8,y1=4+6*facr)
segments(x0=0.7,y0=4+9*facr,x1=0.8,y1=4+9*facr)
segments(x0=0.7,y0=4+12*facr,x1=0.8,y1=4+12*facr)
segments(x0=0.7,y0=4+3*facr,x1=0.8,y1=4+3*facr)
segments(x0=0.7,y0=4,x1=0.8,y1=4)
text(0.38,4+12*facr,paste0(f*10),cex=1.3)
text(0.42,4+9*facr,paste0(f*7.5),cex=1.3)
text(0.42,4+6*facr,paste0(f*5),cex=1.3)
text(0.42,4+3*facr,paste0(f*2.5),cex=1.3)
text(0.48,4,"0",cex=1.3)

for (i in 1:dim(A)[2]) {
text((0.99+0.3*i)*f2-0.03,3.77,paste(FG[i]),cex=1.2,srt=90,xpd=TRUE,pos=2)

segments(x0=(0.69+0.3*i)*f2, y0=4+facr*1.2*T_Nut[1,i]/f, x1=(0.81+0.3*i)*f2, y1=4+facr*1.2*T_Nut[1,i]/f,col="red",lwd=2)
segments(x0=(0.69+0.3*i)*f2, y0=4+facr*1.2*T_Nut[2,i]/f, x1=(0.81+0.3*i)*f2, y1=4+facr*1.2*T_Nut[2,i]/f,col="red",lwd=2)
segments(x0=(0.75+0.3*i)*f2, y0=4+facr*1.2*T_Nut[1,i]/f, x1=(0.75+0.3*i)*f2, y1=4+facr*1.2*T_Nut[2,i]/f,col="red",lwd=2)



segments(x0=(0.82+0.3*i)*f2, y0=4+facr*1.2*T_NonNut[1,i]/f, x1=(0.94+0.3*i)*f2, y1=4+facr*1.2*T_NonNut[1,i]/f,col="blue",lwd=2)
segments(x0=(0.82+0.3*i)*f2, y0=4+facr*1.2*T_NonNut[2,i]/f, x1=(0.94+0.3*i)*f2, y1=4+facr*1.2*T_NonNut[2,i]/f,col="blue",lwd=2)
segments(x0=(0.88+0.3*i)*f2, y0=4+facr*1.2*T_NonNut[1,i]/f, x1=(0.88+0.3*i)*f2, y1=4+facr*1.2*T_NonNut[2,i]/f,col="blue",lwd=2)

if ((S_NonNut[1,i]>0)&&(S_NonNut[2,i]>0)&&(S_Nut[1,i]>0)&&(S_Nut[2,i]>0)) {
text((0.82+0.3*i)*f2,max(c(4+facr*1.2*T_Nut[2,i]/f,4+facr*1.2*T_NonNut[2,i]/f))+0.5,"+",cex=1.5)
}
else {
if ((S_NonNut[1,i]<0)&&(S_NonNut[2,i]<0)&&(S_Nut[1,i]<0)&&(S_Nut[2,i]<0)) {
text((0.82+0.3*i)*f2,max(c(4+facr*1.2*T_Nut[2,i]/f,4+facr*1.2*T_NonNut[2,i]/f))+0.5,"−",cex=1.5)}
else {
text((0.82+0.3*i)*f2,max(c(4+facr*1.2*T_Nut[2,i]/f,4+facr*1.2*T_NonNut[2,i]/f))+0.5,"x",cex=1.5)
}}
}

segments(x0=0.8,y0=4-log10(0.05)*facr*1.2/f,x1=20.3,y1=4-log10(0.05)*facr*1.2/f,lty="dashed",col="purple",lwd=3)
text(0.45, 4.0-log10(0.05)*facr*1.2/f,"95%",cex=1.2,col="purple")
text(0.45, 3.6-log10(0.05)*facr*1.2/f,"conf.",cex=1.2,col="purple")

library(plotrix)

rectFill(x1=8.4, y1=4+8*facr, x2=14.5, y2=4+12*facr, fg="white", bg = "white",pch=0,pch.col="white")
segments(x0=8.6, y0=4+11.8*facr, x1=8.6, y1=4+12*facr,col="red",lwd=2)
segments(x0=10.2, y0=4+11.8*facr, x1=10.2, y1=4+12*facr,col="red",lwd=2)
segments(x0=8.6, y0=4+11.9*facr, x1=10.2, y1=4+11.9*facr,col="red",lwd=2)
segments(x0=8.6, y0=4+11.0*facr, x1=8.6, y1=4+11.2*facr,col="blue",lwd=2)
segments(x0=10.2, y0=4+11.0*facr, x1=10.2, y1=4+11.2*facr,col="blue",lwd=2)
segments(x0=8.6, y0=4+11.1*facr, x1=10.2, y1=4+11.1*facr,col="blue",lwd=2)


text(10.2,4+11.9*facr,"Nutrient confounders added",pos=4,cex=1.2)
text(10.2,4+11.1*facr,"Only Non-nutrient confounders",pos=4,cex=1.2)
text(8.4,4+10.3*facr,"+ : only positive Coefficients obtained",pos=4,cex=1.2)
text(8.4,4+9.5*facr,"− : only negative Coefficients obtained",pos=4,cex=1.2)
text(8.4,4+8.7*facr,"x : inconsistent signs of Coefficients",pos=4,cex=1.2)

dev.off()

