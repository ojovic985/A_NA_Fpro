
library(readxl)
L<-read_excel("Supplementary_table_results_main.xlsx",sheet = "p025")
L1<-as.numeric(as.matrix(L[3:38,7:197]))
A1<-matrix(L1,nrow=36,ncol=191)
L<-read_excel("Supplementary_table_results_main.xlsx",sheet = "p05")
L1<-as.numeric(as.matrix(L[3:38,7:197]))
A2<-matrix(L1,nrow=36,ncol=191)
L<-read_excel("Supplementary_table_results_main.xlsx",sheet = "p10")
L1<-as.numeric(as.matrix(L[3:38,7:197]))
A3<-matrix(L1,nrow=36,ncol=191)
L<-read_excel("Supplementary_table_results_main.xlsx",sheet = "p20")
L1<-as.numeric(as.matrix(L[3:38,7:197]))
A4<-matrix(L1,nrow=36,ncol=191)
L<-read_excel("Supplementary_table_results_main.xlsx",sheet = "p30")
L1<-as.numeric(as.matrix(L[3:38,7:197]))
A5<-matrix(L1,nrow=36,ncol=191)
L<-read_excel("Supplementary_table_results_main.xlsx",sheet = "p50")
L1<-as.numeric(as.matrix(L[3:38,7:197]))
A6<-matrix(L1,nrow=36,ncol=191)
L<-read_excel("Supplementary_table_results_main.xlsx",sheet = "p70")
L1<-as.numeric(as.matrix(L[3:38,7:197]))
A7<-matrix(L1,nrow=36,ncol=191)
L<-read_excel("Supplementary_table_results_main.xlsx",sheet = "p80")
L1<-as.numeric(as.matrix(L[3:38,7:197]))
A8<-matrix(L1,nrow=36,ncol=191)

F1<-rbind(A1,A2,A3,A4,A5,A6,A7,A8)

T<-matrix(rep(NA, nrow(F1)*63),nrow= nrow(F1))

for (i in 1:nrow(T)) {
for (j in 1:ncol(T)) {
T[i,j]<-F1[i,j+65]/F1[i,j+128]
}}
any(is.na(T))



T<--T #Comment: This change in sign is only necessary because in our Main code we put: m_diff[ji]<-mean(Anem[,ji])-mean(NonAnem[,ji]) instead of   m_diff[ji]<-mean(NonAnem[,ji])-mean(Anem[,ji])

Zu<-as.vector(t(colMeans(T)))
EM<-array(ncol(T))
medi<-array(ncol(T))
mini<-array(ncol(T))
maxi<-array(ncol(T))
quart1<-array(ncol(T))
quart3<-array(ncol(T))

for (i in 1:ncol(T)) {
EM[i]<-1.968*sd(T[,i])/sqrt(nrow(T))
medi[i]<-median(T[,i])
mini[i]<-quantile(T[,i])[1]
quart1[i]<-quantile(T[,i])[2]
quart3[i]<-quantile(T[,i])[4]
maxi[i]<-quantile(T[,i])[5]
}


FG<-c("prot","fat","carb","alco","mois","caff","theo","sugr","fibe","calc","iron","magn","phos","pota","sodi","zinc","copp","sele","reti","bcar","acar","atoc","vd","cryp","lyco","lz","vc","vb1","vb2","niac","vb6","fola","vb12","tchl","vk","fa","tff","atoa","b12a","chol","sfat","s040","s060","s080","s100","s120","s140","s160","s180","m181","p182","p183","p204","p226","m161","p184","m201","p205","m221","p225","mfat","pfat","Fpro")
color<-c("red","darkgreen")

FG2<-c("Protein","Total fat","Carbohydrate","Alcohol","Water", "Caffeine", "Theobromine","Total sugars","Dietary fiber", "Calcium", "Iron", "Magnesium", "Phosphorus", "Potassium", "Sodium", "Zinc","Copper","Selenium","Retinol","β-carotene", "α-carotene", "Vitamin E", "Vitamin D", "β-cryptoxanthin", "Lycopene", "Lut.+zexanthin","Vitamin C","Vitamin B1","Vitamin B2", "Vitamin B3", "Vitamin B6","Total folate", "Vitamin B12", "Choline","Vitamin K", "Folic acid", "Food folate", "Add. vit. E","Add. vit. B12", "Cholesterol", "Total SFA", "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 18:1", "PFA 18:2", "PFA 18:3", "PFA 20:4","PFA 22:6", "MFA 16:1", "PFA 18:4","MFA 20:1","PFA 20:5","MFA 22:1", "PFA 22:5","Total MFA","Total PFA","Fpro")

mdif<-Zu

dev.new(width=15.86, height=10.2, unit="in")
frame()
plot.window(xlim=c(0,20), ylim=c(0,16))

text(10,16.25,"Average standardized difference in means (95% C.I.) between NA and A through all datasets",cex=1.4)


segments(x0=0.8,y0=10,x1=20,y1=10)
text(0,10,"m([m(NA)-m(A)]/EM(95% CI))",cex=1.3,srt=90,xpd=TRUE)
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
text(0.48,16,"3",cex=1.3)
text(0.48,14,"2",cex=1.3)
text(0.48,12,"1",cex=1.3)
text(0.48,10,"0",cex=1.3)
text(0.48,8,"-1",cex=1.3)
text(0.48,6,"-2",cex=1.3)
text(0.48,4,"-3",cex=1.3)
text(0.48,2,"-4",cex=1.3)
tuf<-c(12,63)

for (i in 1:63) {
if (i%in%tuf) {
colo<-color[2]}
else {
colo<-color[1]}
points(0.9+0.3*i,10+2*mdif[i],pch=16,col="blue",cex=1.2)
#text(0.9+0.3*i,1.25,paste(FG[i]),cex=1.2,srt=90,xpd=TRUE)
text(1.05+0.3*i,1.77,paste(FG2[i]),cex=1.2,srt=90,xpd=TRUE,pos=2)
segments(x0=0.9+0.3*i,y0=1.9,x1=0.9+0.3*i,y1=2)
segments(x0=0.9+0.3*i,y0=10+2*mini[i],x1=0.9+0.3*i,y1=10+2*quart1[i],col=colo,lwd=2)
segments(x0=0.9+0.3*i,y0=10+2*quart3[i],x1=0.9+0.3*i,y1=10+2*maxi[i],col=colo,lwd=2)
segments(x0=0.9+0.3*i+0.1,y0=10+2*quart1[i],x1=0.9+0.3*i+0.1,y1=10+2*quart3[i],col=colo,lwd=2)
segments(x0=0.9+0.3*i-0.1,y0=10+2*quart1[i],x1=0.9+0.3*i-0.1,y1=10+2*quart3[i],col=colo,lwd=2)

segments(x0=0.9+0.3*i-0.1,y0=10+2*quart1[i],x1=0.9+0.3*i+0.1,y1=10+2*quart1[i],col=colo,lwd=2)

segments(x0=0.9+0.3*i-0.1,y0=10+2*quart3[i],x1=0.9+0.3*i+0.1,y1=10+2*quart3[i],col=colo,lwd=2)
segments(x0=0.9+0.3*i-0.1,y0=10+2*medi[i],x1=0.9+0.3*i+0.1,y1=10+2*medi[i],col=colo,lwd=2)
segments(x0=0.9+0.3*i,y0=2,x1=0.9+0.3*i,y1=10+2*mini[i],col="gray",lwd=1,lty="dashed")

segments(x0=0.9+0.3*i-0.1,y0=10+2*(mdif[i]-EM[i]),x1=0.9+0.3*i+0.1,y1=10+2*(mdif[i]-EM[i]),col="orange",lwd=2)
segments(x0=0.9+0.3*i-0.1,y0=10+2*(mdif[i]+EM[i]),x1=0.9+0.3*i+0.1,y1=10+2*(mdif[i]+EM[i]),col="orange",lwd=2)
}




