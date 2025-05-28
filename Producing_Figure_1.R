#R

#Working directory in R must be '/A_NA_FPro'


######## EXTREMELY IMPORTANT: First run: "Preparing_input_for_producing_figure_1.R" - THEN continue by running this!

## By default we will set:

source("Preparing_input_for_producing_figure_1.R")

A<-read.csv("Food_classes_A_perc_sorted_by_Fpro_G2404_without_M1.csv")
A<-A[,-1]
A<-A[,-1]

De<-read.csv("Correlation_Fpro_vs_Score_for_17_cat.csv",header=TRUE)
De<-De[,-1]
De<-as.vector(t(De))

WE<-read.csv("Compas_scores_Added_to_2404_without_M1_All_food_codes_from_WWEIA_analysis_Incl_dup.csv")
WE<-WE[,-1]
Fpro<-WE[,"min_FPro"]
Food_Compass_Score_2<-WE[,ncol(WE)]

De<-as.vector(t(cbind(t(cor(Fpro,Food_Compass_Score_2)),t(De))))

#dev.new(width=15.86, height=10.7, unit="in")

png("Figure_1.png", width = 6600, height = 3450, res = 450)
par(mar = c(0, 0, 0, 1),cex=0.8)
frame()
plot.window(xlim=c(-0.5,2.6), ylim=c(-13.2,1.5))
#text(0.7,1.5,"Consumption of food categories by anemic and non-anemic participants",cex=1.4)

segments(x0=0.1,y0=1.2,x1=0.1,y1=-11.5,lwd=1)
segments(x0=0.1,y0=-11.42,x1=1.3,y1=-11.42)

for (i in 0:10) {
segments(x0=0.1+i*0.12,y0=1.2,x1=0.1+i*0.12,y1=-11.5)
text(0.1+i*0.12,-11.7,paste(i*10,"%"))
}
expression(italic("Non") * "-" * italic("A"))

library(plotrix)

text(-0.03,1.7,"Food Category",cex=1.2,pos=2)
text(-0.03,0.9,"Overall",cex=1.2,pos=2)
text(0.1,1.7,expression(italic(n)),cex=1.2,pos=2)
text(0.1,0.9,paste(sum(A[,3])+sum(A[,4])),cex=1.2,pos=2)
text(0.1+1.2*0.2,1.7,expression(italic(f) * "(" * italic(A) * ")"),cex=1.2)
text(0.1+1.2*0.8,1.7,expression(italic(f) * "(" * italic("Non") * "-" * italic("A") * ")"),cex=1.2)
text(1.7+0.4,1.7,"FPro",cex=1.2)
text(1.12+0.4,1.7,expression(italic(R) * "(FPro vs. Score)"),cex=1.2)

rectFill(x1=0.1, y1=0.9-0.2, x2=0.1+sum(A[,4])/(sum(A[,3])+sum(A[,4]))*1.2, y2=0.9+0.2, fg="pink", bg = "pink",pch=0,pch.col="pink")
rectFill(x1=0.1+sum(A[,4])/(sum(A[,3])+sum(A[,4]))*1.2, y1=0.9-0.2, x2=1.3, y2=0.9+0.2, fg="cyan", bg = "cyan",pch=0,pch.col="cyan")
text(0.1+0.2*1.2,0.9,paste(round(100*sum(A[,4])/(sum(A[,3])+sum(A[,4])),1) ) )
text(0.1+0.8*1.2,0.9,paste(round(100*sum(A[,3])/(sum(A[,3])+sum(A[,4])),1) ) )



for (i in 1:nrow(A)) {
text(-0.03,0.9-0.7*i,paste(A[i,1]),cex=1.2,pos=2)
text(0.1,0.9-0.7*i,paste(A[i,3]+A[i,4]),cex=1.2,pos=2)
rectFill(x1=0.1, y1=0.9-0.2-0.7*i, x2=0.1+(A[i,4]/(A[i,3]+A[i,4]))*1.2, y2=0.9+0.2-0.7*i, fg="pink", bg = "pink",pch=0,pch.col="pink")
rectFill(x1=0.1+(A[i,4]/(A[i,3]+A[i,4]))*1.2, y1=0.9-0.2-0.7*i, x2=1.3, y2=0.9+0.2-0.7*i, fg="cyan", bg = "cyan",pch=0,pch.col="cyan")
text(0.1+0.2*1.4,0.9-0.7*i,paste(round(100*A[i,4]/(A[i,3]+A[i,4]),1) ) )
text(0.1+0.8*1.4,0.9-0.7*i,paste(round(100*A[i,3]/(A[i,3]+A[i,4]),1) ) )
}


rectFill(x1=0.95-0.3, y1=-12.4, x2=1.002-0.3, y2=-12.0, fg="pink", bg = "pink",pch=0,pch.col="pink")
rectFill(x1=0.9, y1=-12.4, x2=0.952, y2=-12.0, fg="cyan", bg = "cyan",pch=0,pch.col="cyan")
text(1.071-0.3,-12.23,expression(italic(A)),cex=1.2,pos=2)
text(1.399-0.3,-12.23,expression(italic("Non") * "-" * italic("A")),cex=1.2,pos=2)

segments(x0=1.7,y0=1.2,x1=1.7,y1=-11.5,lwd=1)
segments(x0=1.7,y0=-11.42,x1=2.6,y1=-11.42)
for (i in 0:10) {
segments(x0=1.7+i*0.09,y0=1.2,x1=1.7+i*0.09,y1=-11.5)
text(1.7+i*0.09,-11.7,paste(i/10))
}

rectFill(x1=1.7, y1=0.9, x2=1.7+0.7586938*0.9, y2=0.9+0.2, fg="red", bg = "red",pch=0,pch.col="red")
segments(x0=1.7+(0.7586938-0.01496126)*0.9,y0=0.9+0.1,x1=1.7+(0.7586938+0.01496126)*0.9,y1=0.9+0.1,col="black")
segments(x0=1.7+(0.7586938-0.01496126)*0.9,y0=0.9,x1=1.7+(0.7586938-0.01496126)*0.9,y1=0.9+0.2,col="black")
segments(x0=1.7+(0.7586938+0.01496126)*0.9,y0=0.9,x1=1.7+(0.7586938+0.01496126)*0.9,y1=0.9+0.2,col="black")
rectFill(x1=1.7, y1=0.9-0.2, x2=1.7+0.6764192*0.9, y2=0.9, fg="darkgreen", bg = "darkgreen",pch=0,pch.col="darkgreen")
segments(x0=1.7+(0.6764192-0.01825134)*0.9,y0=0.9-0.1,x1=1.7+(0.6764192+0.01825134)*0.9,y1=0.9-0.1,col="black")
segments(x0=1.7+(0.6764192-0.01825134)*0.9,y0=0.9,x1=1.7+(0.6764192-0.01825134)*0.9,y1=0.9-0.2,col="black")
segments(x0=1.7+(0.6764192+0.01825134)*0.9,y0=0.9,x1=1.7+(0.6764192+0.01825134)*0.9,y1=0.9-0.2,col="black")


for (i in 1:nrow(A)) {
rectFill(x1=1.7, y1=0.9-0.7*i, x2=1.7+A[i,8]*0.9, y2=0.9-0.7*i+0.2, fg="red", bg = "red",pch=0,pch.col="red")
segments(x0=1.7+(A[i,8]-A[i,11])*0.9,y0=0.9+0.1-0.7*i,x1=1.7+(A[i,8]+A[i,11])*0.9,y1=0.9+0.1-0.7*i,col="black")
segments(x0=1.7+(A[i,8]-A[i,11])*0.9,y0=0.9-0.7*i,x1=1.7+(A[i,8]-A[i,11])*0.9,y1=0.9+0.2-0.7*i,col="black")
segments(x0=1.7+(A[i,8]+A[i,11])*0.9,y0=0.9-0.7*i,x1=1.7+(A[i,8]+A[i,11])*0.9,y1=0.9+0.2-0.7*i,col="black")
rectFill(x1=1.7, y1=0.9-0.2-0.7*i, x2=1.7+A[i,9]*0.9, y2=0.9-0.7*i, fg="darkgreen", bg = "darkgreen",pch=0,pch.col="darkgreen")
segments(x0=1.7+(A[i,9]-A[i,12])*0.9,y0=0.9-0.1-0.7*i,x1=1.7+(A[i,9]+A[i,12])*0.9,y1=0.9-0.1-0.7*i,col="black")
segments(x0=1.7+(A[i,9]-A[i,12])*0.9,y0=0.9-0.7*i,x1=1.7+(A[i,9]-A[i,12])*0.9,y1=0.9-0.2-0.7*i,col="black")
segments(x0=1.7+(A[i,9]+A[i,12])*0.9,y0=0.9-0.7*i,x1=1.7+(A[i,9]+A[i,12])*0.9,y1=0.9-0.2-0.7*i,col="black")}

rectFill(x1=2.18, y1=-12.4, x2=2.232, y2=-12.0, fg="red", bg = "red",pch=0,pch.col="red")
rectFill(x1=1.9, y1=-12.4, x2=1.952, y2=-12.0, fg="darkgreen", bg = "darkgreen",pch=0,pch.col="darkgreen")
text(2.099,-12.23,expression(italic("Non") * "-" * italic("A")),cex=1.2,pos=2)
text(2.295,-12.23,expression(italic(A)),cex=1.2,pos=2)


segments(x0=1.4,y0=1.2,x1=1.4,y1=-11.5,lwd=1)
segments(x0=1.52,y0=1.2,x1=1.52,y1=-11.5,lwd=1)
segments(x0=1.64,y0=1.2,x1=1.64,y1=-11.5,lwd=1)
segments(x0=1.4,y0=-11.42,x1=1.64,y1=-11.42)
text(1.4,-11.7,"-1")
text(1.52,-11.7,"-0.5")
text(1.64,-11.7,"0")

rectFill(x1=1.64+De[1]*(1.64-1.4), y1=0.9-0.2, x2=1.64, y2=0.9+0.2, fg="orange", bg = "orange",pch=0,pch.col="orange")

for (i in 1:nrow(A)) {
rectFill(x1=1.64+De[1+i]*(1.64-1.4), y1=0.9-0.2-0.7*i, x2=1.64, y2=0.9+0.2-0.7*i, fg="orange", bg = "orange",pch=0,pch.col="orange")
}


segments(x0=1.67,y0=-12.07,x1=1.67,y1=0.5,lty="dashed",col="purple")
segments(x0=1.62,y0=0.5,x1=1.72,y1=0.5,lty="dashed",col="purple")
segments(x0=1.66,y0=-12.07+0.2,x1=1.67,y1=-12.07,lty="dashed",col="purple")
segments(x0=1.68,y0=-12.07+0.2,x1=1.67,y1=-12.07,lty="dashed",col="purple")
text(1.47,-12.31,expression("Increasing " * italic(m) * "(FPro)"),cex=1.1,pos=4,col="purple")
segments(x0=2.34,y0=-12.2,x1=2.4,y1=-12.2)
segments(x0=2.34,y0=-12.05,x1=2.34,y1=-12.35)
segments(x0=2.4,y0=-12.05,x1=2.4,y1=-12.35)
text(2.4,-12.23,"95% CI",cex=1.1,pos=4,col="black")

dev.off()


