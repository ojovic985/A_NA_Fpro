## After the files hn23_24rc.csv - hn13_24rc.csv files have been obtained:


A<-read.csv('hn23_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

positions$'98023'
#[1] 97568

 i=96637
positions[[as.character(i)]]
#[1] 130636 145095 446446 446554

length(positions[[as.character(i)]])
#[1] 4

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn23_24rc.csv")



A<-read.csv('hn22_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

positions$'98023'
#[1] 97568

 i=96637
positions[[as.character(i)]]
#[1] 130636 145095 446446 446554

length(positions[[as.character(i)]])
#[1] 4

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn22_24rc.csv")




A<-read.csv('hn21_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

positions$'98023'
#[1] 97568

 i=96637
positions[[as.character(i)]]
#[1] 130636 145095 446446 446554

length(positions[[as.character(i)]])
#[1] 4

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn21_24rc.csv")




A<-read.csv('hn20_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

positions$'98023'
#[1] 97568

 i=96637
positions[[as.character(i)]]
#[1] 130636 145095 446446 446554

length(positions[[as.character(i)]])
#[1] 4

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn20_24rc.csv")




A<-read.csv('hn19_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

positions$'98023'
#[1] 97568

 i=96637
positions[[as.character(i)]]
#[1] 130636 145095 446446 446554

length(positions[[as.character(i)]])
#[1] 4

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn19_24rc.csv")




A<-read.csv('hn18_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

positions$'98023'
#[1] 97568

 i=96637
positions[[as.character(i)]]
#[1] 130636 145095 446446 446554

length(positions[[as.character(i)]])
#[1] 4

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn18_24rc.csv")




A<-read.csv('hn17_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

positions$'98023'
#[1] 97568

 i=96637
positions[[as.character(i)]]
#[1] 130636 145095 446446 446554

length(positions[[as.character(i)]])
#[1] 4

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn17_24rc.csv")




A<-read.csv('hn16_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

positions$'98023'
#[1] 97568

 i=96637
positions[[as.character(i)]]
#[1] 130636 145095 446446 446554

length(positions[[as.character(i)]])
#[1] 4

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn16_24rc.csv")



A<-read.csv('hn15_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn15_24rc.csv")




A<-read.csv('hn14_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn14_24rc.csv")



A<-read.csv('hn13_24rc.csv',header=TRUE)

Codes<-unique(A[,"N_FCODE"])

counts<-table(A[,"N_FCODE"])

repeated_vals<-names(counts)
positions<-lapply(repeated_vals, function(x) which(A[,"N_FCODE"]==as.numeric(x)))
names(positions)<-repeated_vals

Torrs<-sort(Codes)
B<-A[,which(colnames(A)=="NF_WATER"):which(colnames(A)=="NF_VITC")]
F_code<-matrix(rep(0,length(Torrs)*(dim(B)[2]+1)),nrow=length(Torrs))
colnames(F_code)<-cbind(t(colnames(B)),"N_FCODE")
F_code[,ncol(F_code)]<-Torrs

for (j in 1:length(Torrs)) {
for (k in 1:length(positions[[as.character(Torrs[j])]])) {
if (k==1) { 
tumins<-B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]) }
else {
tumins<-rbind(tumins,B[positions[[as.character(Torrs[j])]][k],]*(100/ A[positions[[as.character(Torrs[j])]][k],"NF_INTK"]))}
}
vumins<-array(ncol(tumins))
for (i in 1:ncol(tumins)) {
vumins[i]<-median(tumins[,i]) }

F_code[j,1:dim(B)[2]]<-vumins}

write.csv(F_code,"F_codes_nutr_cont_per_100g_of_hn13_24rc.csv")


