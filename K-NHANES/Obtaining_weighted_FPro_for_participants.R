
A<-read.csv('hn13_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn13_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn13_all_kindg1_n21&22.csv")


A<-read.csv('hn14_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn14_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn14_all_kindg1_n21&22.csv")

A<-read.csv('hn15_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn15_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn15_all_kindg1_n21&22.csv")

A<-read.csv('hn16_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn16_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn16_all_kindg1_n21&22.csv")

A<-read.csv('hn17_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn17_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn17_all_kindg1_n21&22.csv")

A<-read.csv('hn18_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn18_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn18_all_kindg1_n21&22.csv")

A<-read.csv('hn19_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn19_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn19_all_kindg1_n21&22.csv")

A<-read.csv('hn20_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn20_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn20_all_kindg1_n21&22.csv")

A<-read.csv('hn21_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn21_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn21_all_kindg1_n21&22.csv")

A<-read.csv('hn22_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn22_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn22_all_kindg1_n21&22.csv")

A<-read.csv('hn23_24rc.csv',header=TRUE)
B<-read.csv('N_Code_and_FPro_of_hn23_24rc.csv',header=TRUE)

B<-B[,-1]
C<-merge(B,A,by="N_FCODE",all.x=TRUE)
C<-C[order(C[,"ID"]),]
counts<-table(C[,"ID"])
dezi<-as.numeric(counts)

Weigh_FPro<-rep(0,length(dezi))
for (j in 1:length(dezi)) {
corsum<-0
if (j==1) {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k]!=21)&&(C$N_KINDG1[k]!=22)) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k,"FPro"]*C[k,"NF_INTK"])
corsum<-corsum+C[k,"NF_INTK"]} }}
else {
for (k in 1:dezi[j]) {
if  ((C$N_KINDG1[k+cumsum(dezi)[j-1]]!=21)&&(C$N_KINDG1[k+cumsum(dezi)[j-1]]!=22) ) {
Weigh_FPro[j]<-Weigh_FPro[j]+(C[k+cumsum(dezi)[j-1],"FPro"]*C[k+cumsum(dezi)[j-1],"NF_INTK"])
corsum<-corsum+C[k+cumsum(dezi)[j-1],"NF_INTK"]}}}
Weigh_FPro[j]<-Weigh_FPro[j]/corsum}

repeated_vals<-names(counts)

Mat<-data.frame(cbind(t(t(repeated_vals)),t(t(Weigh_FPro))))

write.csv(Mat,"ID_and_Weigh_FPro_for_hn23_all_kindg1_n21&22.csv")


