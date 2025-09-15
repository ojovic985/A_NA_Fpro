A<-read.csv("ndns_rp_yr1-4a_indiv_uk.tab",sep="	",header=TRUE)
A2<-read.csv("ndns_rp_yr5-6a_indiv.tab",sep="	",header=TRUE)
A3<-read.csv("ndns_rp_yr7-8a_indiv.tab",sep="	",header=TRUE)
A4<-read.csv("ndns_rp_yr9-11a_indiv_20211020.tab",sep="	",header=TRUE)

B<-read.csv("Conf_set_ndns_rp_yr1-4a_AllF15_plus_ImpfHeal.csv",header=TRUE)
B<-B[,-1]
B2<-read.csv("Conf_set_ndns_rp_yr5-6a_AllF15_plus_ImpfHeal.csv",header=TRUE)
B2<-B2[,-1]
B3<-read.csv("Conf_set_ndns_rp_yr7-8a_AllF15_plus_ImpfHeal.csv",header=TRUE)
B3<-B3[,-1]
B4<-read.csv("Conf_set_ndns_rp_yr9a_AllF15_plus_ImpfHeal.csv",header=TRUE)
B4<-B4[,-1]
B5<-read.csv("Conf_set_ndns_rp_yr10a_AllF15_plus_ImpfHeal.csv",header=TRUE)
B5<-B5[,-1]
B6<-read.csv("Conf_set_ndns_rp_yr11a_AllF15_plus_ImpfHeal.csv",header=TRUE)
B6<-B6[,-1]

Aq<-A[colnames(A) %in% c("seriali","area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_UKY1234")]

A2q<-A2[colnames(A2) %in% c("seriali","area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_Y56")]

A3q<-A3[colnames(A3) %in% c("seriali","area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_Y78")]

A4q<-A4[colnames(A4) %in% c("seriali","Area","astrata1", "astrata2", "astrata3", "astrata4", "astrata5", "wti_Y911")]

wti_UKY1234r = Aq[,"wti_UKY1234"] * (15655 / 6828) * (4/11)
wti_UKY56r = A2q[,"wti_Y56"] * (15655 / 2546) * (2/11)
wti_UKY78r = A3q[,"wti_Y78"] * (15655 / 2723) * (2/11)
wti_UKY911r = A4q[,"wti_Y911"] * (15655 / 3558) * (3/11)

Aq_2<-cbind(Aq,t(t(wti_UKY1234r)))
A2q_2<-cbind(A2q,t(t(wti_UKY56r)))
A3q_2<-cbind(A3q,t(t(wti_UKY78r)))
A4q_2<-cbind(A4q,t(t(wti_UKY911r)))
colnames(Aq_2)[ncol( Aq_2)]<-"Comb_weights"
colnames(A2q_2)[ncol( A2q_2)]<-"Comb_weights"
colnames(A3q_2)[ncol( A3q_2)]<-"Comb_weights"
colnames(A4q_2)[ncol( A4q_2)]<-"Comb_weights"
colnames(A4q_2)<-gsub("Area","area",colnames(A4q_2))

Aq_2<- Aq_2[!colnames(Aq_2) %in% c("wti_UKY1234")]
A2q_2<- A2q_2[!colnames(A2q_2) %in% c("wti_Y56")]
A3q_2<- A3q_2[!colnames(A3q_2) %in% c("wti_Y78")]
A4q_2<- A4q_2[!colnames(A4q_2) %in% c("wti_Y911")]

Aq_2<-Aq_2[,colnames(A2q_2)]

colnames(Aq_2)==colnames(A2q_2)
colnames(Aq_2)==colnames(A3q_2)
colnames(Aq_2)==colnames(A4q_2)

Auk<-rbind(Aq_2, A2q_2, A3q_2, A4q_2)


B<-B[!colnames(B) %in% c("eqvinc")]
B2<-B2[!colnames(B2) %in% c("eqvinc")]
B3<-B3[!colnames(B3) %in% c("eqvinc")]

colnames(B)==colnames(B2)
colnames(B)==colnames(B3)
colnames(B)==colnames(B4)
colnames(B)==colnames(B5)
colnames(B)==colnames(B6)

Buk<-rbind(B,B2,B3,B4,B5,B6)


C<-merge(Auk,Buk,by="seriali")

data=C

write.csv(data,"Complex_survey_design_data_NonNut.csv")



