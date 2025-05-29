
#### AFTER "MAIN_github_code_Approach_1_2_and_3_results_for_each_p_value_3.R" was run - then run this code! Not before.


library(openxlsx)

wb <- createWorkbook()

A<-read.csv("Labels_for_main_data.csv",header=TRUE)
A<-as.data.frame(A)

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

addWorksheet(wb, "p_0.025")
writeData(wb, sheet = "p_0.025", x = cbind(A,A1))

addWorksheet(wb, "p_0.05")
writeData(wb, sheet = "p_0.05", x = cbind(A,A2))

addWorksheet(wb, "p_0.1")
writeData(wb, sheet = "p_0.1", x = cbind(A,A3))

addWorksheet(wb, "p_0.2")
writeData(wb, sheet = "p_0.2", x = cbind(A,A4))

addWorksheet(wb, "p_0.3")
writeData(wb, sheet = "p_0.3", x = cbind(A,A5))

addWorksheet(wb, "p_0.5")
writeData(wb, sheet = "p_0.5", x = cbind(A,A6))

addWorksheet(wb, "p_0.7")
writeData(wb, sheet = "p_0.7", x = cbind(A,A7))

addWorksheet(wb, "p_0.8")
writeData(wb, sheet = "p_0.8", x = cbind(A,A8))

saveWorkbook(wb, "Supplementary_Table_Results_Main.xlsx", overwrite = TRUE)


