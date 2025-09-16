A<-read.csv("Train.csv",header=TRUE)
A1<-read.csv("Test.csv",header=TRUE)

A<-A[!colnames(A) %in% c('Alcohol','Caffeine','Theobromine','Copper','Selenium','Carotene..alpha','Cryptoxanthin..beta','Lycopene','Lutein...zeaxanthin','Vitamin.B.6','Folate..total','Vitamin.B.12','Choline..total','Vitamin.K..phylloquinone.','Folate..food','Vitamin.E..added','Vitamin.B.12..added','X4.00','X6.00','X8.00','Magnesium','Sugars..total','Vitamin.E..alpha.tocopherol.','Vitamin.D..D2...D3.','Folic.acid','Zinc')]
A<-A[,-1]
A1<-read.csv("Test.csv",header=TRUE)
A1<-A1[,-1]
A1<-A1[!colnames(A1) %in% c('Alcohol','Caffeine','Theobromine','Copper','Selenium','Carotene..alpha','Cryptoxanthin..beta','Lycopene','Lutein...zeaxanthin','Vitamin.B.6','Folate..total','Vitamin.B.12','Choline..total','Vitamin.K..phylloquinone.','Folate..food','Vitamin.E..added','Vitamin.B.12..added','X4.00','X6.00','X8.00','Magnesium','Sugars..total','Vitamin.E..alpha.tocopherol.','Vitamin.D..D2...D3.','Folic.acid','Zinc')]

write.csv(A,"Train_36nutr.csv")
write.csv(A1,"Test_36nutr.csv")
