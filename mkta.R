##Questions 
#1.	Execute market basket with supp=0.01, conf=0.8 and answer below question

#a.	What is the support for {BACK DOOR}  => {KEY FOB}  

#b.	What is the confidence for  {COFFEE} => {SUGAR}   

#2.	supp=0.001, conf=0.8 what are the items with which metal are purchased
#3.	supp=0.004, conf=0.8 what are the items that are purchased with coffee
##Importing the Libraries 
library(arules)
library(plyr)
library(dplyr)
library(readxl)

setwd("C:\\Users\\hp\\Desktop\\Backup_27072020\\R_Practise")
on_data<-read_excel("Online Retail.xlsx")
on_data$InvoiceDate<-as.Date(data$InvoiceDate)
Tran_Data = ddply(on_data,c("InvoiceNo","InvoiceDate"),
                         function(df)paste(df$Description,collapse = ","))

View(head(Tran_Data))
Tran_Data$InvoiceNo<-NULL
Tran_Data$InvoiceDate<-NULL
colnames(Tran_Data)=c("items")
write.csv(Tran_Data,"Info.csv",quote = FALSE, row.names = FALSE)
Transaction = read.transactions("Info.csv", format = 'basket', sep = ',')
summary(Transaction)

#1.	Execute market basket with supp=0.01, conf=0.8 and answer below question
#a.	What is the support for {BACK DOOR}  => {KEY FOB}  

R1<-apriori(Transaction, parameter = list(support = 0.01, conf =0.8, maxlen = 3), appearance = list(lhs = 'BACK DOOR', default ='rhs'))
summary(R1)
inspect(R1)

#b.	What is the confidence for  {COFFEE} => {SUGAR}   

R2<-apriori(Transaction,parameter = list(support = 0.01, conf =0.8), appearance = list(lhs = 'COFFEE', default ='rhs'))
summary(R2) 
inspect(R2)


#2.	supp=0.001, conf=0.8 what are the items with which metal are purchased

R3<-apriori(Transaction,parameter = list(support = 0.001, conf =0.8), appearance = list(rhs = 'METAL', default ='lhs'))
summary(R3) 
inspect(R3)

#3.	supp=0.004, conf=0.8 what are the items that are purchased with coffee

R4<-apriori(Transaction,parameter = list(support = 0.004, conf =0.8), appearance = list(lhs = 'COFFEE', default ='rhs'))
summary(R4) 
inspect(R4)