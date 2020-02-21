
install.packages("readr")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")
library(readr)
library(plyr)
library(ggplot2)
library(magrittr) 
library(dplyr) 
library(tidyr)
library(gridExtra)


custc <- read.csv("C:/Users/admin/Desktop/MVA/PROJECT/TelEco_Customer_Churn.csv")
#dimension
dim(custc)
#structure of dataset
str(custc)

#missing values
sapply(custc, function(x) sum(is.na(x)))
#
custc <- custc[complete.cases(custc),]  ## to remove which has null values
sapply(custc, function(x) sum(is.na(x)))
dim(custc)
#

unique(custc['OnlineSecurity'])
#

#
min(custc$tenure)
max(custc$tenure)
max(custc$MonthlyCharges)
#
custc$SeniorCitizen <- factor(custc$SeniorCitizen)

#Factor the response variable into 0, 1
custc$Churn <- factor(ifelse(custc$Churn == 'No', 0, 1))

#
cus <- custc %>% group_by(Churn)%>%
  summarise(Count = length(Churn)) %>%
  mutate(Rate = Count / sum(Count)*100.0)
cus

ggplot(cus, aes(x = '', y = Rate, fill = Churn)) +
  geom_bar(width = 1, size = 1, color = 'black', stat = 'identity') +
  coord_polar('y') +
  geom_text(aes(label = paste0(round(Rate), '%')),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#999999", "#E69F00"))+
  labs(title = 'Churners Rate') +
  theme_classic() +
  theme(axis.line = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())



custc$MonthlyChargesBin <- NA
custc$MonthlyChargesBin[custc$MonthlyCharges > 0 & custc$MonthlyCharges <= 10] <- '10'
custc$MonthlyChargesBin[custc$MonthlyCharges > 10 & custc$MonthlyCharges <= 20] <- '20'
custc$MonthlyChargesBin[custc$MonthlyCharges > 20 & custc$MonthlyCharges <= 30] <- '30'
custc$MonthlyChargesBin[custc$MonthlyCharges > 30 & custc$MonthlyCharges <= 40] <- '40'
custc$MonthlyChargesBin[custc$MonthlyCharges > 40 & custc$MonthlyCharges <= 50] <- '50'
custc$MonthlyChargesBin[custc$MonthlyCharges > 50 & custc$MonthlyCharges <= 60] <- '60'
custc$MonthlyChargesBin[custc$MonthlyCharges > 60 & custc$MonthlyCharges <= 70] <- '70'
custc$MonthlyChargesBin[custc$MonthlyCharges > 70 & custc$MonthlyCharges <= 80] <- '80'
custc$MonthlyChargesBin[custc$MonthlyCharges > 80 & custc$MonthlyCharges <= 90] <- '90'
custc$MonthlyChargesBin[custc$MonthlyCharges > 90 & custc$MonthlyCharges <= 100] <- '100'
custc$MonthlyChargesBin[custc$MonthlyCharges > 100 & custc$MonthlyCharges <= 110] <- '110'
custc$MonthlyChargesBin[custc$MonthlyCharges > 110 & custc$MonthlyCharges <= 120] <- '120'

custc$MonthlyChargesBin <- factor(custc$MonthlyChargesBin, 
                                      levels = c('10', '20', '30', '40', '50', '60', '70', '80', '90','100','110','120'))



cols_recode1 <- c(10:15)
for (i in 1:ncol(custc[, cols_recode1])) {
  custc[, cols_recode1][, i] <- as.factor(mapvalues(custc[, cols_recode1][, i], from = c("No internet service"), to = c("No")))
}
str(custc)


custc$MultipleLines <- as.factor(mapvalues(custc$MultipleLines, from = c("No phone service"), to = c("No")))

b1 <- ggplot(custc, aes(gender,fill=Churn)) + geom_bar(position='fill') +scale_fill_manual(values=c("#999999", "#E69F00"))
#+theme(legend.position="none")
b2 <- ggplot(custc, aes(SeniorCitizen, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b3 <- ggplot(custc, aes(Partner, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b4 <- ggplot(custc, aes(Dependents, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b5 <- ggplot(custc, aes(PhoneService, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b6 <- ggplot(custc, aes(MultipleLines, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b7 <- ggplot(custc, aes(InternetService, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b8 <- ggplot(custc, aes(OnlineSecurity, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b9 <- ggplot(custc, aes(OnlineBackup, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b10 <- ggplot(custc, aes(DeviceProtection, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b11 <- ggplot(custc, aes(TechSupport, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b12 <- ggplot(custc, aes(StreamingTV, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b13 <- ggplot(custc, aes(StreamingMovies, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b14 <- ggplot(custc, aes(Contract, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b15 <- ggplot(custc, aes(PaperlessBilling, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))
b16 <- ggplot(custc, aes(PaymentMethod, fill = Churn)) + geom_bar(position='fill')+scale_fill_manual(values=c("#999999", "#E69F00"))

grid.arrange(b1,b2,b3,b4, ncol = 2)
grid.arrange(b5,b6,b7,b8,b9,b10,b11,b12,b13, ncol = 3)
grid.arrange(b14,b15,b16, ncol = 2)


boxplot(custc$TotalCharges,data=custc, main="Total Charges")
boxplot(custc$MonthlyCharges,data=custc, main="Monthly Charges")
boxplot(custc$tenure,data=custc, main="Tenure")


#The we look at the 3 variables based on churn.


b1 <- boxplot(tenure~Churn,data = custc,col = c("yellow","green"), xlab ="Churn" , ylab = "tenure")
b2 <- boxplot(MonthlyCharges~Churn,data = custc,col = c("yellow","green"), xlab ="Churn" , ylab = "MonthlyCharges")
b3 <- boxplot(TotalCharges~Churn,data = custc,col = c("yellow","green"), xlab ="Churn" , ylab = "TotalCharges")
#ggplot(custc, aes(xlab = Churn, ylab = TotalCharges)) + geom_boxplot(fill = Churn)
#grid.arrange(b1,b2,b3,ncol = 2)




plot(custc$TotalCharges, custc$tenure)


library(corrplot)
cor_data <-data.frame(custc$tenure,custc$MonthlyCharges,custc$TotalCharges)
corr <- cor(cor_data)
corrplot(corr, method = "number")



hist(custc$tenure, main="Tenure Distribution",xlab="Tenure (Months)")

hist(custc$MonthlyCharges, main="Distribution of Monthly Charges", xlab="Monthly Charges",xlim=c(0,120),breaks=12)

hist(custc$TotalCharges, main="Distribution of Total Charges", xlab="Total Charges")

library(psych)
pairs.panels(custc[c(3,6,8,14,15,16,17,18,19,20,21)],gaps=0)
