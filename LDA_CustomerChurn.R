library(ggplot2)
library(dplyr)
library (stringr)
library(data.table)
library(grid)
library(gridExtra)
library(corrplot)
library(scales)
library(qqplotr)
library(MASS)
library(DMwR)
library(car)
library(e1071)
library(regclass)
library(caret)
library(caTools)
library(pROC)
library(ROCR)
library(tidyverse)
library(MVA)
library(GGally)
library(gvlma)



##-----------------------------------------------------------------------------------
##Importing Dataset and doing preliminary analysis
##-----------------------------------------------------------------------------------

#Importing CSV file from drive on my local computer and viewing it 

custc <- read.csv("C:/Users/admin/Desktop/MVA/PROJECT/TelEco_Customer_Churn.csv")
custc <- as.data.frame(custc)
View(custc)

#Checking the Dimension of the dataset

dim(custc)

#Viewing the first 4 rows of the dataset to get the overview of the dataset

head(custc,4)

#Gaining more insight about the kind of data stored in each column

summary(custc)
glimpse(custc)

#The above results give us an insight that TotalCharges and MonthlyCharges are numerical values
#SeniorCitizen and tenure are stored as numerical which need to be converted to categorical variables



##-----------------------------------------------------------------------------------
## Performing Data Cleaning and Formatting
##-----------------------------------------------------------------------------------

#Converting SeniorCitizen numerical variable into Categorical Variable 

custc$SeniorCitizen<-factor(custc$SeniorCitizen,levels = c(0 ,1),labels = c('no','yes'))

#Converting tenure values into ranges of 12 months

custc <- mutate(custc,Tenure_Range =tenure)
cut(custc$Tenure_Range,6,labels = c('0-1 Years','1-2 Years','2-3 Years','4-5 Years','5-6 Years','6-7 Years'))
custc$Tenure_Range <- cut(custc$Tenure_Range,6,labels = c('0-1 Years','1-2 Years','2-3 Years','4-5 Years','5-6 Years','6-7 Years'))

#Checking if there are any NULL values in any of the columns  
table(is.na(custc))
str_detect(custc,'NA')
setDT(custc)
custc[is.na(TotalCharges),NROW(TotalCharges)]

#There are 11 rows out of 7043 rows that have null values.Hence removing these rows since they are only 0.15% of total so we can afford to drop them

custc <- custc[complete.cases(custc), ]

#Replacing 'No Internet Service' values in OnlineSecurity,OnlineBackup DeviceProtection,TechSupport,StreamingTV and StreamingMovies columns with 'No'

custc$OnlineSecurity[custc$OnlineSecurity=='No internet service'] <- 'No'
custc$OnlineBackup[custc$OnlineBackup=='No internet service'] <- 'No'
custc$DeviceProtection[custc$DeviceProtection=='No internet service'] <- 'No'
custc$TechSupport[custc$TechSupport=='No internet service'] <- 'No'
custc$StreamingTV[custc$StreamingTV=='No internet service'] <- 'No'
custc$StreamingMovies[custc$StreamingMovies=='No internet service'] <- 'No'

#Deleting the unused levels from the factor variables

custc$OnlineSecurity <- factor(custc$OnlineSecurity)
custc$OnlineBackup <- factor(custc$OnlineBackup)
custc$DeviceProtection <- factor(custc$DeviceProtection)
custc$TechSupport <- factor(custc$TechSupport)
custc$StreamingTV <- factor(custc$StreamingTV)
custc$StreamingMovies <- factor(custc$StreamingMovies)


##------------------Linear Discriminant Analysis (LDA)---------------------##

##Using same independent variables that we found from logistic regression and performing LDA to see how well we would be able to predict using this model

custc.data <-(custc[,c("SeniorCitizen","Partner","Dependents","Tenure_Range",
                                 "PhoneService","InternetService","OnlineBackup","OnlineSecurity",
                                 "DeviceProtection","TechSupport","Contract",
                                 "PaperlessBilling","PaymentMethod","Churn")])


##Splitting data into 75% training and 25% test so that we have some data we can test our model on

smp_size_churn <- floor(0.75 * nrow(custc.data))
train_ind_churn <- sample(nrow(custc.data), size = smp_size_churn)
train_churn.df <- as.data.frame(custc.data[train_ind_churn, ])
test_churn.df <- as.data.frame(custc.data[-train_ind_churn, ])

##Performing LDA on our training data

custc.lda <- lda(Churn~SeniorCitizen+Partner+Dependents+Tenure_Range+
                        PhoneService+InternetService+OnlineBackup+OnlineSecurity+
                        DeviceProtection+TechSupport+Contract+
                        PaperlessBilling+PaymentMethod, data=train_churn.df)

plot(custc.lda)

##Making predictions on our testing data 

custc.lda.predict <- predict(custc.lda, newdata = test_churn.df)

### CONSTRUCTING ROC AUC PLOT:

# Get the posteriors as a dataframe.
custc.lda.predict.posteriors <- as.data.frame(custc.lda.predict$posterior)
head(custc.lda.predict.posteriors)

# Evaluating the model

pred <- prediction(custc.lda.predict.posteriors[,2], test_churn.df$Churn)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plotting the graph for better visualization

plot(roc.perf)
abline(a=0, b= 1)
text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

##From the above results we see that we get AUC value as 83.5% using LDA which implies this model is good
##fit and the predictors used in this model can influence our dependent variable Churn.
