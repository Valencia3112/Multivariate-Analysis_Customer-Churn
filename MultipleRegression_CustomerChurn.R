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
library(caret)
library(caTools)
library(pROC)
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

custc <- mutate(custc,tenure_Range = tenure)
cut(custc$tenure_Range,6,labels = c('0-1 Years','1-2 Years','2-3 Years','4-5 Years','5-6 Years','6-7 Years'))
custc$tenure_Range <- cut(custc$tenure_Range,6,labels = c('0-1 Years','1-2 Years','2-3 Years','4-5 Years','5-6 Years','6-7 Years'))

#Checking if there are any NULL values in any of the columns  
table(is.na(custc))
str_detect(custc,'NA')
setDT(custc)
custc[is.na(TotalCharges),NROW(TotalCharges)]

#There are 11 rows out of 7043 rows that have null values.Hence removing these rows since they are only 0.15% of total so we can afford to drop them

custc <- custc[complete.cases(custc), ]
dim(custc)

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

#Converting Categorical Columns into numerical values 
custc$Partner<-ifelse(custc$Partner=='Yes', 1,2)
custc$Dependents<-ifelse(custc$Dependents=='Yes', 1,2)
custc$PhoneService<-ifelse(custc$PhoneService=='Yes', 1,2)
custc$OnlineBackup<-ifelse(custc$OnlineBackup=='Yes', 1,2)
custc$OnlineSecurity<-ifelse(custc$OnlineSecurity=='Yes', 1,2)
custc$DeviceProtection<-ifelse(custc$DeviceProtection=='Yes', 1,2)
custc$TechSupport<-ifelse(custc$TechSupport=='Yes', 1,2)
custc$StreamingTV<-ifelse(custc$StreamingTV=='Yes', 1,2)
custc$StreamingMovies<-ifelse(custc$StreamingMovies=='Yes', 1,2)
custc$PaperlessBilling<-ifelse(custc$PaperlessBilling=='Yes', 1,2)
custc$Churn<-ifelse(custc$Churn=='Yes', 1,2)

#Converting Categorical Columns into numerical values
custc$gender<-factor(custc$gender,levels = c('Female','Male'),labels = c(1,2))
custc$MultipleLines<-factor(custc$MultipleLines,levels = c('No','Yes','No phone service'),labels = c(1,2,3))
custc$InternetService<-factor(custc$InternetService,levels = c('No','Fiber optic','DSL'),labels = c(1,2,3))
custc$Contract<-factor(custc$Contract,levels = c('Month-to-month','One year','Two year'),labels = c(1,2,3))
custc$PaymentMethod<-factor(custc$PaymentMethod,levels = c('Bank transfer (automatic)',
                                                                     'Credit card (automatic)',
                                                                     'Electronic check',
                                                                     'Mailed check'),labels = c(1,2,3,4))

#Checking the dimension of the dataset again after performing above changes

dim(custc)
str(custc)

##Converting factors into numeric columns
custc$gender<-as.numeric(custc$gender)
custc$SeniorCitizen<-as.numeric(custc$SeniorCitizen)
custc$tenure<-as.numeric(custc$tenure)
custc$MultipleLines<-as.numeric(custc$MultipleLines)
custc$InternetService<-as.numeric(custc$InternetService)
custc$Contract<-as.numeric(custc$Contract)
custc$PaymentMethod<-as.numeric(custc$PaymentMethod)

custc$Churn<-as.integer(custc$Churn)

str(custc)

dim(custc)


#MULTIPLE Regression

##Performing regression taking all independent variables

fit <- lm(Churn~gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService
          +MultipleLines+InternetService+OnlineBackup+OnlineSecurity
          +DeviceProtection+TechSupport+StreamingTV+StreamingMovies
          +Contract+PaperlessBilling+PaymentMethod+MonthlyCharges
          +TotalCharges, data=custc)

summary(fit)
coefficients(fit)
confint(fit,level=0.95)

# Predicted Values
fitted(fit)
residuals(fit)

#Anova Table
anova(fit)
vcov(fit)
cov2cor(vcov(fit))
temp <- influence.measures(fit)
temp

##Diagnostics Plots for visualization
plot(fit)

#Finding Outliers and plottting scatterplot for visual analysis
outlierTest(fit)
qqPlot(fit, main="QQ Plot")

#Plotting leverage plots
leveragePlots(fit) 

# Plotting variable plots for Influential Observations

avPlots(fit)

# Normality of Residuals and qqplot for studentized resid
qqPlot(fit, main="QQ Plot")

#Distribution of studentized residuals
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

#Non-constant Error Variance
#Evaluating homoscedasticity
#Non-constant error variance test
ncvTest(fit)

#Plotting studentized residuals v/s. fitted values
spreadLevelPlot(fit)

#Multi-collinearity
# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2

#Nonlinearity
# component + residual plot
crPlots(fit)


#Global test of model assumptions

##In the below steps we are performing regressions using all independent variable and we keep
##on eliminating the ones which are not significant in the next regression until we see
##constant values for R2

gvmodel <- gvlma(fit)
summary(gvmodel)
fit
summary(fit)
fit1<-fit
fit2<-  lm(Churn~gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService
           +MultipleLines+InternetService+OnlineBackup+OnlineSecurity
           +DeviceProtection+TechSupport+StreamingTV+StreamingMovies
           +Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=custc)
summary(fit2)



fit3<-  lm(Churn~gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService
           +MultipleLines+InternetService+OnlineBackup+OnlineSecurity
           +DeviceProtection+TechSupport+StreamingTV+
             Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=custc)
summary(fit3)

fit4<-  lm(Churn~gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService
           +MultipleLines+InternetService+OnlineBackup+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=custc)
summary(fit4)


fit5<-  lm(Churn~gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService
           +MultipleLines+InternetService+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=custc)
summary(fit5)

fit6<-  lm(Churn~gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService
           +MultipleLines+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=custc)
summary(fit6)

fit7<-  lm(Churn~SeniorCitizen+Dependents+tenure+PhoneService
           +MultipleLines+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=custc)
summary(fit7)


fit7<-  lm(Churn~SeniorCitizen+Dependents+tenure+PhoneService+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=custc)
summary(fit7)

fit8<-  lm(Churn~SeniorCitizen+tenure+PhoneService+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=custc)
summary(fit8)

fit9<-  lm(Churn~tenure+MonthlyCharges+TotalCharges, data=custc)
summary(fit9)

##By running the above regression we notice that independent variables like SeniorCitizen,tenure,PhoneService,OnlineSecurity
##DeviceProtection,TechSupportContract,PaperlessBilling,MonthlyCharges and TotalCharges could be factors
##that cause person to churn 

#Comparing model
anova(fit1,fit8)

step <- stepAIC(fit, direction="both")
step$anova

predict.lm(fit8, data.frame(SeniorCitizen = 0,tenure =20,PhoneService = 1,OnlineSecurity = 2,
                            DeviceProtection = 1,TechSupport = 2,Contract = 3,PaperlessBilling = 1,
                            MonthlyCharges = 100,TotalCharges=1000) )

##By running the above prediction model using values like the user not being senior citizen,tenure
##being 20 months, leveraging Phoneservice,DeviceProtection and PaperlessBilling, not using OnlineSecurity
##and TechSupport, having contract of 2 years, MonthlyCharges and TotalCharges being 100 &1000 respectively
##there is a likelihood of this customer churning 