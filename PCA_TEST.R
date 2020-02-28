custc <- read.csv("C:/Users/admin/Desktop/MVA/PROJECT/TelEco_Customer_Churn.csv")
dim(custc)
#structure of dataset
str(custc)


sapply(custc, function(x) sum(is.na(x)))
#
custc <- custc[complete.cases(custc),]  ## to remove which has null values
sapply(custc, function(x) sum(is.na(x)))
dim(custc)

quant_var_df<- data.frame(custc$tenure,custc$MonthlyCharges,custc$TotalCharges)
quant_var_df
attach(quant_var_df)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(custc[quant_var], method = c("spearman"),histogram = TRUE, pch = "19", cex= 0.7)


# apply PCA
pca<-prcomp(quant_var_df[,],scale=TRUE)
pca


# sample scores stored in sparrows_pca$x
# singular values (square roots of eigenvalues) stored in sparrow_pca$sdev
# loadings (eigenvectors) are stored in sparrows_pca$rotation
# variable means stored in sparrows_pca$center
# variable standard deviations stored in sparrows_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2

(eigen_custc <- pca$sdev^2)
names(eigen_custc) <- paste("PC",1:3,sep="")
eigen_custc
sumlambdas <- sum(eigen_custc)
sumlambdas
propvar <- eigen_custc/sumlambdas
propvar
#pc1 holds 72 % of variance ,pc1 n pc2 holds 82% of variance

cumvar_custc <- cumsum(propvar)
cumvar_custc
matlambdas <- rbind(eigen_custc,propvar,cumvar_custc)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
summary(pca)
pca$rotation
print(pca)
# Sample scores stored in sparrow_pca$x
pca$x
#observation values after applying pc


# Identifying the scores by their survival status
custch_pca <- cbind(data.frame(custc$Churn),pca$x)
custch_pca
# Means of scores for all the PC's classified by Survival status
#2 means are significantly different
churnmeansPC <- aggregate(custch_pca[,-1],by=list(Churn=custc$Churn),mean)
churnmeansPC

churnmeansPC <- churnmeansPC[rev(order(churnmeansPC$Churn)),]
churnmeansPC
churnfmeans <- t(churnmeansPC[,-1])
churnfmeans
colnames(churnfmeans) <- t(as.vector(churnmeansPC[1]))
churnfmeans

# Standard deviations of scores for all the PC's classified by Survival status
tabsdsPC <- aggregate(custch_pca[,-1],by=list(Churn=custc$Churn),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds
t.test(PC1~custc$Churn,data=custch_pca)
t.test(PC2~custc$Churn,data=custch_pca)
t.test(PC3~custc$Churn,data=custch_pca)

# F ratio tests
var.test(PC1~custc$Churn,data=custch_pca)
var.test(PC2~custc$Churn,data=custch_pca)
var.test(PC3~custc$Churn,data=custch_pca)

# Levene's tests (one-sided)
(LTPC1 <- leveneTest(PC1~custc$Churn,data=custch_pca))
library(car)
(LTPC1 <- leveneTest(PC1~custc$Churn,data=custch_pca))
(p_PC1_1sided <- LTPC1[[3]][1]/2)
(LTPC2 <- leveneTest(PC2~custc$Churn,data=custch_pca))
(p_PC2_1sided=LTPC2[[3]][1]/2)
(LTPC3 <- leveneTest(PC3~custc$Churn,data=custch_pca))
(p_PC3_1sided <- LTPC3[[3]][1]/2)

# Plotting the scores for the first and second components
plot(custch_pca$PC1, custch_pca$PC2,pch=ifelse(custch_pca$Churn == "Yes",1,16),xlab="PC1", ylab="PC2", main="49 custc against values for PC1 & PC2")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Yes","No"), pch=c(1,16))
plot(eigen_custc, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_custc), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
print(summary(pca))
View(pca)
#diagonal --sum(diag)=5
diag(cov(pca$x))
xlim <- range(pca$x[,1])
pca$x[,1]
pca$x
plot(pca$x,xlim=xlim,ylim=xlim)
pca$rotation[,1]
pca$rotation
plot(custc[,-1])
pca$x
plot(pca)
#get the original value of the data based on PCA
center <- pca$center
scale <-  pca$scale
new_custc <- as.matrix(quant_var_df)
new_custc

out <- sapply(1:3, function(i){plot(custc$Churn,pca$x[,i],xlab=paste("PC",i,sep=""),ylab="Churn")})
out
pairs(pca$x[,1:3], ylim = c(-6,4),xlim = c(-6,4),panel=function(x,y,...){text(x,y,custc$Churn)})






























