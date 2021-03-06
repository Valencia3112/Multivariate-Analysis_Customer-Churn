library(ggplot2)
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
chart.Correlation(quant_var_df, method = c("spearman"),histogram = TRUE, pch = "19", cex= 0.7)


# apply PCA
pca<-prcomp(quant_var_df[,],scale=TRUE)
pca


# sample scores stored in pca$x
# singular values (square roots of eigenvalues) stored in pca$sdev
# loadings (eigenvectors) are stored in pca$rotation
# variable means stored in pca$center
# variable standard deviations stored in pca$scale
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
eigvec.custc <- pca$rotation
round(matlambdas,4)
summary(pca)
pca$rotation
print(pca)
plot(pca)


# Taking the first two PCs to generate linear combinations for all the variables with two factors
pcafactors.custc <- eigvec.custc[,1:2]
pcafactors.custc
# Multiplying each column of the eigenvector's matrix by the square-root of the corresponding eigenvalue in order to get the factor loadings
unrot.fact.custc <- sweep(pcafactors.custc,MARGIN=2,pca$sdev[1:2],`*`)
unrot.fact.custc
# Computing communalities
communalities.custc <- rowSums(unrot.fact.custc^2)
communalities.custc
# Performing the varimax rotation. The default in the varimax function is norm=TRUE thus, Kaiser normalization is carried out
rot.fact.custc <- varimax(unrot.fact.custc)
View(unrot.fact.custc)
rot.fact.custc
# The print method of varimax omits loadings less than abs(0.1). In order to display all the loadings, it is necessary to ask explicitly the contents of the object $loadings
fact.load.custc <- rot.fact.custc$loadings[,1:2]
fact.load.custc
# Computing the rotated factor scores . Notice that signs are reversed for factors F2 (PC2), F3 (PC3) and F4 (PC4)
scale.custc <- scale(quant_var_df[])
scale.custc
#as.matrix(scale.custc)%%fact.load.custc%%solve(t(fact.load.custc)%*%fact.load.custc)

library(psych)
install.packages("psych", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(psych)
fit.pc <- principal(quant_var_df[], nfactors=2, rotate="varimax")
fit.pc
round(fit.pc$values, 4)
fit.pc$loadings
# Loadings with more digits
for (i in c(1,2)) { print(fit.pc$loadings[[1,i]])}
# Communalities
fit.pc$communality
# Rotated factor scores, Notice the columns ordering: RC1, RC2 
fit.pc$scores
# Play with FA utilities

fa.parallel(quant_var_df[]) # See factor recommendation
fa.plot(fit.pc) # See Correlations within Factors
fa.diagram(fit.pc) # Visualize the relationship
vss(quant_var_df[]) # See Factor recommendations for a simple structure
