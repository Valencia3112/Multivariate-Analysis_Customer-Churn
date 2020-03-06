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


dim(custc)

c


# Hirerarchic cluster analysis, Nearest-neighbor

# Standardizing the data with scale()
matstd.custc<- scale(quant_var_df[,])
matstd.custc
# Creating a (Euclidean) distance matrix of the standardized data
dist.custc <- dist(matstd.custc, method="euclidean")
dist.custc
# Invoking hclust command (cluster analysis by single linkage method)
cluscustc.nn <- hclust(dist.custc, method = "single")
cluscustc.nn

#Plotting

# Create extra margin room in the dendrogram, on the bottom (Countries labels)
par(mar=c(8, 4, 4, 2) + 0.1)
# Object "clusemploy.nn" is converted into a object of class "dendrogram"
# in order to allow better flexibility in the (vertical) dendrogram plotting.
plot(as.dendrogram(cluscustc.nn),ylab="Distance between countries",ylim=c(0,6),
     main="Dendrogram. People employed in nine industry groups \n  from European countries")

dev.new()
par(mar=c(5, 4, 4, 7) +0.1)
plot(as.dendrogram(cluscustc.nn), xlab= "Distance between countries", xlim=c(6,0),
     horiz = TRUE,main="Dendrogram. People employed in nine industry groups from European countries")

