# Cluster Analysis full dataset
#install.packages("rlang")
library(readxl)
mydata <- read_excel("C:/Users/owenr/OneDrive/Desktop/2017grefined4.xlsx")
mydata$Happiness.Rank <- NULL
mydata$Whisker.high <- NULL
mydata$Whisker.low <- NULL
#mydata$Happiness.Score <- NULL
View(mydata)
str(mydata)
head(mydata)
GDPperCapita <- mydata$GDP.per.Capita 
LifeExpectancy <- mydata$Life.Expectancy  
Country <- mydata$Country
Freedom <- mydata$Freedom

par(mfrow=(c(1,1)))
# Scatter plots
plot(GDPperCapita ~ LifeExpectancy, data = mydata, col = "blue")
with(mydata,text(GDPperCapita ~ LifeExpectancy, cex=0.1))

plot(GDPperCapita ~ Happiness.Score, data = mydata, col = "red")
with(mydata,text(GDPperCapita ~ Happiness.Score, cex=0.1))

mydata$Happiness.Score <- NULL

# Normalize 
z = mydata[,-c(1,1)]
means = apply(z,2,mean)
sds = apply(z,2,sd)
nor = scale(z,center=means,scale=sds)

#calculate distance matrix (default is Euclidean distance)
distance = dist(nor)
distance
par(mfrow=(c(1,1)))
# Hierarchical agglomerative clustering using "single" linkage 
mydata.hclustsgl <- hclust(distance,method="single")
plot(mydata.hclustsgl ,labels = FALSE, hang=-1, main = "Single linkage")

# Hierarchical agglomerative clustering using "average" linkage 
mydata.hclustavg <- hclust(distance,method="average")
plot(mydata.hclustavg , labels = FALSE, hang=-1, main = "Average linkage")

# Hierarchical agglomerative clustering using default complete linkage 
mydata.hclust = hclust(distance, method = "complete")
plot(mydata.hclust, labels = FALSE, hang=-1, main = "Complete linkage")

# Cluster membership, guessing 3 clusters
member = cutree(mydata.hclust,3)
table(member)

#looking at info of created heirarchil clusters
aggregate(mydata[,-c(1,1)],list(member),mean)

# Silhouette Plot
library(cluster) 
plot(silhouette(cutree(mydata.hclust,3), distance, title = "membership"))

# K-means clustering, nstart runs in n times to get optimal
set.seed(123)
kc<-kmeans(nor,3, nstart = 20)
kc

#varaibiliyt explained diff k values list
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(nor, i)
}

k


# load required packages... thing to show elbow
#install.packages("factoextra")
#install.packages("NbClust")
library(NbClust)
library(factoextra)

#nbclust clustering package
#NbClust package provides 30 indices for determining the number of clusters and proposes
#to user the best clustering scheme from the different results obtained by varying all 
#combinations of number of clusters, distance measures, and clustering methods

NbClust(nor, method = "kmeans")
  #geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  #labs(subtitle = "Elbow method") # add subtitles

par(mfrow=c(1,2))
#plotting k means assigned clusters by colour on graph
plot(GDPperCapita ~ LifeExpectancy, col = kc$cluster)
plot(Freedom ~ LifeExpectancy , col = kc$cluster)

#screeplot to find amount of clcusters
betweenss_totss <- list()
for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

plot(1:10, betweenss_totss, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)")


# MODEL-BASED CLUSTERING ----
library(mclust)
fitM <- Mclust(nor)
plot(fitM)


# DENSITY-BASED CLUSTERING ----
#install.packages("dbscan")
library(dbscan)
kNNdistplot(nor, k = 3) # this line plot shows us an estimate for what the value
#of eps should be, looking at where the graph elbows on the right
abline(h = 2, col = "red", lty = 2)
fitDbscan <- dbscan(nor, eps = 2, minPts = 3)
fitDbscan
plot(nor, col = fitDbscan$cluster)

#install.packages("fpc")
library(fpc)
# Density-based clustering
#install.packages("scales")
library(scales)
# Cluster visualization
#fviz_cluster(fitDbscan, nor, geom = "point")
