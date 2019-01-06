# Programming Assignment 1
# Name: Aaftab Jandeer
# Student No: 301169437

require(stats)
require(ggplot2)
require(cluster)
require(clusteval)

  ## Task 1
#setwd("~/Documents/cmpt459/codingAssignments/codingAssign1")
theData <- read.csv(file="./wine.csv", header=TRUE, sep=",")       #reads csv file
myData0 <- as.data.frame(theData)
myData <- myData0[ ,2:14]                                          #omits first column of data


  ## Task 2
myDataScaled <- data.frame(lapply(myData, function(x) (x-mean(x))/sd(x)))     #scales data using self-created z-score function
myDataScaled


  ## Task 3
distance <- dist(myDataScaled)                     #calculate euclidean distance
kmeansClust1 <- kmeans(myDataScaled, centers=4)                  #perform k-means clustering

sil <- silhouette(kmeansClust1$cluster,distance)
summary(sil)


  ## Task 4                                                           
groupChem1 <- kmeans(myDataScaled[,c("Alcohol","Ash")], centers=4)                #compute k-means clustering on pair of attributes
ggplot(myDataScaled, aes(x=Alcohol,y=Ash)) + geom_point(aes(colour= factor(groupChem1$cluster)))         #scatterplot of clustering

groupChem2 <- kmeans(myDataScaled[,c("Acl","Mg")], centers=4)
ggplot(myDataScaled, aes(x=Acl,y=Mg)) + geom_point(aes(colour= factor(groupChem2$cluster)))

groupChem3 <- kmeans(myDataScaled[,c("Phenols","Flavanoids")], centers=4)
ggplot(myDataScaled, aes(x=Phenols,y=Flavanoids)) + geom_point(aes(colour= factor(groupChem3$cluster)))

groupChem4 <- kmeans(myDataScaled[,c("Nonflavanoid.phenols","Proanth")], centers=4)
ggplot(myDataScaled, aes(x=Nonflavanoid.phenols,y=Proanth)) + geom_point(aes(colour= factor(groupChem4$cluster)))

groupChem5 <- kmeans(myDataScaled[,c("Color.int","OD")], centers=4)
ggplot(myDataScaled, aes(x=Color.int,y=OD)) + geom_point(aes(colour= factor(groupChem5$cluster)))

groupChem6 <- kmeans(myDataScaled[,c("Proline","Hue")], centers=4)
ggplot(myDataScaled, aes(x=Proline,y=Hue)) + geom_point(aes(colour= factor(groupChem6$cluster)))

groupChem7 <- kmeans(myDataScaled[,c("Color.int","Ash")], centers=4)
ggplot(myDataScaled, aes(x=Color.int,y=Ash)) + geom_point(aes(colour= factor(groupChem7$cluster)))


  ## Task 5
kmeansClust2 <- kmeans(myDataScaled, centers=3)          #perform k-means clustering
sil2 <- silhouette(kmeansClust2$cluster,distance)        #calculate silhouette score for corresponding clustering
summary(sil2)
#kmeansClust2 <- kmeans(myDataScaled, centers=5)        #experiment with different number of clusters
#kmeansClust2 <- kmeans(myDataScaled, centers=8)        #experiment with different number of clusters
#kmeansClust2 <- kmeans(myDataScaled, centers=2)        #experiment with different number of clusters


  ## Task 6
completeLink <- hclust(distance, method="complete")   #hierarchical cluster analysis using complete linkage algorithm
plot(completeLink)

avgLink <- hclust(distance, method="average")         #hierarchical cluster analysis using average linkage algorithm
plot(avgLink)

singleLink <- hclust(distance, method="single")       #hierarchical cluster analysis using single linkage algorithm
plot(singleLink)


  ## Task 7
cut1 <- cutree(completeLink, 3)          #cut the complete linkage dendrogram
cut1
cut2 <- cutree(avgLink, 3)              #cut the average linkage dendrogram 
cut2
cut3 <- cutree(singleLink, 3)          #cut the single linkage dendrogram 
cut3


  ## Task 8                                  
trueLabels <- myData0[,c("Wine")]          #retrieve the "wine" column

clusteval:::rand_indep(trueLabels,cut1)    #calculate rand index 
clusteval:::rand_indep(trueLabels,cut2)    #calculate rand index 
clusteval:::rand_indep(trueLabels,cut3)    #calculate rand index 

