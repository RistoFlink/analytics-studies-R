rm(list = ls())
dev.off()
install.packages("NbClust")
install.packages("scales")
install.packages("purrr")

library(NbClust)
library(scales)
library(purrr)
library(dplyr)
library(ggplot2)
library(patchwork)

#loading the first data set
initdata = data.frame(read.csv("artificial2_Mac.csv", header = TRUE, sep = ","))

#glimpse into the data
str(initdata)
summary(initdata)

ggplot(initdata, aes(x = BrandLoyalty, y = PriceSensitivity)) +
  geom_point() +
  ggtitle("First dataset")

#scaling the data (not that necessary in this example due to the existing values being close to each other)
data = data.frame(apply(initdata, 2, rescale, to = c(0, 1)))
summary(data)

#hierarchical clustering
#getting the distance matrix (only the lower half since it's mirrored)
distmat = dist(data, method = "euclidean")
hmdl = hclust(distmat, method = "complete")

#making the dendrogram
dendhmdl = as.dendrogram(hmdl)

#plotting the dendrogram
plot(dendhmdl, xlab = "observation", ylab = "height of the tree", main = "Dendrogram")

#getting the two-cluster solution
clustermember = factor(cutree(hmdl, k = 2))

ggplot(initdata, aes(x = BrandLoyalty, y = PriceSensitivity, col = clustermember)) +
  geom_point() +
  ggtitle("Two-cluster solution")

#getting the three-cluster solution
clustermember = factor(cutree(hmdl, k = 3))

ggplot(initdata, aes(x = BrandLoyalty, y = PriceSensitivity, col = clustermember)) +
  geom_point() +
  ggtitle("Three-cluster solution")

#using kmeans for k = 2
kmeansmdl = kmeans(data, centers = 2, nstart = 25)
kmeansmdl$cluster
kmeansmdl$centers
kmeansmdl$withinss
kmeansmdl$tot.withinss

#plotting the result

ggplot(data, aes(x = BrandLoyalty, y = PriceSensitivity, col = factor(kmeansmdl$cluster))) +
  geom_point() +
  geom_point(aes(x = kmeansmdl$centers[1, 1], y = kmeansmdl$centers[1, 2]), colour = "black", size = 5) +
  geom_point(aes(x = kmeansmdl$centers[2, 1], y = kmeansmdl$centers[2, 2]), colour = "black", size = 5) +
  ggtitle("K-means - two clusters")


#using kmeans for k = 3
kmeansmdl = kmeans(data, centers = 3, nstart = 25)

ggplot(data, aes(x = BrandLoyalty, y = PriceSensitivity, col = factor(kmeansmdl$cluster))) +
  geom_point() +
  geom_point(aes(x = kmeansmdl$centers[1, 1], y = kmeansmdl$centers[1, 2]), colour = "black", size = 5) +
  geom_point(aes(x = kmeansmdl$centers[2, 1], y = kmeansmdl$centers[2, 2]), colour = "black", size = 5) +
  geom_point(aes(x = kmeansmdl$centers[3, 1], y = kmeansmdl$centers[3, 2]), colour = "black", size = 5) +
  ggtitle("K-means - three clusters")

#utilizing the Elbow method to determine the optimal amount of clusters
tot_within_ss = map_dbl(1:10, function(k){
  kmeansmdl = kmeans(data, centers = k, nstart = 25)
  kmeansmdl$tot.withinss
})
#converting the variable to a data frame in order to use ggplot2
tot_within_ss = data.frame(tot_within_ss)
str(tot_within_ss)

ggplot(tot_within_ss, aes(x = 1:10, y = tot_within_ss)) +
  geom_line() +
  ggtitle("Elbow method")

#plotting with the core package
plot(1:10, tot_within_ss$tot_within_ss, type = "o")
grid()

#utilizing the Silhouette method and Calinski-Harabasz, Gap-statistic to determine the optimal amount of clusters
SilClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "silhouette")
GapClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "gap")
CHClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ch")

#plotting all 3 of them at the same time with the base package
par(mfrow = c(1, 3))
plot(2:10, SilClust$All.index, type = "o", col = "forestgreen", xlab = "Number of clusters k", ylab = "Silhouette width")
grid()
plot(2:10, GapClust$All.index, type = "o", col = "forestgreen", xlab = "Number of clusters k", ylab = "Gap statistic")
grid()
plot(2:10, CHClust$All.index, type = "o", col = "forestgreen", xlab = "Number of clusters k", ylab = "Calinski-Harabasz index")
grid()

#doing the clustering based on the results and analyzing the data
kmeansmdl = kmeans(data, centers = 3, nstart = 25)
initdata$cluster = kmeansmdl$cluster
str(initdata)

initdata %>%
  group_by(cluster) %>%
  summarise_all(c("Avg" = mean, "Std" = sd))

#loading the second dataset
rm(list=ls())

initdata = data.frame(read.csv("Mall_Mac.csv", header = TRUE, sep = ";"))

#glimpse into the data
str(initdata)
summary(initdata)
#renaming the columns to something easier
colnames(initdata) = c("ID", "LoyaltyCard", "Age", "Income", "Spending")
#removing the ID columns as we don't need it
initdata = initdata[, -1]
plot(initdata)

#the below is copied from above!

#scaling the data
data = data.frame(apply(initdata, 2, rescale, to = c(0, 1)))
summary(data)

#hierarchical clustering
#getting the distance matrix (only the lower half since it's mirrored)
distmat = dist(data, method = "euclidian")
hmdl = hclust(distmat, method = "complete")

#making the dendrogram
dendhmdl = as.dendrogram(hmdl)

#plotting the dendrogram
par(mfrow = c(1, 1))
plot(dendhmdl, xlab = "observation", ylab = "height of the tree", main = "Dendrogram")

#getting the two-cluster solution
clustermember = factor(cutree(hmdl, k = 2))
plot(initdata, col = clustermember, main = "Two clusters")
grid()

#removing the LoyaltyCard from our data
data = data[, -1]

ggplot(initdata, aes(x = BrandLoyalty, y = PriceSensitivity, col = clustermember)) +
  geom_point() +
  ggtitle("Two-cluster solution")

#getting the three-cluster solution
clustermember = factor(cutree(hmdl, k = 3))

ggplot(initdata, aes(x = BrandLoyalty, y = PriceSensitivity, col = clustermember)) +
  geom_point() +
  ggtitle("Three-cluster solution")

#using kmeans for k = 2
kmeansmdl = kmeans(data, centers = 2, nstart = 25)
kmeansmdl$cluster
kmeansmdl$centers
kmeansmdl$withinss
kmeansmdl$tot.withinss

#plotting the result

ggplot(data, aes(x = BrandLoyalty, y = PriceSensitivity, col = factor(kmeansmdl$cluster))) +
  geom_point() +
  geom_point(aes(x = kmeansmdl$centers[1, 1], y = kmeansmdl$centers[1, 2]), colour = "black", size = 5) +
  geom_point(aes(x = kmeansmdl$centers[2, 1], y = kmeansmdl$centers[2, 2]), colour = "black", size = 5) +
  ggtitle("K-means - two clusters")


#using kmeans for k = 3
kmeansmdl = kmeans(data, centers = 3, nstart = 25)

ggplot(data, aes(x = BrandLoyalty, y = PriceSensitivity, col = factor(kmeansmdl$cluster))) +
  geom_point() +
  geom_point(aes(x = kmeansmdl$centers[1, 1], y = kmeansmdl$centers[1, 2]), colour = "black", size = 5) +
  geom_point(aes(x = kmeansmdl$centers[2, 1], y = kmeansmdl$centers[2, 2]), colour = "black", size = 5) +
  geom_point(aes(x = kmeansmdl$centers[3, 1], y = kmeansmdl$centers[3, 2]), colour = "black", size = 5) +
  ggtitle("K-means - three clusters")

#utilizing the Elbow method to determine the optimal amount of clusters
tot_within_ss = map_dbl(1:10, function(k){
  kmeansmdl = kmeans(data, centers = k, nstart = 25)
  kmeansmdl$tot.withinss
})
#converting the variable to a data frame in order to use ggplot2
tot_within_ss = data.frame(tot_within_ss)
str(tot_within_ss)

ggplot(tot_within_ss, aes(x = 1:10, y = tot_within_ss)) +
  geom_line() +
  ggtitle("Elbow method")

#plotting with the core package
par(mfrow = c(1, 1))
plot(1:10, tot_within_ss$tot_within_ss, type = "o")
grid()

#utilizing the Silhouette method and Calinski-Harabasz, Gap-statistic to determine the optimal amount of clusters
SilClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "silhouette")
GapClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "gap")
CHClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ch")

#plotting all 3 of them at the same time with the base package
par(mfrow = c(1, 3))
plot(2:10, SilClust$All.index, type = "o", col = "forestgreen", xlab = "Number of clusters k", ylab = "Silhouette width")
grid()
plot(2:10, GapClust$All.index, type = "o", col = "forestgreen", xlab = "Number of clusters k", ylab = "Gap statistic")
grid()
plot(2:10, CHClust$All.index, type = "o", col = "forestgreen", xlab = "Number of clusters k", ylab = "Calinski-Harabasz index")
grid()

#doing the clustering based on the results and analyzing the data
kmeansmdl = kmeans(data, centers = 5, nstart = 25)
initdata$cluster = factor(kmeansmdl$cluster)
str(initdata)

initdata %>%
  group_by(cluster) %>%
  summarise_all(c("Avg" = mean, "Std" = sd))

#one last plot for today
p1 = ggplot(initdata, aes(x = cluster, y = Age, fill = cluster)) +
  geom_boxplot() +
  ggtitle("Age by cluster")

p2 = ggplot(initdata, aes(x = cluster, y = Spending, fill = cluster)) +
  geom_boxplot() +
  ggtitle("Spending by cluster")

p1/p2
