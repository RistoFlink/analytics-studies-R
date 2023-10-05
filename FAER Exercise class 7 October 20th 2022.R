rm(list=ls())

library(scales)

install.packages("NbClust")
library(NbClust)

install.packages("purrr")
library(purrr)

library(ggplot2)
library(dplyr)

#let's load our first data set
initdata = read.csv("artificialClusters_Mac.csv", header = TRUE, sep = ";")
str(initdata)

#checking for missing values
any(is.na(initdata))

#recommended for clustering: scaling the data
data = apply(initdata, 2, rescale, to = c(0,1)) #min max normalization

#if you want to do z-scores
#data = apply(initdata, 2, scale, center = TRUE, scale = TRUE)

ggplot(data.frame(data), aes(x = Variable1, y = Variable2)) +
  geom_point() +
  ggtitle("First Clustering Example")

#let's do our first hierarchical clustering
distmat = dist(data, method = "euclidian")
model = hclust(distmat, method = "complete")

#let's make a dendrogram for our hierarchical clustering
dendmodel = as.dendrogram(model)
plot(dendmodel, main = "First Dendrogram", panel.first = grid())

#cluster memberships = which observation belongs to which cluster
clustermember = cutree(model, k = 3)

#let's visualize the result
plot(data[,1], data[,2], col = clustermember, pch = 16, panel.first = grid())

#let's try k-means clustering
kmeansmdl = kmeans(data, centers = 6, nstart = 25)

kmeansmdl$cluster
kmeansmdl$centers
plot(data[,1], data[,2], col = kmeansmdl$cluster, pch = 16, main = "Our K-means Clustering Result", panel.first = grid())

kmeansmdl$size
kmeansmdl$withinss
kmeansmdl$tot.withinss

#let's determine the optimal number of clusters
#first method: the elbow method
tot_within_ss = map_dbl(1:10, function(k){
  model = kmeans(data, centers = k, nstart = 25)
  model$tot.withinss
})

plot(1:10, tot_within_ss, type = "o", xlab = "Number of Clusters", ylab = "Total WSS",
     main = "Elbow Method", panel.first = grid())

#other methods to determine the optimal number of clusters
SilClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "silhouette")

GapClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "gap")

CHClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ch")

#let's visualize these methods
par(mfrow = c(1, 3))
plot(2:10, SilClust$All.index, type = "o", xlab = "Number of Clusters", ylab = "Silhouette Score",
     panel.first = grid())
plot(2:10, GapClust$All.index, type = "o", xlab = "Number of Clusters", ylab = "Gap Statistic",
     panel.first = grid())
plot(2:10, CHClust$All.index, type = "o", xlab = "Number of Clusters", ylab = "Calinski Harabasz Index",
     panel.first = grid())

#let's use the optimal number of clusters
par(mfrow = c(1,1))
kmeansmdl = kmeans(data, centers = 3, nstart = 25)

#let's load our second & more complex data set
rm(list = ls())
data = read.csv("simpleClusters_Mac.csv", header = TRUE, sep = ";")
str(data)


tot_within_ss = map_dbl(1:10, function(k){
  model = kmeans(data, centers = k, nstart = 25)
  model$tot.withinss
})

plot(1:10, tot_within_ss, type = "o", xlab = "Number of Clusters", ylab = "Total WSS",
     main = "Elbow Method", panel.first = grid())

#other methods to determine the optimal number of clusters
SilClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "silhouette")

GapClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "gap")

CHClust = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ch")

#let's visualize these methods
par(mfrow = c(1, 3))
plot(2:10, SilClust$All.index, type = "o", xlab = "Number of Clusters", ylab = "Silhouette Score",
     panel.first = grid())
plot(2:10, GapClust$All.index, type = "o", xlab = "Number of Clusters", ylab = "Gap Statistic",
     panel.first = grid())
plot(2:10, CHClust$All.index, type = "o", xlab = "Number of Clusters", ylab = "Calinski Harabasz Index",
     panel.first = grid())

#let's cluster with the optimal number
kmeansmdl = kmeans(data, centers = 3, nstart = 25)
par(mfrow = c(1, 1))

#let's add cluster memberships to the data
dataNew = data %>% mutate(member = factor(kmeansmdl$cluster))

dataNew %>% 
  group_by(member) %>%
  summarise_all(list(avg = mean))

dataNew %>% 
  group_by(member) %>%
  summarise_all(list(avg = mean, Std = sd))

#let's investigate this visually
par(mfrow = c(1,1))
ggplot(dataNew, aes(x = V1, y = V2, col = member)) +
  geom_point() +
  ggtitle("Clusters in the data set")

#investigate variable V1
ggplot(dataNew, aes(x = member, y = V1, fill = member)) +
  geom_boxplot() +
  ggtitle("Distribution of V1 by Cluster") +
  xlab("Cluster") +
  ylab("V1 value")

dist()





