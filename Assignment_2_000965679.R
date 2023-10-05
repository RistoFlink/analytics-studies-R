rm(list=ls()) #clear the environment to start on an empty desk

library(ggplot2) #load ggplo2 for visualisation - what other packages might be needed?
library(corrplot) #load this package to provide better visualisation for correlations
library(tsoutliers) #this package is for the Jarque-Bera test for normal distribution
library(purrr) #this for one part in the clustering
library(scales) #scaling data
library(NbClust) #clustering methods etc
library(dplyr) #for manipulating/transforming data

### PART 1 (REGRESSION ANALYSIS) ###
data = read.csv("dataArrests_Mac.csv", header = TRUE, sep = ";") #did this on a Macbook -- change settings as needed!
str(data) #look at the structure of the data
any(is.na(data)) #quick check to see if there are any NA values in the observations
#since the result was TRUE -> remove observations where any of the variables are NA as was in the instructions
crimeData = data[complete.cases(data), ] #we can see 5 observations were removed
#looking at the correlations between Murders, Assaults, Urban Population, Traffic and Car Accidents
#murder being the dependent variable and the others being the independents
#1. assault vs murder
cor(crimeData$Assault,crimeData$Murder)
ggplot(crimeData, (aes(x = Assault, y = Murder, col = "#fafafa", legend = "None"))) + 
  geom_point(show.legend = FALSE) +
  ggtitle("Assaults vs Murders") +
  theme(plot.title = element_text(hjust = 0.5))

#2. urban population vs murder
cor(crimeData$UrbanPop,crimeData$Murder)
ggplot(crimeData, (aes(x = UrbanPop, y = Murder, col = "#fafafa", legend = "None"))) + 
  geom_point(show.legend = FALSE) +
  ggtitle("Urban population vs Murders") +
  theme(plot.title = element_text(hjust = 0.5))

#3. traffic vs murder
cor(crimeData$Traffic,crimeData$Murder)
ggplot(crimeData, (aes(x = Traffic, y = Murder, col = "#fafafa", legend = "None"))) + 
  geom_point(show.legend = FALSE) +
  ggtitle("Traffic vs Murders") +
  theme(plot.title = element_text(hjust = 0.5))

#4. car accidents vs murder
cor(crimeData$CarAccidents,crimeData$Murder)
ggplot(crimeData, (aes(x = CarAccidents, y = Murder, col = "#fafafa", legend = "None"))) + 
  geom_point(show.legend = FALSE) +
  ggtitle("Car accidents vs Murders") +
  theme(plot.title = element_text(hjust = 0.5))


corrplot(cor(crimeData[, 1:9]), "number") #create the correlation plot for all variables


#creating the linear regression model
linRegModel = lm(Murder~Assault+UrbanPop+Drug+Cyber+Kidnapping+Domestic+Alcohol, data = crimeData)
summary(linRegModel)
#simplifying it step-by-step until all remaining variables are significant
linRegModel = lm(Murder~Assault+UrbanPop+Drug+Cyber+Domestic+Alcohol, data = crimeData)
summary(linRegModel)
linRegModel = lm(Murder~Assault+UrbanPop+Drug+Cyber+Domestic, data = crimeData)
summary(linRegModel)
linRegModel = lm(Murder~Assault+UrbanPop+Drug+Cyber, data = crimeData)
summary(linRegModel)
linRegModel = lm(Murder~Assault+UrbanPop+Cyber, data = crimeData)
summary(linRegModel)
linRegModel = lm(Murder~Assault+UrbanPop, data = crimeData)
summary(linRegModel)
linRegModel = lm(Murder~Assault, data = crimeData)
summary(linRegModel)

ggplot(crimeData, aes(x = Assault, y = Murder)) +
  geom_point() +
  geom_line(aes(x = Assault, y = fitted.values(linRegModel)), col = "red")

#checking if the residuals have zero mean - passes the test
mean(residuals(linRegModel))
     
#check the skedasticity visually
par(mfrow=c(1,1))
plot(residuals(linRegModel), 
     type = "p", col = "#333333", ylim = c(-30,30), pch = 16, 
     ylab = "Residuals", main = "Residuals over time")
abline(a = 3 * sd(residuals(linRegModel)), b = 0, col = "red", lty = 2)
abline(a = -3 * sd(residuals(linRegModel)), b = 0, col = "red", lty = 2)
abline(a = 0, b = 0, col ="black", lty = 2)

#check the correlation between residual and exploratory variables
cor(residuals(linRegModel), crimeData[,2:10])

#Jarque-Bera test for normal distribution
JarqueBera.test(residuals(linRegModel))

#visualizing the normal distribution
hist(x = residuals(linRegModel), main = "Normal distribution of residuals", xlab = NULL)

#finally check how the fitted values fit in with the residuals visually
plot(fitted.values(linRegModel), residuals(linRegModel), 
     type = "p", col = "blue", ylim = c(-20,20), pch = 16, 
     xlab = "Fitted", ylab = "Residuals", main = "Fitted values vs Residuals")
abline(a = 3 * sd(residuals(linRegModel)), b = 0, col = "red", lty = 2)
abline(a = -3 * sd(residuals(linRegModel)), b = 0, col = "red", lty = 2)
abline(a = 0, b = 0, col ="black", lty = 2)

### PART 2 (clustering)###
#load the data set
shopData = read.csv("wholesale_Mac.csv", header = TRUE, sep = ";")
str(shopData)
any(is.na(shopData)) #there appears to be no missing values

#exploratory data analysis
#first 2 appear to be categorical in numerical form - maybe bar chart to display how many belong to each category?
par(mfrow = c(1,2))
barplot(table(shopData$Channel), main = "Channel")
barplot(table(shopData$Region), main = "Region")
#histograms for the others, show all 6 at once for hopefully higher glance value - set the x and y limits on each as the same for better readability (at least to me)
par(mfrow = c(2, 3))
hist(x = shopData$Fresh, xlab = "Annual spending", main = "Fresh", xlim = c(0,125000), ylim = c(0,450), col = "lightblue")
hist(x = shopData$Milk, xlab = "Annual spending", main = "Milk", xlim = c(0,125000), ylim = c(0,450), col = "lightblue")
hist(x = shopData$Grocery, xlab = "Annual spending", main = "Grocery", xlim = c(0,125000), ylim = c(0,450), col = "lightblue")
hist(x = shopData$Frozen, xlab = "Annual spending", main = "Frozen", xlim = c(0,125000), ylim = c(0,450), col = "lightblue")
hist(x = shopData$Detergents_Paper, xlab = "Annual spending", main = "Detergents_Paper", xlim = c(0,125000), ylim = c(0,450), col = "lightblue")
hist(x = shopData$Delicassen, xlab = "Annual spending", main = "Delicassen", xlim = c(0,125000), ylim = c(0,450), col = "lightblue")
#correlation analysis
corrplot(cor(shopData[, 1:8]), "number")

#scaling the data in each column separately - min-max normalization
scaledData = apply(shopData, 2, rescale, to = c(0, 1)) #this appears to not create a data frame so..
str(scaledData)
scaledData = data.frame(scaledData) #transform the scaled data back into a data frame


# k-means clustering
kmeansmdl = kmeans(scaledData, centers = 5, nstart = 25)

tot_within_ss = map_dbl(1:25, function(k){
  model = kmeans(scaledData, centers = k, nstart = 25)
  model$tot.withinss
})
#creating the different methods to search for the optimal number of clusters
par(mfrow = c(1,1))
plot(1:25, tot_within_ss, type = "o", xlab = "Number of Clusters", ylab = "Total WSS",
     main = "Elbow Method", panel.first = grid())

SilClust = NbClust(scaledData, distance = "euclidean", min.nc = 2, max.nc = 25, method = "kmeans", index = "silhouette")

GapClust = NbClust(scaledData, distance = "euclidean", min.nc = 2, max.nc = 25, method = "kmeans", index = "gap")

CHClust = NbClust(scaledData, distance = "euclidean", min.nc = 2, max.nc = 25, method = "kmeans", index = "ch")

#let's visualize these methods
par(mfrow = c(1, 3))
plot(2:25, SilClust$All.index, type = "o", xlab = "Number of Clusters", ylab = "Silhouette Score",
     panel.first = grid())
plot(2:25, GapClust$All.index, type = "o", xlab = "Number of Clusters", ylab = "Gap Statistic",
     panel.first = grid())
plot(2:25, CHClust$All.index, type = "o", xlab = "Number of Clusters", ylab = "Calinski Harabasz Index",
     panel.first = grid())

#run the k-means algorithm on the original data
kmeansmdlOriginalData = kmeans(shopData, centers = 9, nstart = 25)


#create a new data frame that has a cluster membership as a variable
dataNew = shopData %>% mutate(member = factor(kmeansmdlOriginalData$cluster))
#use piping operators to group up the observations based on their cluster membership and display average spends in each variable per cluster
dataNew %>% 
  group_by(member) %>%
  summarise_all(list(avg = mean))
#same as above but with the standard deviation
dataNew %>%
  group_by(member) %>%
  summarise_all(list(Std = sd))
#plot 2 variables against each other and colour them based on cluster membership
par(mfrow = c(1,1))
ggplot(dataNew, aes(x = Fresh, y = Grocery, col = member)) +
  geom_point() +
  ggtitle("Clusters in the data set")

#another visualisation
ggplot(dataNew, aes(x = Detergents_Paper, y = Delicassen, col = member)) +
  geom_point() +
  ggtitle("Clusters in the data set")
#boxplots  - include or not in the report? could be a nice addition
ggplot(dataNew, aes(x = member, y = Fresh, fill = member)) +
  geom_boxplot() +
  ggtitle("Distribution of Fresh by Cluster") +
  xlab("Cluster") +
  ylab("Fresh value") +
  theme(plot.title = element_text(hjust = 0.5))


