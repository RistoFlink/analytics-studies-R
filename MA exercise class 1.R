rm(list=ls())
#install and load needed packages
library(ggplot2)
library(dplyr)
library(corrplot)
install.packages("patchwork")
library(patchwork)

#load the data set
data = read.csv("datanew.csv", header = TRUE, sep = ",")
str(data)

#ensuring there are no missing values
any(is.na(data))
#if true
data = data[complete.cases(data), ] #removes all observations with missing values

#getting to understand the customers
head(data)
data[1:5, 2]
data[1:5, "Income"]
data$Income[1:5]

data[c(2,7), c("Income", "Age")]
colnames(data)

#Marriage and Education were characters -> the below converts them into categorical variables
data$Marriage = factor(data$Marriage)
data$Education = factor(data$Education)
summary(data)
summary(data[,c("Income", "Age", "Marriage", "Education")])

#creating some visualizations
ggplot(data, aes(x = Income)) + 
  geom_histogram(colour = "black", fill = "lightblue", bins = 50) + 
  ggtitle("Income distribution")

ggplot(data, aes(x = Age)) +
  geom_histogram(colour = "black", fill = "goldenrod", bins = 50) +
  ggtitle("Age distribution")

ggplot(data, aes(x = Education)) +
  geom_bar(colour = "black", fill = "darkgreen") +
  ggtitle("Education levels") +
  xlab("")

#plotting income by educational level
ggplot(data, aes(x = Education, y = Income)) +
  geom_boxplot(colour = "black", fill = "lightblue") +
  ggtitle("Income by Educational level") +
  xlab(NULL)

#looking at the products
summary(data[,8:14])

p1 = ggplot(data, aes(x = MntWines)) +
  geom_histogram(colour = "black", fill = "lightblue") +
  ggtitle("Wines")

p2 = ggplot(data, aes(x = MntMeatProducts)) +
  geom_histogram(colour = "black", fill = "lightblue") +
  ggtitle("Meat")

p3 = ggplot(data, aes(x = MntRegularProds)) +
  geom_histogram(colour = "black", fill = "lightblue") +
  ggtitle("Regular products")

p1 + p2 + p3

p1 / p2

(p1 + p2) / p3

#taking a look a the campaign acceptance
summary(data$AcceptedCmpOverall)

ggplot(data, aes(x = factor(AcceptedCmpOverall), y = Income)) +
  geom_boxplot(colour = "black", fill = "lightblue")

ggplot(data, aes(x = factor(AcceptedCmpOverall), y = NumWebPurchases)) +
  geom_boxplot(colour = "black", fill = "lightblue")

#which campaigns were most frequently accepted?
data %>% 
  select(c(2, 21:26)) %>%
  filter(AcceptedCmpOverall > 0) %>%
  summarize_all(mean)

data %>% 
  select(c(2, 21:26)) %>%
  filter(AcceptedCmpOverall > 0) %>%
  group_by(AcceptedCmpOverall) %>%
  summarize_all(mean)

install.packages("tidyr")
install.packages("igraph")
install.packages("igraphdata")

library(tidyr)
library(igraph)
library(igraphdata)

install.packages("vctrs")
