rm(list=ls())
install.packages("caTools")
library(caTools)
install.packages("neuralnet")
library(dplyr)
library(neuralnet)

#let's load our first data set
data = read.csv("NN Simple_1_Mac.csv", header = TRUE, sep =";")
str(data)

#data balance
table(data$Class)
prop.table(table(data$Class))

#data split
sample = sample.split(data$Class, SplitRatio = 0.8)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

#let's train our neural network (NN)
nn = neuralnet(Class~Tasks+Salary, data = train, hidden = 0, act.fct = "logistic", linear.output = FALSE)

nn
plot(nn)

dev.off()

#let's make our confusion matrix
tabTrain = table(train$Class, ifelse(nn$net.result[[1]] >= 0.5,1,0))

nn.pred = compute(nn, test)
tabTest = table(test$Class, ifelse(nn.pred$net.result >= 0.5,1,0))

#our own function for accuracy, recall and precision
eval_class = function(tp, tn, fp, fn){
  accuracy = (tp + tn) / (tp + tn + fn + fp)
  recall = tp / (tp + fn)
  precision = tp / (tp + fp)
  res = c(accuracy, recall, precision)
  names(res) = c("Accuracy", "Recall", "Precision")
  return(res)
}

eval_class(tabTrain[2,2], tabTrain[1,1], tabTrain[1,2], tabTrain[2,1])
eval_class(tabTest[2,2], tabTest[1,1], tabTest[1,2], tabTest[2,1])

#let's make a plot of the decision regions and boundary
plot(train[,1], train[,2], cex = 0.8, pch = 16, col = ifelse(train$Class == 1, "blue", "red"),
     main = "Neural Network Decision Regions and Boundary")
points(test[,1], test[,2], pch = 6, col = ifelse(test$Class == 1, "blue", "red"))

#200 equally spaced points in [0, 10]
x1 = seq(0, 10, length.out = 200)
x2 = seq(0, 10, length.out = 200)
#create a grid of all (x1, x2) combinations
Xtest = expand.grid(x1, x2)

nn.pred = compute(nn, Xtest)
points(Xtest[,2], Xtest[,1], col = ifelse(nn.pred$net.result >= 0.5, "blue", "red"), cex = 0.1)

#let's work with our second data set
data = read.csv("NN Simple_2_Mac.csv", header = TRUE, sep = ";")

str(data)
sample = sample.split(data$Class, SplitRatio = 0.8)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

nn = neuralnet(Class~Tasks+Salary, data = train, hidden = 3, act.fct = "logistic", linear.output = TRUE)

nn
plot(nn)

tabTrain = table(train$Class, ifelse(nn$net.result[[1]] >= 0.5,1,0))

nn.pred = compute(nn, test)
tabTest = table(test$Class, ifelse(nn.pred$net.result >= 0.5,1,0))


eval_class(tabTrain[2,2], tabTrain[1,1], tabTrain[1,2], tabTrain[2,1])
eval_class(tabTest[2,2], tabTest[1,1], tabTest[1,2], tabTest[2,1])

#let's make a plot of the decision regions and boundary
plot(train[,1], train[,2], cex = 0.8, pch = 16, col = ifelse(train$Class == 1, "blue", "red"),
     main = "Neural Network Decision Regions and Boundary")
points(test[,1], test[,2], pch = 6, col = ifelse(test$Class == 1, "blue", "red"))

#200 equally spaced points in [0, 10]
x1 = seq(0, 10, length.out = 200)
x2 = seq(0, 10, length.out = 200)
#create a grid of all (x1, x2) combinations
Xtest = expand.grid(x1, x2)

nn.pred = compute(nn, Xtest)
points(Xtest[,1], Xtest[,2], col = ifelse(nn.pred$net.result >= 0.5, "blue", "red"), cex = 0.1)

dev.off()

