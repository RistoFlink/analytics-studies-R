rm(list=ls())
#remove old plots dev.off()
library(ggplot2)
library(tidyr)
library(igraph)
library(igraphdata)

#only for mac
library(tcltk)

#first example from the lecture
matrix_int = matrix(
  c(1, 1, 0,
    1, 0, 1,
    0, 1, 0),
  nrow=3, ncol = 3, byrow = TRUE)

#constructing our graph
graph_int = graph.adjacency(matrix_int, mode = "directed")
V(graph_int)$label = c("Y", "A", "M")
tkplot(graph_int, vertex.color = "green")

dIn = degree(graph_int, mode = "in")
dOut = degree(graph_int, mode = "out")

stackeddata = rbind(dIn, dOut)
barplot(stackeddata, xlab = "Node", ylab = "Node Degree", names.arg = V(graph_int)$label, 
        beside = TRUE, legend=c("In-degree", "Out-degree"))

#setting up the PageRank
Mmat = t(matrix_int) / matrix(dOut, nrow = 3, ncol = 3, byrow = TRUE)

r = matrix(c(1/3, 1/3, 1/3), nrow = 3, ncol = 1)

rall  = r #save all r values over time
rchange = 1 #initial value for the change
n = 1
#run loop while differences between old and new r values is larger than 1%
while(rchange > 0.01){
  #new probablities
  r = Mmat %*% r #%*% is matrix multiplication
  #saving the new r
  rall = cbind(rall, r)
  #update differences between the old and the new r values
  rchange = max(abs(rall[, n] - r))
  #update the loop counter
  n = n + 1
}

#getting the data into the right shape for plotting
rall = data.frame(rall)
colnames(rall) = 1:dim(rall)[2]
rall$Webpage = c("Y", "A", "M")

rall = pivot_longer(rall, cols = 1:n, names_to = "Iteration", values_to = "Prob")
rall$Iteration = factor(rall$Iteration, levels = 1:n)

ggplot(rall, aes(x = Iteration, y = Prob, fill = Webpage)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Example 1: Simple Network")
  
#second example: Dead End
matrix_int = matrix(
  c(1, 1, 0,
    1, 0, 1,
    0, 0, 0),
  nrow=3, ncol = 3, byrow = TRUE)

#constructing our graph
graph_int = graph.adjacency(matrix_int, mode = "directed")
V(graph_int)$label = c("Y", "A", "M")
tkplot(graph_int, vertex.color = "green")

dIn = degree(graph_int, mode = "in")
dOut = degree(graph_int, mode = "out")

stackeddata = rbind(dIn, dOut)
barplot(stackeddata, xlab = "Node", ylab = "Node Degree", names.arg = V(graph_int)$label, 
        beside = TRUE, legend=c("In-degree", "Out-degree"))

#setting up the PageRank
Mmat = t(matrix_int) / matrix(dOut, nrow = 3, ncol = 3, byrow = TRUE)
Mmat[matrix(dOut, nrow = 3, ncol = 3, byrow = TRUE) == 0] = 0

r = matrix(c(1/3, 1/3, 1/3), nrow = 3, ncol = 1)

rall  = r #save all r values over time
rchange = 1 #initial value for the change
n = 1

#run loop while differences between old and new r values is larger than 1%
while(rchange > 0.001){
  #new probablities
  r = Mmat %*% r #%*% is matrix multiplication
  #saving the new r
  rall = cbind(rall, r)
  #update differences between the old and the new r values
  rchange = max(abs(rall[, n] - r))
  #update the loop counter
  n = n + 1
}

#getting the data into the right shape for plotting
rall = data.frame(rall)
colnames(rall) = 1:dim(rall)[2]
rall$Webpage = c("Y", "A", "M")

rall = pivot_longer(rall, cols = 1:n, names_to = "Iteration", values_to = "Prob")
rall$Iteration = factor(rall$Iteration, levels = 1:n)

ggplot(rall, aes(x = Iteration, y = Prob, fill = Webpage)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Example 2: Dead End")

#third example: Spider Trap
matrix_int = matrix(
  c(1, 1, 0,
    1, 0, 1,
    0, 0, 1),
  nrow=3, ncol = 3, byrow = TRUE)

#constructing our graph
graph_int = graph.adjacency(matrix_int, mode = "directed")
V(graph_int)$label = c("Y", "A", "M")
#tkplot(graph_int, vertex.color = "green")

dIn = degree(graph_int, mode = "in")
dOut = degree(graph_int, mode = "out")

stackeddata = rbind(dIn, dOut)
barplot(stackeddata, xlab = "Node", ylab = "Node Degree", names.arg = V(graph_int)$label, 
        beside = TRUE, legend=c("In-degree", "Out-degree"))

#setting up the PageRank
Mmat = t(matrix_int) / matrix(dOut, nrow = 3, ncol = 3, byrow = TRUE)
Mmat[matrix(dOut, nrow = 3, ncol = 3, byrow = TRUE) == 0] = 0

r = matrix(c(1/3, 1/3, 1/3), nrow = 3, ncol = 1)

rall  = r #save all r values over time
rchange = 1 #initial value for the change
n = 1

#run loop while differences between old and new r values is larger than 1%
while(rchange > 0.001){
  #new probablities
  r = Mmat %*% r #%*% is matrix multiplication
  #saving the new r
  rall = cbind(rall, r)
  #update differences between the old and the new r values
  rchange = max(abs(rall[, n] - r))
  #update the loop counter
  n = n + 1
}

#getting the data into the right shape for plotting
rall = data.frame(rall)
colnames(rall) = 1:dim(rall)[2]
rall$Webpage = c("Y", "A", "M")

rall = pivot_longer(rall, cols = 1:n, names_to = "Iteration", values_to = "Prob")
rall$Iteration = factor(rall$Iteration, levels = 1:n)

ggplot(rall, aes(x = Iteration, y = Prob, fill = Webpage)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Example 3: Spider Trap")


#using teleportation
beta = 0.8
Amat = beta * Mmat + (1 - beta) * matrix(1/3, nrow = 3, ncol = 3)

r = matrix(c(1/3, 1/3, 1/3), nrow = 3, ncol = 1)

rall  = r #save all r values over time
rchange = 1 #initial value for the change
n = 1

#run loop while differences between old and new r values is larger than 1%
while(rchange > 0.001){
  #new probablities
  r = Amat %*% r #%*% is matrix multiplication
  #saving the new r
  rall = cbind(rall, r)
  #update differences between the old and the new r values
  rchange = max(abs(rall[, n] - r))
  #update the loop counter
  n = n + 1
}

#getting the data into the right shape for plotting
rall = data.frame(rall)
colnames(rall) = 1:dim(rall)[2]
rall$Webpage = c("Y", "A", "M")

rall = pivot_longer(rall, cols = 1:n, names_to = "Iteration", values_to = "Prob")
rall$Iteration = factor(rall$Iteration, levels = 1:n)

ggplot(rall, aes(x = Iteration, y = Prob, fill = Webpage)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Example 4: Teleportation with beta of 0.8")

#Example 5: topic-specific PageRank
teleportSet = c(1, 1, 0)
Amat = beta * Mmat + (1 - beta) * matrix(teleportSet / sum(teleportSet), ncol = 3, nrow = 3, byrow = FALSE)


r = matrix(c(1/3, 1/3, 1/3), nrow = 3, ncol = 1)

rall  = r #save all r values over time
rchange = 1 #initial value for the change
n = 1

#run loop while differences between old and new r values is larger than 1%
while(rchange > 0.001){
  #new probablities
  r = Amat %*% r #%*% is matrix multiplication
  #saving the new r
  rall = cbind(rall, r)
  #update differences between the old and the new r values
  rchange = max(abs(rall[, n] - r))
  #update the loop counter
  n = n + 1
}

#getting the data into the right shape for plotting
rall = data.frame(rall)
colnames(rall) = 1:dim(rall)[2]
rall$Webpage = c("Y", "A", "M")

rall = pivot_longer(rall, cols = 1:n, names_to = "Iteration", values_to = "Prob")
rall$Iteration = factor(rall$Iteration, levels = 1:n)

ggplot(rall, aes(x = Iteration, y = Prob, fill = Webpage)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Example 5: Topic-sensitive PageRank")

